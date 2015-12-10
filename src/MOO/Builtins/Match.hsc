
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls,
             GeneralizedNewtypeDeriving #-}
{-# CFILES src/cbits/match.c #-}

-- | Regular expression matching via PCRE through the FFI
module MOO.Builtins.Match (
    Regexp
  , MatchResult(..)

  -- ** Compiling
  , newRegexp

  -- ** Matching
  , match
  , rmatch

  -- ** Miscellaneous
  , pcreVersion
  , verifyPCRE
  ) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad (unless)
import Data.Bits (Bits(zeroBits, (.|.), (.&.), complement))
import Data.ByteString (ByteString, useAsCString, useAsCStringLen)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mempty, mappend), (<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Foreign (Ptr, FunPtr, ForeignPtr, toBool,
                Storable(peek, peekByteOff, pokeByteOff),
                nullPtr, alloca, allocaArray, peekArray,
                newForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.C (CString, CInt(CInt), CULong, peekCString)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as BS
import qualified Data.Text as T

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}
{-# ANN module ("HLint: ignore Redundant lambda" :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket" :: String) #-}

# include <pcre.h>

foreign import ccall safe "static pcre.h"
  pcre_compile :: CString -> PCREOptions -> Ptr CString -> Ptr CInt ->
                  Ptr CharacterTables -> IO (Ptr PCRE)

foreign import ccall safe "static pcre.h"
  pcre_study :: Ptr PCRE -> PCREStudyOptions -> Ptr CString ->
                IO (Ptr PCREExtra)

foreign import ccall unsafe "static pcre.h &"
  pcre_free_study :: FunPtr (Ptr PCREExtra -> IO ())

foreign import ccall unsafe "static pcre.h &"
  pcre_free :: Ptr (FunPtr (Ptr a -> IO ()))

foreign import ccall unsafe "static pcre.h"
  pcre_version :: IO CString

foreign import ccall unsafe "static pcre.h"
  pcre_config :: PCREConfig a -> Ptr a -> IO CInt

foreign import ccall safe "static match.h"  match_helper :: Helper
foreign import ccall safe "static match.h" rmatch_helper :: Helper

type Helper = Ptr PCRE -> Ptr PCREExtra -> CString -> CInt ->
              PCREOptions -> Ptr CInt -> IO CInt

data PCRE
data PCREExtra
data CharacterTables

newtype BitFlags a = Flags a deriving (Eq, Bits, Storable)

instance Bits a => Monoid (BitFlags a) where
  mempty  = zeroBits
  mappend = (.|.)

andNot :: Bits a => a -> a -> a
x `andNot` y = x .&. complement y

newtype PCREOptions = Options (BitFlags CInt) deriving Monoid

# enum PCREOptions, (Options . Flags)  \
  , PCRE_UTF8                          \
  , PCRE_NO_UTF8_CHECK                 \
  , PCRE_DOLLAR_ENDONLY                \
  , PCRE_CASELESS

newtype PCREStudyOptions = StudyOptions (BitFlags CInt) deriving Monoid

# enum PCREStudyOptions, (StudyOptions . Flags)  \
  , PCRE_STUDY_JIT_COMPILE

newtype PCREExtraFlags = ExtraFlags (BitFlags CULong)
                       deriving (Monoid, Eq, Bits, Storable)

# enum PCREExtraFlags, (ExtraFlags . Flags)  \
  , PCRE_EXTRA_MATCH_LIMIT                   \
  , PCRE_EXTRA_MATCH_LIMIT_RECURSION         \
  , PCRE_EXTRA_CALLOUT_DATA

peekExtraFlags :: Ptr PCREExtra -> IO PCREExtraFlags
peekExtraFlags = #{peek pcre_extra, flags}

pokeExtraFlags :: Ptr PCREExtra -> PCREExtraFlags -> IO ()
pokeExtraFlags = #{poke pcre_extra, flags}

patchExtraFlags :: Ptr PCREExtra -> (PCREExtraFlags -> PCREExtraFlags) -> IO ()
patchExtraFlags ptr f = peekExtraFlags ptr >>= pokeExtraFlags ptr . f

data Regexp = Regexp {
    pattern     :: Text
  , caseMatters :: Bool

  , code        :: ForeignPtr PCRE
  , extra       :: ForeignPtr PCREExtra
  } deriving Show

instance Eq Regexp where
  (==) = (==) `on` (caseMatters &&& pattern)

data RewriteState = StateBase
                  | StateEsc
                  | StateCsetInit
                  | StateCsetInit2
                  | StateCset

{-# ANN translate ("HLint: ignore Use list literal" :: String) #-}

-- | Translate MOO regular expression syntax into PCRE syntax.
--
-- Aside from changing % to \ and sundry tweaks we also address an
-- incompatibility between MOO %b, %B, %w, %W and PCRE \b, \B, \w, \W --
-- namely, the inclusion of _ in \w and its absence in %w.
translate :: Text -> Text
translate = T.pack . concat . rewrite . T.unpack
  where
    rewrite :: String -> [String]
    -- wrap entire expression so we can add a callout at the end
    rewrite s = "(?:" : rewrite' StateBase s

    rewrite' :: RewriteState -> String -> [String]
    rewrite' StateBase ('%':cs)     =              rewrite' StateEsc       cs
    rewrite' StateBase ('[':cs)     = "["        : rewrite' StateCsetInit  cs
    rewrite' StateBase ( c :cs)
      | c `elem` "\\|(){"           = "\\" : [c] : rewrite' StateBase      cs
      | otherwise                   =        [c] : rewrite' StateBase      cs

    rewrite' StateCsetInit ('^':cs) = "^"        : rewrite' StateCsetInit2 cs
    rewrite' StateCsetInit ( c :cs)
      | c `elem` "\\["              = "\\" : [c] : rewrite' StateCset      cs
      | otherwise                   =        [c] : rewrite' StateCset      cs

    rewrite' StateCsetInit2 (c:cs)
      | c `elem` "\\["              = "\\" : [c] : rewrite' StateCset      cs
      | otherwise                   =        [c] : rewrite' StateCset      cs

    rewrite' StateCset (']':cs)     = "]"        : rewrite' StateBase      cs
    rewrite' StateCset ( c :cs)
      | c `elem` "\\["              = "\\" : [c] : rewrite' StateCset      cs
      | otherwise                   =        [c] : rewrite' StateCset      cs

    -- insert a null-op (comment) to prevent special sequences
    rewrite' StateEsc ('(':cs)      = "((?#)"    : rewrite' StateBase      cs
    rewrite' StateEsc ('b':cs)      = alt wordBegin wordEnd
                                                 : rewrite' StateBase      cs
    rewrite' StateEsc ('B':cs)      = alt (lookba    word    word)
                                          (lookba nonword nonword)
                                                 : rewrite' StateBase      cs
    rewrite' StateEsc ('<':cs)      = wordBegin  : rewrite' StateBase      cs
    rewrite' StateEsc ('>':cs)      = wordEnd    : rewrite' StateBase      cs
    rewrite' StateEsc ('w':cs)      = word       : rewrite' StateBase      cs
    rewrite' StateEsc ('W':cs)      = nonword    : rewrite' StateBase      cs
    rewrite' StateEsc ( c :cs)
      | c `elem` ['1'..'9']         = "\\" : [c] : "(?#)"
                                                 : rewrite' StateBase      cs
      | c `elem` "\\^$.[?*+{"       = "\\" : [c] : rewrite' StateBase      cs
      | otherwise                   =        [c] : rewrite' StateBase      cs

    -- add callout at end of pattern for rmatch
    rewrite' state []               = ")(?C)"    : rewriteFinal state

    rewriteFinal :: RewriteState -> [String]
    -- don't let a trailing % get away without a syntax error
    rewriteFinal StateEsc           = "\\"       : []
    rewriteFinal _                  =              []

    word, nonword :: String
    word       = "[^\\W_]"
    nonword    =  "[\\W_]"

    alt :: String -> String -> String
    alt a b    = "(?:" ++ a ++ "|" ++ b ++ ")"

    lbehind, lahead :: String -> String
    lbehind p  = "(?<=" ++ p ++ ")"
    lahead  p  = "(?="  ++ p ++ ")"

    lookba :: String -> String -> String
    lookba b a = lbehind b ++ lahead a

    wordBegin, wordEnd :: String
    wordBegin  = alt "^" (lbehind nonword) ++ lahead word
    wordEnd    = lbehind word ++ alt "$" (lahead nonword)

-- | Compile a regular expression pattern into a 'Regexp' value, or return an
-- error description if the pattern is malformed.
newRegexp :: Text -- ^ pattern
          -> Bool -- ^ case matters?
          -> Either String Regexp
newRegexp regexp caseMatters =
  unsafePerformIO     $
  useAsCString string $ \pattern        ->
  alloca              $ \errorPtr       ->
  alloca              $ \errorOffsetPtr -> do

    code <- pcre_compile pattern compileOptions errorPtr errorOffsetPtr nullPtr
    if code == nullPtr
      then Left . patchError <$> (peekCString =<< peek errorPtr)
      else do
        extraFP <- mkExtra code
        setExtraFlags extraFP
        codeFP <- flip newForeignPtr code =<< peek pcre_free
        return $ Right Regexp { pattern     = regexp
                              , caseMatters = caseMatters
                              , code        = codeFP
                              , extra       = extraFP
                              }

  where string = encodeUtf8 (translate regexp) :: ByteString

        compileOptions :: PCREOptions
        compileOptions = pcreUtf8 <> pcreNoUtf8Check <> pcreDollarEndonly <>
                         if caseMatters then mempty else pcreCaseless

        mkExtra :: Ptr PCRE -> IO (ForeignPtr PCREExtra)
        mkExtra code = alloca $ \errorPtr -> do
          extra <- pcre_study code studyOptions errorPtr
          if extra == nullPtr
            then do
              extraFP <- mallocForeignPtrBytes #{size pcre_extra}
              withForeignPtr extraFP $ \extra -> pokeExtraFlags extra mempty
              return extraFP
            else newForeignPtr pcre_free_study extra

          where studyOptions = pcreStudyJitCompile :: PCREStudyOptions

        setExtraFlags :: ForeignPtr PCREExtra -> IO ()
        setExtraFlags extraFP = withForeignPtr extraFP $ \extra -> do
          #{poke pcre_extra, match_limit}           extra matchLimit
          #{poke pcre_extra, match_limit_recursion} extra matchLimitRecursion

          patchExtraFlags extra $ \flags -> (flags <> matchLimitFlags)
                                            `andNot` pcreExtraCalloutData

          where matchLimit, matchLimitRecursion :: CULong
                matchLimit          = 100000
                matchLimitRecursion = matchLimit

                matchLimitFlags :: PCREExtraFlags
                matchLimitFlags = pcreExtraMatchLimit <>
                                  pcreExtraMatchLimitRecursion

        patchError :: String -> String
        patchError = concatMap patch
          where patch '\\' = "%"
                patch '('  = "%("
                patch ')'  = "%)"
                patch  c   = [c]

maxCaptures :: Num a => a
maxCaptures = 10

data MatchResult = MatchFailed
                 | MatchAborted
                 | MatchSucceeded [(Int, Int)]
                 deriving Show

-- | Perform regular expression matching.
match :: Regexp -> Text -> MatchResult
match regexp text = unsafePerformIO $ doMatch match_helper regexp text

-- | Perform regular expression matching, returning the rightmost match.
rmatch :: Regexp -> Text -> MatchResult
rmatch regexp text = unsafePerformIO $ doMatch rmatch_helper regexp text

doMatch :: Helper -> Regexp -> Text -> IO MatchResult
doMatch helper Regexp { code = codeFP, extra = extraFP } text =
  withForeignPtr codeFP  $ \code           ->
  withForeignPtr extraFP $ \extra          ->
  useAsCStringLen string $ \(cstring, len) ->
  allocaArray ovecLen    $ \ovec           ->

  helper code extra cstring (fromIntegral len) options ovec >>=
  matchResult string (T.length text) ovec

  where string  = encodeUtf8 text :: ByteString
        ovecLen = maxCaptures * 3 :: Int
        options = pcreNoUtf8Check :: PCREOptions

matchResult :: ByteString -> Int -> Ptr CInt -> CInt -> IO MatchResult
matchResult subject subjectCharLen ovec rc
  | rc == #{const PCRE_ERROR_NOMATCH} = return MatchFailed
  | rc < 0                            = return MatchAborted
  | otherwise = MatchSucceeded . pairs . map (rebase . fromIntegral) <$>
                peekArray (n * 2) ovec

  where n :: Int
        n | rc == 0 || rc > maxCaptures = maxCaptures
          | otherwise                   = fromIntegral rc

        pairs :: [a] -> [(a, a)]
        pairs (s:e:rs) = (s, e) : pairs rs
        pairs []       = []

        rebase :: Int -> Int
        -- translate UTF-8 byte offset to character offset
        rebase 0 = 0
        rebase i = subjectCharLen - T.length (decodeUtf8 $ BS.drop i subject)

-- | Return the current version of the linked PCRE library.
pcreVersion :: String
pcreVersion = "PCRE " ++ unsafePerformIO (peekCString =<< pcre_version)

newtype PCREConfig a = Config CInt

# enum PCREConfig CInt, Config      \
  , PCRE_CONFIG_UTF8                \
  , PCRE_CONFIG_UNICODE_PROPERTIES  \
  , PCRE_CONFIG_JIT
# enum PCREConfig CString, Config   \
  , PCRE_CONFIG_JITTARGET

-- | Retrieve a PCRE build-time option.
pcreConfig :: Storable a => PCREConfig a -> IO (Maybe a)
pcreConfig what = alloca $ \ptr -> do
  result <- pcre_config what ptr
  case result of
    0 -> Just <$> peek ptr
    _ -> return Nothing

pcreBoolConfig :: PCREConfig CInt -> IO (Maybe Bool)
pcreBoolConfig what = fmap toBool <$> pcreConfig what

pcreStringConfig :: PCREConfig CString -> IO (Maybe String)
pcreStringConfig what = pcreConfig what >>=
                        maybe (return Nothing) (fmap Just . peekCString)

-- | Verify the bound PCRE library was built with the required features.
verifyPCRE :: IO ()
verifyPCRE = do
  verify pcreConfigUtf8 "PCRE is missing UTF-8 support"
{-
  verify pcreConfigUnicodeProperties
    "PCRE is missing Unicode character properties support"
-}
  where verify :: PCREConfig CInt -> String -> IO ()
        verify config msg = do
          supported <- fromMaybe False <$> pcreBoolConfig config
          unless supported $ error msg

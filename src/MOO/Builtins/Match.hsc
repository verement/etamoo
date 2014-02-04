
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
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
  ) where

import Foreign (Ptr, FunPtr, ForeignPtr, alloca, allocaArray, nullPtr,
                peek, peekArray, peekByteOff, pokeByteOff,
                newForeignPtr, mallocForeignPtrBytes, withForeignPtr, (.|.))
import Foreign.C (CString, CInt(..), CULong, peekCString)
import Control.Monad (liftM)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString (ByteString, useAsCString, useAsCStringLen)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text as T
import qualified Data.ByteString as BS

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

#include <pcre.h>

data PCRE
data PCREExtra
data CharacterTables

foreign import ccall unsafe "static pcre.h"
  pcre_compile :: CString -> CInt -> Ptr CString -> Ptr CInt ->
                  Ptr CharacterTables -> IO (Ptr PCRE)

foreign import ccall unsafe "static pcre.h"
  pcre_study :: Ptr PCRE -> CInt -> Ptr CString -> IO (Ptr PCREExtra)

foreign import ccall unsafe "static pcre.h &"
  pcre_free_study :: FunPtr (Ptr PCREExtra -> IO ())

foreign import ccall unsafe "static pcre.h &"
  pcre_free :: Ptr (FunPtr (Ptr a -> IO ()))

-- This must be /unsafe/ to block other threads while it executes, since it
-- relies on being able to modify and use some PCRE global state.
foreign import ccall unsafe "static match.h"
  match_helper :: Ptr PCRE -> Ptr PCREExtra -> CString -> CInt ->
                  CInt -> Ptr CInt -> IO CInt

-- This must be /unsafe/ to block other threads while it executes, since it
-- relies on being able to modify and use some PCRE global state.
foreign import ccall unsafe "static match.h"
  rmatch_helper :: Ptr PCRE -> Ptr PCREExtra -> CString -> CInt ->
                   CInt -> Ptr CInt -> IO CInt

data Regexp = Regexp {
    pattern     :: Text
  , caseMatters :: Bool

  , code        :: ForeignPtr PCRE
  , extra       :: ForeignPtr PCREExtra
  } deriving Show

instance Eq Regexp where
  Regexp { pattern = p1, caseMatters = cm1 } ==
    Regexp { pattern = p2, caseMatters = cm2 } =
      cm1 == cm2 && p1 == p2

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
    -- wrap entire expression so we can add a callout at the end
    rewrite s = "(?:" : rewrite' StateBase s

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

    -- don't let a trailing % get away without a syntax error
    rewriteFinal StateEsc           = "\\"       : []
    rewriteFinal _                  =              []

    word       = "[^\\W_]"
    nonword    =  "[\\W_]"
    alt a b    = "(?:" ++ a ++ "|" ++ b ++ ")"
    lbehind p  = "(?<=" ++ p ++ ")"
    lahead  p  = "(?="  ++ p ++ ")"
    lookba b a = lbehind b ++ lahead a
    wordBegin  = alt "^" (lbehind nonword) ++ lahead word
    wordEnd    = lbehind word ++ alt "$" (lahead nonword)

-- | @newRegexp@ /regexp/ /case-matters/ compiles a regular expression pattern
-- /regexp/ into a 'Regexp' value, or returns an error description if the
-- pattern is malformed. The returned 'CInt' is a byte offset into an
-- internally translated pattern, and thus is probably not very useful.
newRegexp :: Text -> Bool -> IO (Either (String, CInt) Regexp)
newRegexp regexp caseMatters =
  useAsCString (encodeUtf8 $ translate regexp) $ \pattern ->
    alloca $ \errorPtr ->
    alloca $ \errorOffsetPtr -> do
      code <- pcre_compile pattern options errorPtr errorOffsetPtr nullPtr
      if code == nullPtr
        then do error <- peek errorPtr >>= peekCString
                errorOffset <- peek errorOffsetPtr
                return $ Left (patchError error, errorOffset)
        else do extraFP <- mkExtra code
                setExtraFlags extraFP
                codeFP <- peek pcre_free >>= flip newForeignPtr code
                return $ Right Regexp { pattern     = regexp
                                      , caseMatters = caseMatters
                                      , code        = codeFP
                                      , extra       = extraFP
                                      }
  where
    mkExtra code = alloca $ \errorPtr -> do
      extra <- pcre_study code 0 errorPtr
      if extra == nullPtr
        then do extraFP <- mallocForeignPtrBytes #{const sizeof(pcre_extra)}
                withForeignPtr extraFP $ \extra ->
                  #{poke pcre_extra, flags} extra (0 :: CULong)
                return extraFP
        else newForeignPtr pcre_free_study extra

    setExtraFlags extraFP = withForeignPtr extraFP $ \extra -> do
      #{poke pcre_extra, match_limit}           extra matchLimit
      #{poke pcre_extra, match_limit_recursion} extra matchLimitRecursion
      flags <- #{peek pcre_extra, flags} extra
      #{poke pcre_extra, flags} extra $ flags .|. (0 :: CULong)
        .|. #{const PCRE_EXTRA_MATCH_LIMIT}
        .|. #{const PCRE_EXTRA_MATCH_LIMIT_RECURSION}

    matchLimit          = 100000 :: CULong
    matchLimitRecursion =   5000 :: CULong

    patchError = concatMap patch
      where patch '\\' = "%"
            patch '('  = "%("
            patch ')'  = "%)"
            patch  c   = [c]

    options = #{const PCRE_UTF8 | PCRE_NO_UTF8_CHECK}
      -- allow PCRE to optimize .* at beginning of pattern by implicit anchor
      .|. #{const PCRE_DOTALL}
      .|. if caseMatters then 0 else #{const PCRE_CASELESS}

maxCaptures = 10
ovecLen     = maxCaptures * 3

data MatchResult = MatchFailed
                 | MatchAborted
                 | MatchSucceeded [(Int, Int)]
                 deriving Show

match :: Regexp -> Text -> IO MatchResult
match = doMatch match_helper

rmatch :: Regexp -> Text -> IO MatchResult
rmatch = doMatch rmatch_helper

doMatch :: (Ptr PCRE -> Ptr PCREExtra -> CString -> CInt ->
            CInt -> Ptr CInt -> IO CInt) -> Regexp -> Text -> IO MatchResult
doMatch helper Regexp { code = codeFP, extra = extraFP } text =
  withForeignPtr codeFP  $ \code           ->
  withForeignPtr extraFP $ \extra          ->
  useAsCStringLen string $ \(cstring, len) ->
  allocaArray ovecLen    $ \ovec           -> do

    rc <- helper code extra cstring (fromIntegral len) options ovec
    if rc < 0
      then case rc of
        #{const PCRE_ERROR_NOMATCH} -> return MatchFailed
        _                           -> return MatchAborted
      else mkMatchResult rc ovec subject

  where string  = encodeUtf8 text
        subject = (string, T.length text)
        options = #{const PCRE_NO_UTF8_CHECK}

mkMatchResult :: CInt -> Ptr CInt -> (ByteString, Int) -> IO MatchResult
mkMatchResult rc ovec (subject, subjectCharLen) =
  (MatchSucceeded . pairs . map (rebase . fromIntegral)) `liftM`
  peekArray (n * 2) ovec

  where rc' = fromIntegral rc
        n   = if rc' == 0 || rc' > maxCaptures then maxCaptures else rc'

        pairs (s:e:rs) = (s, e) : pairs rs
        pairs []       = []

        -- translate UTF-8 byte offset to character offset
        rebase 0 = 0
        rebase i = subjectCharLen - T.length (decodeUtf8 $ BS.drop i subject)

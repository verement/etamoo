
module MOO.Builtins.Match ( MatchResult(..)
                          , newRegexp
                          , match
                          , rmatch
                          ) where

import Foreign hiding (unsafePerformIO)
import Foreign.C
import Control.Monad
import Control.Exception
import Control.Concurrent.MVar
import Data.Text (Text)
import Data.Text.Encoding
import Data.ByteString (ByteString, useAsCString, useAsCStringLen)
import Data.Bits
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text as T
import qualified Data.ByteString as BS

#include <pcre.h>

data PCRE
data PCREExtra
data PCRECalloutBlock
data CharacterTables

type Callout = Ptr PCRECalloutBlock -> IO CInt

foreign import ccall unsafe "static pcre.h"
  pcre_compile :: CString -> CInt -> Ptr CString -> Ptr CInt ->
                  Ptr CharacterTables -> IO (Ptr PCRE)

foreign import ccall unsafe "static pcre.h"
  pcre_study :: Ptr PCRE -> CInt -> Ptr CString -> IO (Ptr PCREExtra)

foreign import ccall unsafe "static pcre.h &"
  pcre_free_study :: FunPtr (Ptr PCREExtra -> IO ())

foreign import ccall safe "static pcre.h"
  pcre_exec :: Ptr PCRE -> Ptr PCREExtra -> CString -> CInt -> CInt ->
               CInt -> Ptr CInt -> CInt -> IO CInt

foreign import ccall unsafe "static pcre.h &"
  pcre_free :: Ptr (FunPtr (Ptr a -> IO ()))

foreign import ccall "static pcre.h &"
  pcre_callout :: Ptr (FunPtr Callout)

foreign import ccall "wrapper"
  mkCallout :: Callout -> IO (FunPtr Callout)

data Regexp = Regexp {
    pattern     :: Text
  , caseMatters :: Bool

  , code        :: ForeignPtr PCRE
  , extra       :: ForeignPtr PCREExtra
  }
            deriving Show

instance Eq Regexp where
  Regexp { pattern = p1, caseMatters = cm1 } ==
    Regexp { pattern = p2, caseMatters = cm2 } =
      cm1 == cm2 && p1 == p2

data RewriteState = StateBase
                  | StateEsc
                  | StateCsetInit
                  | StateCsetInit2
                  | StateCset

translate :: Text -> ByteString
translate = encodeUtf8 . T.pack . concat . rewrite . T.unpack
  where
    -- Translate MOO regular expression syntax into PCRE syntax
    --
    -- Aside from changing % to \ and sundry tweaks we also address an
    -- incompatibility between MOO %b, %B, %w, %W and PCRE \b, \B, \w,
    -- \W -- namely, the inclusion of _ in \w and its absence in %w.

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

newRegexp :: Text -> Bool -> IO (Either (String, CInt) Regexp)
newRegexp regexp caseMatters =
  useAsCString (translate regexp) $ \pattern ->
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

-- We need a lock to protect pcre_callout which is shared by all threads
matchLock :: MVar ()
matchLock = unsafePerformIO $ newMVar ()

match :: Regexp -> ByteString -> IO MatchResult
match Regexp { code = codeFP, extra = extraFP } string =
  bracket (takeMVar matchLock) (putMVar matchLock) $ \_ ->
  withForeignPtr codeFP  $ \code           ->
  withForeignPtr extraFP $ \extra          ->
  useAsCStringLen string $ \(cstring, len) ->
  allocaArray ovecLen    $ \ovec           -> do

    flags <- #{peek pcre_extra, flags} extra
    #{poke pcre_extra, flags} extra $ flags .&. complement (0 :: CULong)
      .&. complement #{const PCRE_EXTRA_CALLOUT_DATA}
    poke pcre_callout nullFunPtr

    rc <- pcre_exec code extra cstring (fromIntegral len) 0 options
          ovec (fromIntegral ovecLen)
    if rc < 0
      then case rc of
        #{const PCRE_ERROR_NOMATCH} -> return MatchFailed
        _                           -> return MatchAborted
      else mkMatchResult rc ovec

  where options = #{const PCRE_NO_UTF8_CHECK}

rmatch :: Regexp -> ByteString -> IO MatchResult
rmatch Regexp {code = codeFP, extra = extraFP } string =
  bracket (takeMVar matchLock) (putMVar matchLock) $ \_ ->
  withForeignPtr codeFP  $ \code           ->
  withForeignPtr extraFP $ \extra          ->
  useAsCStringLen string $ \(cstring, len) ->
  allocaArray ovecLen    $ \ovec           ->
  allocaArray ovecLen    $ \rOvec          -> do

    rdRef <- newIORef RmatchData { rmatchResult = 0, rmatchOvec = rOvec }
    bracket (newStablePtr rdRef) freeStablePtr $ \sp -> do
      #{poke pcre_extra, callout_data} extra sp

      flags <- #{peek pcre_extra, flags} extra
      #{poke pcre_extra, flags} extra $ flags .|. (0 :: CULong)
              .|. #{const PCRE_EXTRA_CALLOUT_DATA}

      bracket (mkCallout rmatchCallout) freeHaskellFunPtr $ \callout -> do
        poke pcre_callout callout

        rc <- pcre_exec code extra cstring (fromIntegral len) 0 options
              ovec (fromIntegral ovecLen)
        if rc < 0
          then case rc of
            #{const PCRE_ERROR_NOMATCH} -> do
              rd <- readIORef rdRef
              if valid rd
                then mkMatchResult (rmatchResult rd) (rmatchOvec rd)
                else return MatchFailed
            _ -> return MatchAborted
          else mkMatchResult rc ovec
  where options = #{const PCRE_NO_UTF8_CHECK}

mkMatchResult :: CInt -> Ptr CInt -> IO MatchResult
mkMatchResult rc ovec = fmap (MatchSucceeded . convert) $
                        peekArray (n * 2) ovec
  where rc' = fromIntegral rc
        n   = if rc' == 0 || rc' > maxCaptures then maxCaptures else rc'
        convert (s:e:rs) = (fromIntegral s, fromIntegral e) : convert rs
        convert []       = []

data RmatchData = RmatchData {
    rmatchResult :: CInt
  , rmatchOvec   :: Ptr CInt
  }

valid :: RmatchData -> Bool
valid RmatchData { rmatchResult = rc } = rc /= 0

rmatchCallout :: Callout
rmatchCallout block = do
  rdRef <- deRefStablePtr =<< #{peek pcre_callout_block, callout_data} block
  rd <- readIORef rdRef

  currentPos <- #{peek pcre_callout_block, current_position} block
  startMatch <- #{peek pcre_callout_block, start_match}      block

  let ovec = rmatchOvec rd
  ovec0 <- peekElemOff ovec 0
  ovec1 <- peekElemOff ovec 1

  when (not (valid rd) || startMatch > ovec0 ||
        (startMatch == ovec0 && currentPos > ovec1)) $ do
    -- make a copy of the offsets vector so the last such vector found can
    -- be returned as the rightmost match
    pokeElemOff ovec 0 startMatch
    pokeElemOff ovec 1 currentPos

    offsetVector <- #{peek pcre_callout_block, offset_vector} block
    captureTop   <- #{peek pcre_callout_block, capture_top}   block

    copyArray (ovec         `advancePtr` 2)
              (offsetVector `advancePtr` 2)
              (sizeOf ovec0 * 2 * (fromIntegral captureTop - 1))

    writeIORef rdRef rd { rmatchResult = captureTop }

  return 1  -- cause match failure at current point, but continue trying

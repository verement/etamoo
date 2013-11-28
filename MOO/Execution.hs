
{-# LANGUAGE OverloadedStrings #-}

module MOO.Execution ( MOO
                     , Environment ( indexLength )
                     , StackFrame ( variables, debugBit )
                     , Exception ( .. )
                     , initEnvironment
                     , initStack
                     , reader
                     , local
                     , frame
                     , modifyFrame
                     , catchException
                     , raiseException
                     , notyet
                     , raise
                     , checkFloat
                     , runContT
                     , evalStateT
                     , runReaderT
                     ) where

import           Control.Monad.Cont
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Arrow (first)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V

import MOO.Types

type MOO = ReaderT Environment (StateT CallStack (ContT Value IO))

data Environment =
  Env { exceptionHandler :: ExceptionHandler
      , indexLength      :: MOO Int
      }

initEnvironment :: Environment
initEnvironment = Env {
    exceptionHandler = Handler $ \(Exception _ m _) -> error (T.unpack m)
  , indexLength      = error "Invalid index context"
  }

newtype CallStack = Stack [StackFrame]

data StackFrame =
  Frame { variables :: Map Id Value
        , debugBit  :: Bool
        }

initStack :: CallStack
initStack = Stack [Frame { variables = mkInitVars
                         , debugBit  = True
                         }]

currentFrame :: CallStack -> StackFrame
currentFrame (Stack (x:_)) = x
currentFrame (Stack  [])   = error "Empty call stack"

frame :: (StackFrame -> a) -> MOO a
frame f = gets (f . currentFrame)

modifyFrame :: (StackFrame -> StackFrame) -> MOO ()
modifyFrame f = modify $ \(Stack (frame:stack)) -> Stack (f frame : stack)

mkInitVars = Map.fromList $ map (first T.toCaseFold) initVars

initVars = [
    ("player" , Obj (-1))
  , ("this"   , Obj (-1))
  , ("caller" , Obj (-1))

  , ("args"   , Lst V.empty)
  , ("argstr" , Str T.empty)

  , ("verb"   , Str T.empty)
  , ("dobjstr", Str T.empty)
  , ("dobj"   , Obj (-1))
  , ("prepstr", Str T.empty)
  , ("iobjstr", Str T.empty)
  , ("iobj"   , Obj (-1))
  ] ++ typeVars

typeVars = [
    ("INT"  , Int $ typeCode TInt)
  , ("NUM"  , Int $ typeCode TInt)
  , ("FLOAT", Int $ typeCode TFlt)
  , ("LIST" , Int $ typeCode TLst)
  , ("STR"  , Int $ typeCode TStr)
  , ("OBJ"  , Int $ typeCode TObj)
  , ("ERR"  , Int $ typeCode TErr)
  ]

newtype ExceptionHandler = Handler (Exception -> MOO Value)

data Exception = Exception Code Message Value
type Code = Value
type Message = StrT

catchException :: MOO a -> (Exception -> MOO a) -> MOO a
catchException action handler = callCC $ \k -> local (mkHandler k) action
  where mkHandler k r = r { exceptionHandler = Handler $ \e ->
                             local (const r) $ handler e >>= k }

raiseException :: Exception -> MOO a
raiseException except = do
  Handler handler <- reader exceptionHandler
  handler except
  error "Returned from exception handler"

notyet = raiseException $
         Exception (Err E_INVARG) "Not yet implemented" (Int 0)

raise :: Error -> MOO a
raise err = raiseException $ Exception (Err err) (error2text err) (Int 0)

checkFloat :: FltT -> MOO Value
checkFloat flt | isInfinite flt = raise E_FLOAT
               | isNaN      flt = raise E_INVARG
               | otherwise      = return (Flt flt)

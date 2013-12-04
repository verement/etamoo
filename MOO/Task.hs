
{-# LANGUAGE OverloadedStrings #-}

module MOO.Task ( MOO
                , Task
                , Environment ( .. )
                , TaskState ( .. )
                , CallStack ( .. )
                , StackFrame ( .. )
                , Exception ( .. )
                , liftSTM
                , initEnvironment
                , initState
                , getDatabase
                , putDatabase
                , getObject
                , reader
                , local
                , frame
                , modifyFrame
                , catchException
                , raiseException
                , notyet
                , raise
                , checkFloat
                , binaryString
                , random
                , appendIO
                , runContT
                , evalStateT
                , runReaderT
                ) where

import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Arrow (first)
import Control.Concurrent.STM
import System.Random hiding (random)
import System.Time
import Data.ByteString (ByteString)
import Data.Map (Map)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString as BS

import MOO.Types
import {-# SOURCE #-} MOO.Database
import MOO.Object

data Task = Task
          deriving Show

type MOO = ReaderT Environment (ContT Value (StateT TaskState STM))

liftSTM :: STM a -> MOO a
liftSTM = lift . lift . lift

data Environment = Env {
    taskId           :: IntT
  , startTime        :: ClockTime
  , exceptionHandler :: ExceptionHandler
  , indexLength      :: MOO Int
}

initEnvironment :: IO Environment
initEnvironment = do
  taskId    <- randomRIO (1, maxBound)
  startTime <- getClockTime
  return Env {
      taskId           = taskId
    , startTime        = startTime
    , exceptionHandler = Handler $ \(Exception _ m _) -> error (T.unpack m)
    , indexLength      = error "Invalid index context"
  }

data TaskState = State {
    database  :: TVar Database
  , stack     :: CallStack
  , randomGen :: StdGen
  , queuedIO  :: IO ()
}

initState :: TVar Database -> StdGen -> TaskState
initState db gen = State {
    database  = db
  , stack     = initStack
  , randomGen = gen
  , queuedIO  = return ()
}

getDatabase :: MOO Database
getDatabase = liftSTM . readTVar =<< gets database

putDatabase :: Database -> MOO ()
putDatabase db = do
  dbTVar <- gets database
  liftSTM $ writeTVar dbTVar db

getObject :: ObjId -> MOO (Maybe Object)
getObject oid = liftSTM . dbObject oid =<< getDatabase

newtype CallStack = Stack [StackFrame]
                  deriving Show

data StackFrame = Frame {
    variables :: Map Id Value
  , debugBit  :: Bool
} deriving Show

initStack :: CallStack
initStack = Stack [
  Frame {
       variables = mkInitVars
     , debugBit  = True
   }]

currentFrame :: CallStack -> StackFrame
currentFrame (Stack (x:_)) = x
currentFrame (Stack  [])   = error "Empty call stack"

frame :: (StackFrame -> a) -> MOO a
frame f = gets (f . currentFrame . stack)

modifyFrame :: (StackFrame -> StackFrame) -> MOO ()
modifyFrame f = modify $ \st@State { stack = Stack (frame:stack) } ->
  st { stack = Stack (f frame : stack) }

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
  where mkHandler k env = env { exceptionHandler = Handler $ \e ->
                                 local (const env) $ handler e >>= k }

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

binaryString :: StrT -> MOO ByteString
binaryString = maybe (raise E_INVARG) (return . BS.pack) . text2binary

random :: (Random a) => (a, a) -> MOO a
random range = do
  g <- gets randomGen
  let (r, g') = randomR range g
  modify $ \st -> st { randomGen = g' }
  return r

appendIO :: IO () -> MOO ()
appendIO io = do
  currentIO <- gets queuedIO
  modify $ \st -> st { queuedIO = currentIO >> io }

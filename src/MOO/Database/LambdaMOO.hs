
{-# LANGUAGE OverloadedStrings #-}

module MOO.Database.LambdaMOO ( loadLMDatabase, saveLMDatabase ) where

import Control.Applicative ((<$>))
import Control.Concurrent.STM (STM, atomically, readTVar, writeTVar)
import Control.Monad (unless, when, forM, forM_, join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, local, asks, ask)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Array (Array, listArray, inRange, bounds, (!), elems, assocs)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.IntSet (IntSet)
import Data.List (sort, foldl', elemIndex)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText,
                               fromLazyText, fromString, singleton)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Data.Word (Word)
import System.IO (Handle, withFile, IOMode(ReadMode, WriteMode),
                  hSetBuffering, BufferMode(BlockBuffering),
                  hSetNewlineMode, NewlineMode(NewlineMode, inputNL, outputNL),
                  Newline(CRLF, LF), hSetEncoding, utf8)
import Text.Parsec (ParseError, ParsecT, runParserT, string, count,
                    getState, putState, many1, oneOf, manyTill, anyToken,
                    digit, char, option, try, lookAhead, (<|>), (<?>))

import qualified Data.HashMap.Strict as HM
import qualified Data.IntSet as IS
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Vector as V

import MOO.Compiler
import MOO.Database
import MOO.Object
import MOO.Parser
import MOO.Types
import MOO.Unparser
import MOO.Verb

import qualified MOO.String as Str

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

withDBFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withDBFile dbFile mode io = withFile dbFile mode $ \handle -> do
  hSetBuffering handle (BlockBuffering Nothing)
  hSetNewlineMode handle NewlineMode { inputNL = CRLF, outputNL = LF }
  hSetEncoding handle utf8
  io handle

loadLMDatabase :: FilePath -> (T.Text -> IO ()) ->
                  IO (Either ParseError Database)
loadLMDatabase dbFile writeLog = withDBFile dbFile ReadMode $ \handle -> do
  writeLog $ "LOADING: " <> T.pack dbFile
  contents <- TL.hGetContents handle
  runReaderT (runParserT lmDatabase initDatabase dbFile contents)
    (initDBEnv writeLog)

type DBParser = ParsecT Text Database (ReaderT DBEnv IO)

data DBEnv = DBEnv {
    logger        :: T.Text -> IO ()
  , input_version :: Word
  , users         :: IntSet
}

initDBEnv logger = DBEnv {
    logger        = logger
  , input_version = undefined
  , users         = IS.empty
}

writeLog :: String -> DBParser ()
writeLog line = asks logger >>= \writeLog ->
  liftIO (writeLog $ "LOADING: " <> T.pack line)

header_format_string = ("** LambdaMOO Database, Format Version ", " **")

lmDatabase :: DBParser Database
lmDatabase = do
  let (before, after) = header_format_string
  dbVersion <- line $ between (string before) (string after) unsignedInt

  writeLog $ "LambdaMOO database (version " ++ show dbVersion ++ ")"

  unless (dbVersion < num_db_versions) $
    fail $ "Unsupported database format (version " ++ show dbVersion ++ ")"

  local (\r -> r { input_version = dbVersion }) $ do
    nobjs  <- line signedInt
    nprogs <- line signedInt
    dummy  <- line signedInt
    nusers <- line signedInt

    writeLog $ show nobjs ++ " objects, "
      ++ show nprogs ++ " programs, "
      ++ show nusers ++ " users"

    users <- count nusers read_objid
    installUsers users

    writeLog $ "Players: " ++
      T.unpack (toLiteral $ objectList $ sort users)

    local (\r -> r { users = IS.fromList users }) $ do
      writeLog $ "Reading " <> show nobjs <> " objects..."
      installObjects =<< count nobjs read_object

      writeLog $ "Reading " <> show nprogs <> " MOO verb programs..."
      mapM_ installProgram =<< count nprogs dbProgram

      writeLog "Reading forked and suspended tasks..."
      read_task_queue

      writeLog "Reading list of formerly active connections..."
      read_active_connections

  eof
  writeLog "Database loaded!"

  getState

installUsers :: [ObjId] -> DBParser ()
installUsers users = do
  db <- getState
  putState $ foldr (setPlayer True) db users

data DBObject = DBObject {
    oid   :: ObjId
  , valid :: Maybe ObjectDef
}

data ObjectDef = ObjectDef {
    objName     :: String
  , objFlags    :: Int
  , objOwner    :: ObjId

  , objLocation :: ObjId
  , objContents :: ObjId
  , objNext     :: ObjId

  , objParent   :: ObjId
  , objChild    :: ObjId
  , objSibling  :: ObjId

  , objVerbdefs :: [VerbDef]

  , objPropdefs :: [PropDef]
  , objPropvals :: [PropVal]
}

data VerbDef = VerbDef {
    vbName  :: String
  , vbOwner :: ObjId
  , vbPerms :: Int
  , vbPrep  :: Int
}

type PropDef = String

data PropVal = PropVal {
    propVar   :: LMVar
  , propOwner :: ObjId
  , propPerms :: IntT
}

installObjects :: [DBObject] -> DBParser ()
installObjects dbObjs = do
  -- Check sequential object ordering
  mapM_ checkObjId (zip [0..] dbObjs)

  objs <- liftIO (mapM installPropsAndVerbs preObjs) >>= setPlayerFlags
  getState >>= liftIO . setObjects objs >>= putState

  where dbArray = listArray (0, length dbObjs - 1) $ map valid dbObjs
        preObjs = map (objectForDBObject dbArray) dbObjs

        checkObjId (objId, dbObj) =
          unless (objId == oid dbObj) $
            fail $ "Unexpected object #" ++ show (oid dbObj) ++
                   " (expecting #" ++ show objId ++ ")"

        installPropsAndVerbs :: (ObjId, Maybe Object) -> IO (Maybe Object)
        installPropsAndVerbs (_  , Nothing)  = return Nothing
        installPropsAndVerbs (oid, Just obj) =
          let Just def = dbArray ! oid
              propvals = objPropvals def
              verbdefs = objVerbdefs def
          in fmap Just $ setProperties (mkProperties False oid propvals) obj >>=
             setVerbs (map mkVerb verbdefs)

        mkProperties :: Bool -> ObjId -> [PropVal] -> [Property]
        mkProperties _ _ [] = []
        mkProperties inherited oid propvals
          | inRange (bounds dbArray) oid =
            case maybeDef of
              Nothing  -> []
              Just def ->
                let propdefs = objPropdefs def
                    (mine, others) = splitAt (length propdefs) propvals
                    properties = zipWith (mkProperty inherited) propdefs mine
                in properties ++ mkProperties True (objParent def) others
          | otherwise = []
          where maybeDef = dbArray ! oid

        mkProperty :: Bool -> PropDef -> PropVal -> Property
        mkProperty inherited propdef propval = initProperty {
            propertyName      = Str.fromString propdef
          , propertyValue     = either (const Nothing) id $
                                valueFromVar (propVar propval)
          , propertyInherited = inherited
          , propertyOwner     = propOwner propval
          , propertyPermR     = propPerms propval .&. pf_read  /= 0
          , propertyPermW     = propPerms propval .&. pf_write /= 0
          , propertyPermC     = propPerms propval .&. pf_chown /= 0
        }

        mkVerb :: VerbDef -> Verb
        mkVerb def = initVerb {
            verbNames          = Str.fromString $ vbName def
          , verbOwner          = vbOwner def
          , verbPermR          = vbPerms def .&. vf_read  /= 0
          , verbPermW          = vbPerms def .&. vf_write /= 0
          , verbPermX          = vbPerms def .&. vf_exec  /= 0
          , verbPermD          = vbPerms def .&. vf_debug /= 0
          , verbDirectObject   = toEnum $ fromIntegral $
                                 (vbPerms def `shiftR` dobjShift) .&. objMask
          , verbPreposition    = toEnum $ fromIntegral $ 2 + vbPrep def
          , verbIndirectObject = toEnum $ fromIntegral $
                                 (vbPerms def `shiftR` iobjShift) .&. objMask
        }

        setPlayerFlags :: [Maybe Object] -> DBParser [Maybe Object]
        setPlayerFlags objs = do
          players <- asks users
          return $ map (setPlayerFlag players) $ zip [0..] objs

        setPlayerFlag :: IntSet -> (ObjId, Maybe Object) -> Maybe Object
        setPlayerFlag players (oid, Just obj) = Just $
          obj { objectIsPlayer = oid `IS.member` players }
        setPlayerFlag _ _ = Nothing

objectTrail :: Array ObjId (Maybe ObjectDef) -> ObjectDef ->
               (ObjectDef -> ObjId) -> (ObjectDef -> ObjId) -> [ObjId]
objectTrail arr def first rest = follow first rest (Just def)
  where follow _  _  Nothing = []
        follow f1 f2 (Just def)
          | inRange (bounds arr) idx = idx : follow f2 f2 (arr ! idx)
          | otherwise                = []
          where idx = f1 def

objectForDBObject :: Array ObjId (Maybe ObjectDef) ->
                     DBObject -> (ObjId, Maybe Object)
objectForDBObject dbArray dbObj = (oid dbObj, mkObject <$> valid dbObj)
  where mkObject def = initObject {
            objectParent     = maybeObject (objParent   def)
          , objectChildren   = IS.fromList $
                               objectTrail dbArray def objChild objSibling
          , objectName       = Str.fromString $ objName     def
          , objectOwner      =                  objOwner    def
          , objectLocation   = maybeObject (objLocation def)
          , objectContents   = IS.fromList $
                               objectTrail dbArray def objContents objNext
          , objectProgrammer = flag def flag_programmer
          , objectWizard     = flag def flag_wizard
          , objectPermR      = flag def flag_read
          , objectPermW      = flag def flag_write
          , objectPermF      = flag def flag_fertile
        }

        flag def fl = let mask = 1 `shiftL` fl
                      in (objFlags def .&. mask) /= 0

        maybeObject :: ObjId -> Maybe ObjId
        maybeObject oid
          | oid >=  0 = Just oid
          | otherwise = Nothing

installProgram :: (Int, Int, Program) -> DBParser ()
installProgram (oid, vnum, program) = do
  db <- getState
  maybeObj <- liftIO $ atomically $ dbObject oid db
  case maybeObj of
    Nothing  -> fail $ doesNotExist "Object"
    Just obj -> case lookupVerbRef obj (Int $ 1 + fromIntegral vnum) of
      Nothing            -> fail $ doesNotExist "Verb"
      Just (_, verbTVar) -> liftIO $ atomically $ do
        verb <- readTVar verbTVar
        writeTVar verbTVar verb {
            verbProgram = program
          , verbCode    = compile program
        }

  where doesNotExist what = what ++ " for program " ++ desc ++ " does not exist"
        desc = "#" ++ show oid ++ ":" ++ show vnum

integer :: DBParser Integer
integer = signed (read <$> many1 digit)

unsignedInt :: DBParser Word
unsignedInt = read <$> many1 digit

signedInt :: DBParser Int
signedInt = signed (read <$> many1 digit)

signed :: (Num a) => DBParser a -> DBParser a
signed parser = negative <|> parser
  where negative = char '-' >> negate <$> parser

line :: DBParser a -> DBParser a
line parser = do
  x <- parser
  char '\n'
  return x

read_num :: DBParser IntT
read_num = line (fromInteger <$> integer) <?> "num"

read_objid :: DBParser ObjId
read_objid = line (fromInteger <$> integer) <?> "objid"

read_float :: DBParser FltT
read_float = line (fmap read $ many1 $ oneOf "-0123456789.eE+") <?> "float"

read_string :: DBParser String
read_string = manyTill anyToken (char '\n') <?> "string"

read_object :: DBParser DBObject
read_object = (<?> "object") $ do
  oid <- fmap fromIntegral $ char '#' >> signedInt
  recycled <- option False (string " recycled" >> return True)
  char '\n'

  objectDef <- if recycled then return Nothing else do
    name <- read_string

    -- liftIO $ putStrLn $ "  #" ++ show oid ++ " (" ++ name ++ ")"

    read_string  -- old handles string
    flags <- read_num

    owner <- read_objid

    location <- read_objid
    contents <- read_objid
    next     <- read_objid

    parent  <- read_objid
    child   <- read_objid
    sibling <- read_objid

    numVerbdefs <- read_num
    verbdefs <- count (fromIntegral numVerbdefs) read_verbdef

    numPropdefs <- read_num
    propdefs <- count (fromIntegral numPropdefs) read_propdef

    nprops <- read_num
    propvals <- count (fromIntegral nprops) read_propval

    return $ Just ObjectDef {
        objName     = name
      , objFlags    = fromIntegral flags
      , objOwner    = owner
      , objLocation = location
      , objContents = contents
      , objNext     = next
      , objParent   = parent
      , objChild    = child
      , objSibling  = sibling
      , objVerbdefs = verbdefs
      , objPropdefs = propdefs
      , objPropvals = propvals
    }

  return DBObject { oid = oid, valid = objectDef }

read_verbdef :: DBParser VerbDef
read_verbdef = (<?> "verbdef") $ do
  name  <- read_string
  owner <- read_objid
  perms <- read_num
  prep  <- read_num

  return VerbDef {
      vbName  = name
    , vbOwner = owner
    , vbPerms = fromIntegral perms
    , vbPrep  = fromIntegral prep
  }

read_propdef :: DBParser PropDef
read_propdef = read_string <?> "propdef"

read_propval :: DBParser PropVal
read_propval = (<?> "propval") $ do
  var   <- read_var
  owner <- read_objid
  perms <- read_num

  return PropVal {
      propVar   = var
    , propOwner = owner
    , propPerms = perms
  }

data LMVar = LMClear
           | LMNone
           | LMStr     String
           | LMObj     IntT
           | LMErr     IntT
           | LMInt     IntT
           | LMCatch   IntT
           | LMFinally IntT
           | LMFloat   FltT
           | LMList    [LMVar]

valueFromVar :: LMVar -> Either LMVar (Maybe Value)
valueFromVar LMClear       = Right Nothing
valueFromVar (LMStr str)   = Right $ Just (Str $ Str.fromString str)
valueFromVar (LMObj obj)   = Right $ Just (Obj $ fromIntegral obj)
valueFromVar (LMErr err)   = Right $ Just (Err $ toEnum $ fromIntegral err)
valueFromVar (LMInt int)   = Right $ Just (Int int)
valueFromVar (LMFloat flt) = Right $ Just (Flt flt)
valueFromVar (LMList list) = do
  elems <- mapM valueFromVar list
  return $ Just (fromList $ catMaybes elems)
valueFromVar var           = Left var

read_var :: DBParser LMVar
read_var = (<?> "var") $ do
  l <- read_num
  l <- if l == type_any
       then do input_version <- asks input_version
               return $ if input_version == dbv_prehistory
                        then type_none else l
       else return l

  cases l

  where
    cases l
      | l == type_clear   = return LMClear
      | l == type_none    = return LMNone
      | l == _type_str    =        LMStr     <$> read_string
      | l == type_obj     =        LMObj     <$> read_num
      | l == type_err     =        LMErr     <$> read_num
      | l == type_int     =        LMInt     <$> read_num
      | l == type_catch   =        LMCatch   <$> read_num
      | l == type_finally =        LMFinally <$> read_num
      | l == _type_float  =        LMFloat   <$> read_float
      | l == _type_list   = do
          l <- read_num
          LMList <$> count (fromIntegral l) read_var

    cases l = fail $ "Unknown type (" ++ show l ++ ")"

dbProgram :: DBParser (Int, Int, Program)
dbProgram = do
  char '#'
  oid <- signedInt
  char ':'
  vnum <- signedInt
  char '\n'

  let verbdesc = "#" ++ show oid ++ ":" ++ show vnum

  -- liftIO $ putStr ("  " ++ verbdesc ++ "     \r") >> hFlush stdout

  program <- read_program
  case program of
    Left  err  -> fail $ "Parse error in " ++ verbdesc ++ ": " ++ head err
    Right prog -> return (oid, vnum, prog)

read_program :: DBParser (Either [String] Program)
read_program = (<?> "program") $ do
  source <- try (string ".\n" >> return "") <|>
            manyTill anyToken (try $ string "\n.\n")
  return $ parse (T.pack source)

read_task_queue :: DBParser ()
read_task_queue = (<?> "task_queue") $ do
  nclocks <- signedInt
  string " clocks\n"
  count nclocks $
    signedInt >> char ' ' >> signedInt >> char ' ' >> signedInt >> char '\n'

  ntasks <- signedInt
  string " queued tasks\n"
  count ntasks $ do
    signedInt >> char ' '
    first_lineno <- signedInt
    char ' '
    st <- signedInt
    char ' '
    id <- signedInt
    char '\n'

    a <- read_activ_as_pi
    read_rt_env
    program <- read_program
    return ()

  suspended_count <- signedInt
  string " suspended tasks\n"
  count suspended_count $ do
    start_time <- signedInt
    char ' '
    task_id <- signedInt
    value <- (char ' ' >> read_var) <|> (char '\n' >> return (LMInt 0))
    the_vm <- read_vm
    return ()

  return ()

read_vm :: DBParser ()
read_vm = (<?> "vm") $ do
  top <- unsignedInt
  char ' '
  vector <- signedInt
  char ' '
  func_id <- unsignedInt
  max <- (char ' ' >> unsignedInt) <|>
         (lookAhead (char '\n') >> return default_max_stack_depth)
  char '\n'
  count (fromIntegral top) read_activ
  return ()

default_max_stack_depth = 50

read_activ :: DBParser ()
read_activ = (<?> "activ") $ do
  input_version <- asks input_version
  version <- if input_version < dbv_float then return input_version
             else string "language version " >> unsignedInt
  unless (version < num_db_versions) $
    fail $ "Unrecognized language version: " ++ show version

  prog <- read_program
  read_rt_env

  stack_in_use <- signedInt
  string " rt_stack slots in use\n"
  count stack_in_use read_var

  read_activ_as_pi
  temp <- read_var

  pc <- unsignedInt
  char ' '
  i <- unsignedInt
  let bi_func_pc = i
  error_pc <- (lookAhead (char '\n') >> return pc) <|> (char ' ' >> unsignedInt)
  char '\n'

  when (bi_func_pc /= 0) $ do
    func_name <- read_string
    read_bi_func_data

read_activ_as_pi :: DBParser ()
read_activ_as_pi = (<?> "activ_as_pi") $ do
  read_var

  this <- signedInt
  char ' ' >> signedInt
  char ' ' >> signedInt
  player <- char ' ' >> signedInt
  char ' ' >> signedInt
  progr <- char ' ' >> signedInt
  vloc <- char ' ' >> signedInt
  char ' ' >> signedInt
  debug <- char ' ' >> signedInt
  char '\n'

  read_string  -- was argstr
  read_string  -- was dobjstr
  read_string  -- was iobjstr
  read_string  -- was prepstr

  verb <- read_string
  verbname <- read_string

  return ()

read_rt_env :: DBParser ()
read_rt_env = (<?> "rt_env") $ do
  old_size <- signedInt
  string " variables\n"
  count old_size $ do
    old_names <- read_string
    rt_env <- read_var
    return (old_names, rt_env)
  return ()

read_bi_func_data :: DBParser ()
read_bi_func_data = return ()

read_active_connections :: DBParser [(Int, Int)]
read_active_connections = (<?> "active_connections") $ (eof >> return []) <|> do
  nconnections <- signedInt
  string " active connections"
  have_listeners <- (string " with listeners\n" >> return True) <|>
                    (char '\n' >> return False)

  count nconnections $
    if have_listeners
      then do who <- signedInt
              char ' '
              listener <- signedInt
              char '\n'
              return (who, listener)
      else do who <- read_num
              return (fromIntegral who, system_object)

-- Enumeration constants from LambdaMOO
type_int     = 0
type_obj     = 1
_type_str    = 2
type_err     = 3
_type_list   = 4
type_clear   = 5
type_none    = 6
type_catch   = 7
type_finally = 8
_type_float  = 9

type_any     = -1
type_numeric = -2

dbv_prehistory  = 0
dbv_exceptions  = 1
dbv_breakcont   = 2
dbv_float       = 3
dbv_bfbugfixed  = 4
num_db_versions = 5

system_object = 0

flag_user       = 0
flag_programmer = 1
flag_wizard     = 2
flag_obsolete_1 = 3
flag_read       = 4
flag_write      = 5
flag_obsolete_2 = 6
flag_fertile    = 7

pf_read  = 0x01
pf_write = 0x02
pf_chown = 0x04

vf_read  = 0x01
vf_write = 0x02
vf_exec  = 0x04
vf_debug = 0x08

dobjShift = 4
iobjShift = 6
objMask   = 0x3
permMask  = 0xF

-- Database writing ...

type DBWriter = ReaderT Database (WriterT Builder STM)

liftSTM :: STM a -> DBWriter a
liftSTM = lift . lift

saveLMDatabase :: FilePath -> Database -> IO ()
saveLMDatabase dbFile database = withDBFile dbFile WriteMode $ \handle -> do
  let writer = runReaderT writeDatabase database
      stm    = execWriterT writer
  TL.hPutStr handle . toLazyText =<< atomically stm

tellLn :: Builder -> DBWriter ()
tellLn line = tell line >> tell (singleton '\n')

writeDatabase :: DBWriter ()
writeDatabase = do
  let (before, after) = header_format_string
  tell (fromString before)
  tell (decimal $ num_db_versions - 1)
  tellLn (fromString after)

  db <- ask
  let nobjs = maxObject db + 1
  tellLn (decimal nobjs)

  objects <- listArray (0, maxObject db) <$>
             forM [0..maxObject db] (\oid -> liftSTM $ dbObject oid db)

  let nprogs = foldl' numVerbs 0 (elems objects)
        where numVerbs acc Nothing    = acc
              numVerbs acc (Just obj) = acc + length (objectVerbs obj)
  tellLn (decimal nprogs)

  let dummy = 0 :: Int
  tellLn (decimal dummy)

  let users  = allPlayers db
      nusers = length users
  tellLn (decimal nusers)
  forM_ users $ tellLn . decimal

  verbs <- forM (assocs objects) $ tellObject objects
  forM_ verbs tellVerbs

  tellLn "0 clocks"
  tellLn "0 queued tasks"
  tellLn "0 suspended tasks"

tellObject :: Array ObjId (Maybe Object) -> (ObjId, Maybe Object) ->
              DBWriter (ObjId, [Verb])
tellObject objects (oid, Just obj) = do
  tell (singleton '#')
  tellLn (decimal oid)

  tellLn (string2builder $ objectName obj)
  tellLn ""  -- old handles string

  let flags = flag objectIsPlayer   flag_user       .|.
              flag objectProgrammer flag_programmer .|.
              flag objectWizard     flag_wizard     .|.
              flag objectPermR      flag_read       .|.
              flag objectPermW      flag_write      .|.
              flag objectPermF      flag_fertile
      flag test fl = if test obj then 1 `shiftL` fl else 0 :: Int
  tellLn (decimal flags)

  tellLn (decimal $ objectOwner obj)

  let location = objectLocation obj
  tellLn (decimal $ objectForMaybe location)

  let contents = IS.toList (objectContents obj)
  tellLn (decimal $ objectForMaybe $ listToMaybe contents)
  tellLn (decimal $ nextLink objects oid objectContents location)

  let parent = objectParent obj
  tellLn (decimal $ objectForMaybe parent)

  let children = IS.toList (objectChildren obj)
  tellLn (decimal $ objectForMaybe $ listToMaybe children)
  tellLn (decimal $ nextLink objects oid objectChildren parent)

  verbs <- liftSTM $ mapM (readTVar . snd) $ objectVerbs obj
  tellLn (decimal $ length verbs)
  forM_ verbs $ \verb -> do
    tellLn (string2builder $ verbNames verb)
    tellLn (decimal $ verbOwner verb)

    let flags = flag verbPermR vf_read  .|.
                flag verbPermW vf_write .|.
                flag verbPermX vf_exec  .|.
                flag verbPermD vf_debug .|.
                objectArgs
        flag test fl = if test verb then fl else 0
        objectArgs = fromEnum (verbDirectObject   verb) `shiftL` dobjShift .|.
                     fromEnum (verbIndirectObject verb) `shiftL` iobjShift
    tellLn (decimal flags)

    tellLn (decimal $ fromEnum (verbPreposition verb) - 2)

  definedProperties <- liftSTM $ definedProperties obj
  tellLn (decimal $ length definedProperties)
  forM_ definedProperties $ tellLn . string2builder

  tellLn (decimal $ HM.size $ objectProperties obj)
  tellProperties objects obj (Just oid)

  return (oid, verbs)

tellObject _ (oid, Nothing) = do
  tell (singleton '#')
  tell (decimal oid)
  tellLn " recycled"

  return (oid, [])

nextLink :: Array ObjId (Maybe Object) -> ObjId ->
            (Object -> IntSet) -> Maybe ObjId -> ObjId
nextLink objects oid projection superior = next
  where nexts   = maybe [] (IS.toList . projection) $
                  join $ (objects !) <$> superior
        myIndex = elemIndex oid nexts
        next    = objectForMaybe $ listToMaybe $
                  maybe (const []) (drop . (+ 1)) myIndex nexts

tellProperties :: Array ObjId (Maybe Object) -> Object -> Maybe ObjId ->
                  DBWriter ()
tellProperties objects obj (Just oid) = do
  let Just definer = objects ! oid
  properties <- liftSTM $ definedProperties definer
  forM_ properties $ \propertyName -> do
    Just property <- liftSTM $ lookupProperty obj propertyName
    case propertyValue property of
      Nothing    -> tellLn (decimal type_clear)
      Just value -> tellValue value

    tellLn (decimal $ propertyOwner property)

    let flags = flag propertyPermR pf_read  .|.
                flag propertyPermW pf_write .|.
                flag propertyPermC pf_chown
        flag test fl = if test property then fl else 0
    tellLn (decimal flags)

  tellProperties objects obj (objectParent definer)

tellProperties _ _ Nothing = return ()

tellValue :: Value -> DBWriter ()
tellValue value = case value of
  Int x -> tellLn (decimal  type_int)   >> tellLn (decimal x)
  Flt x -> tellLn (decimal _type_float) >> tellLn (realFloat x)
  Str x -> tellLn (decimal _type_str)   >> tellLn (string2builder x)
  Obj x -> tellLn (decimal  type_obj)   >> tellLn (decimal x)
  Err x -> tellLn (decimal  type_err)   >> tellLn (decimal $ fromEnum x)
  Lst x -> tellLn (decimal _type_list)  >> tellLn (decimal $ V.length x) >>
           V.forM_ x tellValue

tellVerbs :: (ObjId, [Verb]) -> DBWriter ()
tellVerbs (oid, verbs) = forM_ (zip [0..] verbs) $ \(vnum, verb) -> do
  tell (singleton '#')
  tell (decimal oid)
  tell (singleton ':')
  tellLn $ decimal (vnum :: Int)

  tell (fromLazyText $ unparse True False $ verbProgram verb)
  tellLn (singleton '.')

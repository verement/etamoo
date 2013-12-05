
module MOO.Database.LambdaMOO ( loadLMDatabase ) where

import Control.Monad.Reader
import Text.Parsec
import Data.Word (Word)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Array
import Data.Bits
import System.IO (hFlush, stdout)

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.IntSet as IS

import MOO.Database
import MOO.Object
import MOO.Types
import MOO.Parser

loadLMDatabase :: FilePath -> IO (Either ParseError Database)
loadLMDatabase dbFile = do
  contents <- readFile dbFile
  runReaderT (runParserT lmDatabase initDatabase dbFile contents) initDBEnv

type DBParser = ParsecT String Database (ReaderT DBEnv IO)

data DBEnv = DBEnv {
  input_version :: Word
}

initDBEnv = DBEnv {
  input_version = undefined
}

header_format_string = ("** LambdaMOO Database, Format Version ", " **")

lmDatabase :: DBParser Database
lmDatabase = do
  let (before, after) = header_format_string
  dbVersion <- line $ between (string before) (string after) unsignedInt

  liftIO $ putStrLn $ "LambdaMOO database (version " ++ show dbVersion ++ ")"

  unless (dbVersion < num_db_versions) $
    fail $ "Unsupported database format (version " ++ show dbVersion ++ ")"

  local (\r -> r { input_version = dbVersion }) $ do
    nobjs  <- line signedInt
    nprogs <- line signedInt
    dummy  <- line signedInt
    nusers <- line signedInt

    liftIO $ putStrLn $ show nobjs ++ " objects, "
      ++ show nprogs ++ " programs, "
      ++ show nusers ++ " users"

    users <- count nusers read_objid
    installUsers users

    liftIO $ putStrLn $ "Players: " ++
      T.unpack (toLiteral $ Lst $ V.fromList $ sort $ map Obj users)

    liftIO $ putStrLn "Reading objects..."
    count nobjs read_object >>= installObjects

    liftIO $ putStrLn "Reading verb programs..."
    count nprogs program >>= installPrograms

    liftIO $ putStrLn "Reading forked and suspended tasks..."
    read_task_queue

    liftIO $ putStrLn "Reading list of formerly active connections..."
    read_active_connections

    eof
    liftIO $ putStrLn "Database loaded!"

  getState

installUsers :: [ObjId] -> DBParser ()
installUsers players = do
  db <- getState
  putState $ foldr (setPlayer True) db players

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
    verbName  :: String
  , verbOwner :: ObjId
  , verbPerms :: IntT
  , verbPrep  :: IntT
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

  objs <- liftIO $ mapM installProperties preObjs
  getState >>= liftIO . setObjects objs >>= putState

  where dbArray = listArray (0, length dbObjs - 1) $ map valid dbObjs
        preObjs = map (objectForDBObject dbArray) dbObjs

        checkObjId (objId, dbObj) =
          unless (objId == oid dbObj) $
            fail $ "Unexpected object #" ++ show (oid dbObj) ++
                   " (expecting #" ++ show objId ++ ")"

        installProperties (_  , Nothing)  = return Nothing
        installProperties (oid, Just obj) =
          let Just def = dbArray ! oid
              propvals = objPropvals def
          in fmap Just $ setProperties (mkProperties False oid propvals) obj

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
            propertyName      = T.pack propdef
          , propertyValue     = either (const Nothing) id $
                                valueFromVar (propVar propval)
          , propertyInherited = inherited
          , propertyOwner     = propOwner propval
          , propertyPermR     = (propPerms propval) .&. pf_read  /= 0
          , propertyPermW     = (propPerms propval) .&. pf_write /= 0
          , propertyPermC     = (propPerms propval) .&. pf_chown /= 0
        }

objectTrail :: Array ObjId (Maybe ObjectDef) -> ObjectDef ->
               (ObjectDef -> ObjId) -> (ObjectDef -> ObjId) -> [ObjId]
objectTrail arr def first rest = follow first rest (Just def)
  where follow f1 f2 Nothing = []
        follow f1 f2 (Just def)
          | inRange (bounds arr) idx = idx : follow f2 f2 (arr ! idx)
          | otherwise                = []
          where idx = f1 def

objectForDBObject :: Array ObjId (Maybe ObjectDef) ->
                     DBObject -> (ObjId, Maybe Object)
objectForDBObject dbArray dbObj = (oid dbObj, fmap mkObject $ valid dbObj)
  where mkObject def = initObject {
            parent           = maybeObject (objParent   def)
          , children         = IS.fromList $
                               objectTrail dbArray def objChild objSibling
          , objectName       = T.pack     $ objName     def
          , objectOwner      =              objOwner    def
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

installPrograms :: [(Int, Int, Program)] -> DBParser ()
installPrograms programs = return ()

unsignedInt :: DBParser Word
unsignedInt = fmap read $ many1 digit

signedInt :: DBParser Int
signedInt = fmap fromIntegral $ signed unsignedInt

signed :: (Num a) => DBParser a -> DBParser a
signed parser = negative <|> parser
  where negative = char '-' >> fmap negate parser

line :: DBParser a -> DBParser a
line parser = do
  x <- parser
  char '\n'
  return x

read_num :: DBParser IntT
read_num = line (signed (fmap read $ many1 digit)) <?> "num"

read_objid :: DBParser ObjId
read_objid = fmap fromIntegral read_num <?> "objid"

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

    liftIO $ putStrLn $ "  #" ++ show oid ++ " (" ++ name ++ ")"

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
      verbName  = name
    , verbOwner = owner
    , verbPerms = perms
    , verbPrep  = prep
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
valueFromVar (LMStr str)   = Right $ Just (Str $ T.pack str)
valueFromVar (LMObj obj)   = Right $ Just (Obj $ fromIntegral obj)
valueFromVar (LMErr err)   = Right $ Just (Err $ toEnum $ fromIntegral err)
valueFromVar (LMInt int)   = Right $ Just (Int int)
valueFromVar (LMFloat flt) = Right $ Just (Flt flt)
valueFromVar (LMList list) = do
  elems <- mapM valueFromVar list
  return $ Just (Lst $ V.fromList $ catMaybes elems)
valueFromVar var           = Left var

read_var :: DBParser LMVar
read_var = (<?> "var") $ do
  l <- read_num
  l <- if l == type_any
       then do input_version <- reader input_version
               return $ if input_version == dbv_prehistory
                        then type_none else l
       else return l

  cases l

  where
    cases l
      | l == type_clear   = return LMClear
      | l == type_none    = return LMNone
      | l == _type_str    = fmap   LMStr     read_string
      | l == type_obj     = fmap   LMObj     read_num
      | l == type_err     = fmap   LMErr     read_num
      | l == type_int     = fmap   LMInt     read_num
      | l == type_catch   = fmap   LMCatch   read_num
      | l == type_finally = fmap   LMFinally read_num
      | l == _type_float  = fmap   LMFloat   read_float
      | l == _type_list   =
        do l <- read_num
           fmap LMList $ count (fromIntegral l) read_var

    cases l = fail $ "Unknown type (" ++ show l ++ ")"

program :: DBParser (Int, Int, Program)
program = do
  char '#'
  oid <- signedInt
  char ':'
  vnum <- signedInt
  char '\n'

  let verbdesc = "#" ++ show oid ++ ":" ++ show vnum

  liftIO $ putStr ("  " ++ verbdesc ++ "     \r") >> hFlush stdout

  program <- read_program
  case program of
    Left  err  -> fail $ "Parse error in " ++ verbdesc ++ ": " ++ head err
    Right prog -> return (oid, vnum, prog)

read_program :: DBParser (Either [String] Program)
read_program = (<?> "program") $ do
  source <- try (string ".\n" >> return "") <|>
            manyTill anyToken (try $ string "\n.\n")
  return $ MOO.Parser.parse (T.pack source)

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
  input_version <- reader input_version
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
read_bi_func_data = do
  return ()

read_active_connections :: DBParser ()
read_active_connections = (<?> "active_connections") $ eof <|> do
  nconnections <- signedInt
  string " active connections"
  have_listeners <- (string " with listeners\n" >> return True) <|>
                    (char '\n' >> return False)

  count nconnections $ do
    (who, listener) <- if have_listeners
                       then do who <- signedInt
                               char ' '
                               listener <- signedInt
                               char '\n'
                               return (who, listener)
                       else do who <- read_num
                               return (fromIntegral who, system_object)
    return ()

  return ()

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

pf_read  = 01
pf_write = 02
pf_chown = 04

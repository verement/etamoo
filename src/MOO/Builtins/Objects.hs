
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins.Objects ( builtins ) where

import Control.Applicative ((<$>))
import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, writeTVar)
import Control.Monad (when, unless, void, forM_, foldM, join)
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Set (Set)
import Data.Text (Text)
import Prelude hiding (getContents)

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import MOO.AST
import MOO.Builtins.Common
import {-# SOURCE #-} MOO.Compiler
import MOO.Connection
import MOO.Database
import MOO.Object
import MOO.Parser
import MOO.Task
import MOO.Types
import MOO.Unparser
import MOO.Verb

import qualified MOO.List as Lst
import qualified MOO.String as Str

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- | § 4.4.3 Manipulating Objects
builtins :: [Builtin]
builtins = [
    -- § 4.4.3.1 Fundamental Operations on Objects
    bf_create
  , bf_chparent
  , bf_valid
  , bf_parent
  , bf_children
  , bf_recycle
  , bf_object_bytes
  , bf_max_object

    -- § 4.4.3.2 Object Movement
  , bf_move

    -- § 4.4.3.3 Operations on Properties
  , bf_properties
  , bf_property_info
  , bf_set_property_info
  , bf_add_property
  , bf_delete_property
  , bf_is_clear_property
  , bf_clear_property

    -- § 4.4.3.4 Operations on Verbs
  , bf_verbs
  , bf_verb_info
  , bf_set_verb_info
  , bf_verb_args
  , bf_set_verb_args
  , bf_add_verb
  , bf_delete_verb
  , bf_verb_code
  , bf_set_verb_code
  , bf_disassemble

    -- § 4.4.3.5 Operations on Player Objects
  , bf_players
  , bf_is_player
  , bf_set_player_flag
  ]

-- § 4.4.3.1 Fundamental Operations on Objects

modifyQuota :: ObjId -> (IntT -> MOO IntT) -> MOO ()
modifyQuota player f = do
  maybeQuota <- readProperty player ownershipQuota
  case maybeQuota of
    Just (Int quota) -> do
      quota' <- f quota
      writeProperty player ownershipQuota (Int quota')
    _ -> return ()

  where ownershipQuota = "ownership_quota"

bf_create = Builtin "create" 1 (Just 2)
            [TObj, TObj] TObj $ \(Obj parent : optional) -> do
  let (maybeOwner : _) = maybeDefaults optional

  maybeParent <- case parent of
    -1  -> return Nothing
    oid -> checkFertile oid >> return (Just oid)

  db <- getDatabase
  let newOid = maxObject db + 1

  ownerOid <- case maybeOwner of
    Nothing         -> frame permissions
    Just (Obj (-1)) -> checkWizard         >> return newOid
    Just (Obj oid)  -> checkPermission oid >> return oid

  modifyQuota ownerOid $ \quota ->
    if quota <= 0 then raise E_QUOTA else return (quota - 1)

  properties <- case maybeParent of
    Nothing  -> return $ objectProperties initObject
    Just oid -> do
      -- add to parent's set of children
      liftSTM $ modifyObject oid db $ addChild newOid

      -- properties inherited from parent
      Just parent <- getObject oid
      HM.fromList <$> mapM mkProperty (HM.toList $ objectProperties parent)

        where mkProperty :: (StrT, TVar Property) -> MOO (StrT, TVar Property)
              mkProperty (name, propTVar) = liftSTM $ do
                prop <- readTVar propTVar
                let prop' = prop {
                        propertyValue     = Nothing
                      , propertyInherited = True
                      , propertyOwner     = if propertyPermC prop then ownerOid
                                            else propertyOwner prop
                      }
                propTVar' <- newTVar prop'
                return (name, propTVar')

  let newObj = initObject {
          objectParent     = maybeParent
        , objectOwner      = ownerOid
        , objectProperties = properties
        }

  putDatabase =<< liftSTM (addObject newObj db)

  callFromFunc "create" 0 (newOid, "initialize") []
  return (Obj newOid)

reparentObject :: (ObjId, Object) -> (ObjId, Maybe Object) -> MOO ()
reparentObject (object, obj) (new_parent, maybeNewParent) = do
  -- Verify that neither object nor any of its descendants defines a property
  -- with the same name as one defined on new_parent or any of its ancestors
  case maybeNewParent of
    Just newParent -> do
      let props = HM.keys (objectProperties newParent)
      flip traverseDescendants object $ \obj -> forM_ props $ \propName -> do
        maybeProp <- liftSTM $ lookupProperty obj propName
        case maybeProp of
          Just prop | not (propertyInherited prop) -> raise E_INVARG
          _ -> return ()
    Nothing -> return ()

  -- Find the nearest ancestor that object and new_parent have in common
  oldAncestors <- ancestors object
  newAncestors <- case maybeNewParent of
    Just _  -> ancestors' new_parent
    Nothing -> return []
  let maybeCommon = findCommon oldAncestors newAncestors
      underCommon ancestors = maybe ancestors prefix maybeCommon
        where prefix common = takeWhile (/= common) ancestors

  -- Remove properties defined by ancestors of object under common, and add
  -- properties defined by new_parent or its ancestors under common
  db <- getDatabase
  oldProperties <- allDefinedProperties (underCommon oldAncestors)
  newProperties <- case maybeNewParent of
    Just newParent ->
      allDefinedProperties (underCommon newAncestors) >>=
      mapM (liftSTM . fmap fromJust . lookupProperty newParent)
    Nothing -> return []

  flip (modifyDescendants db) object $ \obj -> do
    obj' <- foldM (flip deleteProperty) obj oldProperties
    foldM (flip addInheritedProperty) obj' newProperties

  -- Update the parent/child hierarchy
  liftSTM $ modifyObject object db $ \obj ->
    return obj { objectParent = const new_parent <$> maybeNewParent }
  case objectParent obj of
    Just parentOid -> liftSTM $ modifyObject parentOid db $ deleteChild object
    Nothing        -> return ()
  case maybeNewParent of
    Just _  -> liftSTM $ modifyObject new_parent db $ addChild object
    Nothing -> return ()

  where ancestors :: ObjId -> MOO [ObjId]
        ancestors oid = do
          maybeObject <- getObject oid
          case join $ objectParent <$> maybeObject of
            Just parent -> do
              ancestors <- ancestors parent
              return (parent : ancestors)
            Nothing -> return []

        ancestors' :: ObjId -> MOO [ObjId]
        ancestors' oid = (oid :) <$> ancestors oid

        findCommon :: [ObjId] -> [ObjId] -> Maybe ObjId
        findCommon xs ys = findCommon' (reverse xs) (reverse ys) Nothing
        findCommon' (x:xs) (y:ys) _
          | x == y = findCommon' xs ys (Just x)
        findCommon' _ _ common = common

        allDefinedProperties :: [ObjId] -> MOO [StrT]
        allDefinedProperties = fmap ($ []) . foldM concatProps id
          where concatProps acc oid = do
                  Just obj <- getObject oid
                  props <- liftSTM $ definedProperties obj
                  return (acc props ++)

bf_chparent = Builtin "chparent" 2 (Just 2)
              [TObj, TObj] TAny $ \[Obj object, Obj new_parent] -> do
  obj <- checkValid object
  maybeNewParent <- case new_parent of
    -1  -> return Nothing
    oid -> do
      newParent <- checkValid oid
      checkFertile oid
      return (Just newParent)
  checkPermission (objectOwner obj)
  checkRecurrence objectParent object new_parent

  reparentObject (object, obj) (new_parent, maybeNewParent)

  return zero

bf_valid = Builtin "valid" 1 (Just 1) [TObj] TInt $ \[Obj object] ->
  truthValue . isJust <$> getObject object

bf_parent = Builtin "parent" 1 (Just 1) [TObj] TObj $ \[Obj object] ->
  Obj . getParent <$> checkValid object

bf_children = Builtin "children" 1 (Just 1) [TObj] TLst $ \[Obj object] ->
  objectList . getChildren <$> checkValid object

bf_recycle = Builtin "recycle" 1 (Just 1) [TObj] TAny $ \[Obj object] -> do
  obj <- checkValid object
  let owner = objectOwner obj
  checkPermission owner

  callFromFunc "recycle" 0 (object, "recycle") []

  moveContentsToNothing object
  moveToNothing object

  reparentChildren object (objectParent obj)
  reparent object Nothing

  setPlayerFlag True object False
  getDatabase >>= liftSTM . deleteObject object

  modifyQuota owner $ return . (+ 1)

  return zero

  where moveContentsToNothing :: ObjId -> MOO ()
        moveContentsToNothing object = do
          maybeObj <- getObject object
          case getContents <$> maybeObj of
            Just (oid:_) -> do
              moveToNothing oid
              moveContentsToNothing object
            _ -> return ()

        moveToNothing :: ObjId -> MOO ()
        moveToNothing oid = moveObject "recycle" oid nothing

        reparentChildren :: ObjId -> Maybe ObjId -> MOO ()
        reparentChildren object maybeParent = do
          maybeObj <- getObject object
          case getChildren <$> maybeObj of
            Just (oid:_) -> do
              reparent oid maybeParent
              reparentChildren object maybeParent
            _ -> return ()

        reparent :: ObjId -> Maybe ObjId -> MOO ()
        reparent object maybeParent = do
          maybeObj <- getObject object
          case maybeObj of
            Just obj -> do
              newParent <- case maybeParent of
                Just parentOid -> do
                  parent <- getObject parentOid
                  return (parentOid, parent)
                Nothing -> return (nothing, Nothing)
              reparentObject (object, obj) newParent
            Nothing -> return ()

bf_object_bytes = Builtin "object_bytes" 1 (Just 1)
                  [TObj] TInt $ \[Obj object] -> do
  checkWizard
  obj <- checkValid object

  propertyBytes <- fmap storageBytes $ liftSTM $
                   mapM readTVar $ HM.elems (objectProperties obj)
  verbBytes     <- fmap storageBytes $ liftSTM $
                   mapM (readTVar . snd) $ objectVerbs obj

  return $ Int $ fromIntegral $ storageBytes obj + propertyBytes + verbBytes

bf_max_object = Builtin "max_object" 0 (Just 0) [] TObj $ \[] ->
  Obj . maxObject <$> getDatabase

-- § 4.4.3.2 Object Movement

moveObject :: StrT -> ObjId -> ObjId -> MOO ()
moveObject funcName what where_ = do
  let newWhere = case where_ of
        -1  -> Nothing
        oid -> Just oid

  maybeWhat <- getObject what
  case maybeWhat of
    Nothing      -> return ()
    Just whatObj -> unless (objectLocation whatObj == newWhere) $ do
      maybeWhere <- getObject where_
      when (isNothing newWhere || isJust maybeWhere) $ do
        checkRecurrence objectLocation what where_

        let oldWhere = objectLocation whatObj
        db <- getDatabase

        liftSTM $ modifyObject what db $ \obj ->
          return obj { objectLocation = newWhere }
        case oldWhere of
          Nothing        -> return ()
          Just oldWhere' -> liftSTM $ modifyObject oldWhere' db $
                            deleteContent what
        case newWhere of
          Nothing        -> return ()
          Just newWhere' -> liftSTM $ modifyObject newWhere' db $
                            addContent what
        case oldWhere of
          Nothing        -> return ()
          Just oldWhere' ->
            void $ callFromFunc funcName 1 (oldWhere', "exitfunc") [Obj what]

        maybeWhat <- getObject what
        case maybeWhat of
          Nothing      -> return ()
          Just whatObj ->
            when (objectLocation whatObj == newWhere) $
            void $ callFromFunc funcName 2 (where_, "enterfunc") [Obj what]

bf_move = Builtin "move" 2 (Just 2)
          [TObj, TObj] TAny $ \[Obj what, Obj where_] -> do
  what' <- checkValid what
  where' <- case where_ of
    -1  -> return Nothing
    oid -> Just <$> checkValid oid
  checkPermission (objectOwner what')

  when (isJust where') $ do
    accepted <- maybe False truthOf <$>
                callFromFunc "move" 0 (where_, "accept") [Obj what]
    unless accepted $ do
      wizard <- isWizard =<< frame permissions
      unless wizard $ raise E_NACC

  moveObject "move" what where_

  return zero

-- § 4.4.3.3 Operations on Properties

bf_properties = Builtin "properties" 1 (Just 1)
                [TObj] TLst $ \[Obj object] -> do
  obj <- checkValid object
  unless (objectPermR obj) $ checkPermission (objectOwner obj)

  stringList <$> liftSTM (definedProperties obj)

bf_property_info = Builtin "property_info" 2 (Just 2)
                   [TObj, TStr] TLst $ \[Obj object, Str prop_name] -> do
  obj <- checkValid object
  prop <- getProperty obj prop_name
  unless (propertyPermR prop) $ checkPermission (propertyOwner prop)

  return $ fromList [Obj $ propertyOwner prop, Str $ perms prop]

  where perms prop = Str.fromString $ concat [['r' | propertyPermR prop],
                                              ['w' | propertyPermW prop],
                                              ['c' | propertyPermC prop]]

traverseDescendants :: (Object -> MOO a) -> ObjId -> MOO ()
traverseDescendants f oid = do
  Just obj <- getObject oid
  f obj
  mapM_ (traverseDescendants f) $ getChildren obj

modifyDescendants :: Database -> (Object -> STM Object) -> ObjId -> MOO ()
modifyDescendants db f oid = do
  liftSTM $ modifyObject oid db f
  Just obj <- getObject oid
  mapM_ (modifyDescendants db f) $ getChildren obj

{-# ANN module ("HLint: ignore Use String" :: String) #-}

checkPerms :: [Char] -> StrT -> MOO (Set Char)
checkPerms valid perms = do
  let permSet = S.fromList (T.unpack $ Str.toCaseFold perms)
  unless (S.null $ permSet `S.difference` S.fromList valid) $ raise E_INVARG
  return permSet

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

bf_set_property_info = Builtin "set_property_info" 3 (Just 3)
                       [TObj, TStr, TLst]
                       TAny $ \[Obj object, Str prop_name, Lst info] -> do
  (owner, perms, new_name) <- case Lst.toList info of
    [Obj owner, Str perms]               -> return (owner, perms, Nothing)
    [_        , _        ]               -> raise E_TYPE
    [Obj owner, Str perms, Str new_name] -> return (owner, perms, Just new_name)
    [_        , _        , _           ] -> raise E_TYPE
    _                                    -> raise E_INVARG
  permSet <- checkPerms "rwc" perms
  checkValid owner

  obj <- checkValid object
  prop <- getProperty obj prop_name
  unless (propertyPermW prop) $ checkPermission (propertyOwner prop)
  checkPermission owner

  let setInfo = modifyProperty obj prop_name $ \prop ->
        return prop {
            propertyOwner = owner
          , propertyPermR = 'r' `S.member` permSet
          , propertyPermW = 'w' `S.member` permSet
          , propertyPermC = 'c' `S.member` permSet
        }

  case new_name of
    Nothing      -> setInfo
    Just newName -> do
      let oldName = prop_name

      unless (objectPermW obj) $ checkPermission (objectOwner obj)

      when (propertyInherited prop) $ raise E_INVARG
      unless (newName == oldName) $ flip traverseDescendants object $ \obj ->
        when (isJust $ lookupPropertyRef obj newName) $ raise E_INVARG

      setInfo

      db <- getDatabase
      flip (modifyDescendants db) object $ \obj -> do
        let Just propTVar = lookupPropertyRef obj oldName
        prop <- readTVar propTVar
        writeTVar propTVar $ prop { propertyName = newName }

        return obj { objectProperties =
                        HM.insert newName propTVar $
                        HM.delete oldName (objectProperties obj) }

  return zero

bf_add_property = Builtin "add_property" 4 (Just 4) [TObj, TStr, TAny, TLst]
                  TAny $ \[Obj object, Str prop_name, value, Lst info] -> do
  (owner, perms) <- case Lst.toList info of
    [Obj owner, Str perms] -> return (owner, perms)
    [_        , _        ] -> raise E_TYPE
    _                      -> raise E_INVARG
  permSet <- checkPerms "rwc" perms
  checkValid owner

  obj <- checkValid object
  unless (objectPermW obj) $ checkPermission (objectOwner obj)
  checkPermission owner

  when (isBuiltinProperty prop_name) $ raise E_INVARG
  flip traverseDescendants object $ \obj ->
    when (isJust $ lookupPropertyRef obj prop_name) $ raise E_INVARG

  let newProperty = initProperty {
          propertyName      = prop_name
        , propertyValue     = Just value
        , propertyInherited = False
        , propertyOwner     = owner
        , propertyPermR     = 'r' `S.member` permSet
        , propertyPermW     = 'w' `S.member` permSet
        , propertyPermC     = 'c' `S.member` permSet
        }

  db <- getDatabase
  liftSTM $ modifyObject object db (addProperty newProperty)
  forM_ (getChildren obj) $
    modifyDescendants db $ addInheritedProperty newProperty

  return zero

bf_delete_property = Builtin "delete_property" 2 (Just 2)
                     [TObj, TStr] TAny $ \[Obj object, Str prop_name] -> do
  obj <- checkValid object
  unless (objectPermW obj) $ checkPermission (objectOwner obj)
  prop <- getProperty obj prop_name
  when (propertyInherited prop) $ raise E_PROPNF

  db <- getDatabase
  flip (modifyDescendants db) object $ \obj ->
    return obj { objectProperties = HM.delete prop_name (objectProperties obj) }

  return zero

bf_is_clear_property = Builtin "is_clear_property" 2 (Just 2)
                       [TObj, TStr] TInt $ \[Obj object, Str prop_name] -> do
  obj <- checkValid object
  if isBuiltinProperty prop_name
    then return $ truthValue False
    else do
      prop <- getProperty obj prop_name
      unless (propertyPermR prop) $ checkPermission (propertyOwner prop)

      return (truthValue $ isNothing $ propertyValue prop)

bf_clear_property = Builtin "clear_property" 2 (Just 2)
                    [TObj, TStr] TAny $ \[Obj object, Str prop_name] -> do
  obj <- checkValid object
  if isBuiltinProperty prop_name
    then raise E_PERM
    else do
      modifyProperty obj prop_name $ \prop -> do
        unless (propertyPermW prop) $ checkPermission (propertyOwner prop)
        unless (propertyInherited prop) $ raise E_INVARG
        return prop { propertyValue = Nothing }

      return zero

-- § 4.4.3.4 Operations on Verbs

bf_verbs = Builtin "verbs" 1 (Just 1) [TObj] TLst $ \[Obj object] -> do
  obj <- checkValid object
  unless (objectPermR obj) $ checkPermission (objectOwner obj)

  stringList <$> liftSTM (definedVerbs obj)

bf_verb_info = Builtin "verb_info" 2 (Just 2)
               [TObj, TAny] TLst $ \[Obj object, verb_desc] -> do
  obj <- checkValid object
  verb <- getVerb obj verb_desc
  unless (verbPermR verb) $ checkPermission (verbOwner verb)

  return $ fromList
    [Obj $ verbOwner verb, Str $ perms verb, Str $ verbNames verb]

  where perms verb = Str.fromString $ concat [['r' | verbPermR verb],
                                              ['w' | verbPermW verb],
                                              ['x' | verbPermX verb],
                                              ['d' | verbPermD verb]]

verbInfo :: LstT -> MOO (ObjId, Set Char, StrT)
verbInfo info = do
  (owner, perms, names) <- case Lst.toList info of
    [Obj owner, Str perms, Str names] -> return (owner, perms, names)
    [_        , _        , _        ] -> raise E_TYPE
    _                                 -> raise E_INVARG
  permSet <- checkPerms "rwxd" perms
  checkValid owner
  when (null $ Str.words names) $ raise E_INVARG

  return (owner, permSet, names)

bf_set_verb_info = Builtin "set_verb_info" 3 (Just 3) [TObj, TAny, TLst]
                   TAny $ \[Obj object, verb_desc, Lst info] -> do
  (owner, permSet, names) <- verbInfo info

  obj <- checkValid object
  verb <- getVerb obj verb_desc
  unless (verbPermW verb) $ checkPermission (verbOwner verb)
  checkPermission owner

  unless (names == verbNames verb || objectPermW obj) $
    checkPermission (objectOwner obj)

  modifyVerb (object, obj) verb_desc $ \verb ->
    return verb {
        verbNames = names
      , verbOwner = owner
      , verbPermR = 'r' `S.member` permSet
      , verbPermW = 'w' `S.member` permSet
      , verbPermX = 'x' `S.member` permSet
      , verbPermD = 'd' `S.member` permSet
    }

  return zero

bf_verb_args = Builtin "verb_args" 2 (Just 2)
               [TObj, TAny] TLst $ \[Obj object, verb_desc] -> do
  obj <- checkValid object
  verb <- getVerb obj verb_desc
  unless (verbPermR verb) $ checkPermission (verbOwner verb)

  return $ stringList [dobj verb, prep verb, iobj verb]

  where dobj = obj2string  . verbDirectObject
        iobj = obj2string  . verbIndirectObject
        prep = prep2string . verbPreposition

verbArgs :: LstT -> MOO (ObjSpec, PrepSpec, ObjSpec)
verbArgs args = do
  (dobj, prep, iobj) <- case Lst.toList args of
    [Str dobj, Str prep, Str iobj] -> return (dobj, breakSlash prep, iobj)
      where breakSlash = fst . Str.breakOn "/"
    [_       , _       , _       ] -> raise E_TYPE
    _                              -> raise E_INVARG
  dobj' <- maybe (raise E_INVARG) return $ string2obj  dobj
  prep' <- maybe (raise E_INVARG) return $ string2prep prep
  iobj' <- maybe (raise E_INVARG) return $ string2obj  iobj

  return (dobj', prep', iobj')

bf_set_verb_args = Builtin "set_verb_args" 3 (Just 3) [TObj, TAny, TLst]
                   TAny $ \[Obj object, verb_desc, Lst args] -> do
  (dobj, prep, iobj) <- verbArgs args

  obj <- checkValid object
  verb <- getVerb obj verb_desc
  unless (verbPermW verb) $ checkPermission (verbOwner verb)

  modifyVerb (object, obj) verb_desc $ \verb ->
    return verb {
        verbDirectObject   = dobj
      , verbPreposition    = prep
      , verbIndirectObject = iobj
    }

  return zero

bf_add_verb = Builtin "add_verb" 3 (Just 3)
              [TObj, TLst, TLst] TInt $ \[Obj object, Lst info, Lst args] -> do
  (owner, permSet, names) <- verbInfo info
  (dobj, prep, iobj)      <- verbArgs args

  obj <- checkValid object
  unless (objectPermW obj) $ checkPermission (objectOwner obj)
  checkPermission owner

  let definedVerb = initVerb {
          verbNames          = names
        , verbOwner          = owner
        , verbPermR          = 'r' `S.member` permSet
        , verbPermW          = 'w' `S.member` permSet
        , verbPermX          = 'x' `S.member` permSet
        , verbPermD          = 'd' `S.member` permSet
        , verbDirectObject   = dobj
        , verbPreposition    = prep
        , verbIndirectObject = iobj
      }

  db <- getDatabase
  liftSTM $ modifyObject object db $ addVerb definedVerb

  return $ Int $ fromIntegral $ length (objectVerbs obj) + 1

bf_delete_verb = Builtin "delete_verb" 2 (Just 2)
                 [TObj, TAny] TAny $ \[Obj object, verb_desc] -> do
  obj <- checkValid object
  getVerb obj verb_desc
  unless (objectPermW obj) $ checkPermission (objectOwner obj)

  numericStrings <- serverOption supportNumericVerbnameStrings
  case lookupVerbRef numericStrings obj verb_desc of
    Nothing         -> raise E_VERBNF
    Just (index, _) -> do
      db <- getDatabase
      liftSTM $ modifyObject object db $ deleteVerb index

  return zero

bf_verb_code = Builtin "verb_code" 2 (Just 4) [TObj, TAny, TAny, TAny]
               TLst $ \(Obj object : verb_desc : optional) -> do
  let [fully_paren, indent] = booleanDefaults optional [False, True]

  obj <- checkValid object
  verb <- getVerb obj verb_desc
  unless (verbPermR verb) $ checkPermission (verbOwner verb)

  let code = init $ Str.splitOn "\n" $ Str.fromText $ TL.toStrict $
             unparse fully_paren indent (verbProgram verb)
  return (stringList code)

bf_set_verb_code = Builtin "set_verb_code" 3 (Just 3) [TObj, TAny, TLst]
                   TLst $ \[Obj object, verb_desc, Lst code] -> do
  obj <- checkValid object
  verb <- getVerb obj verb_desc
  text <- T.concat . ($ []) <$> Lst.foldM addLine id code
  unless (verbPermW verb) $ checkPermission (verbOwner verb)
  checkProgrammer

  case parseProgram text of
    Left errors   -> return $ fromListBy (Str . Str.fromString) errors
    Right program -> do
      modifyVerb (object, obj) verb_desc $ \verb ->
        return verb {
            verbProgram = program
          , verbCode    = compile program
        }
      return emptyList

  where addLine :: ([Text] -> [Text]) -> Value -> MOO ([Text] -> [Text])
        addLine add (Str line) = return (add [Str.toText line, "\n"] ++)
        addLine _    _         = raise E_TYPE

bf_disassemble = Builtin "disassemble" 2 (Just 2)
                 [TObj, TAny] TLst $ \[Obj object, verb_desc] -> do
  obj <- checkValid object
  verb <- getVerb obj verb_desc
  unless (verbPermR verb) $ checkPermission (verbOwner verb)

  let Program statements = verbProgram verb
  return $ fromListBy (Str . Str.fromString . show) statements

-- § 4.4.3.5 Operations on Player Objects

bf_players = Builtin "players" 0 (Just 0) [] TLst $ \[] ->
  objectList . allPlayers <$> getDatabase

bf_is_player = Builtin "is_player" 1 (Just 1) [TObj] TInt $ \[Obj object] ->
  truthValue . objectIsPlayer <$> checkValid object

setPlayerFlag :: Bool -> ObjId -> Bool -> MOO ()
setPlayerFlag recycled object isPlayer = do
  db <- getDatabase
  liftSTM $ modifyObject object db $ \obj ->
    return obj { objectIsPlayer = isPlayer }
  putDatabase $ setPlayer isPlayer object db

  unless isPlayer $ bootPlayer' recycled object

bf_set_player_flag = Builtin "set_player_flag" 2 (Just 2)
                     [TObj, TAny] TAny $ \[Obj object, value] -> do
  checkValid object
  checkWizard

  setPlayerFlag False object (truthOf value)

  return zero

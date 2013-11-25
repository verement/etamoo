
{-# LANGUAGE OverloadedStrings #-}

module MOO.Object ( Object
                  ) where

import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
import qualified Data.Vector as V
import           Data.Text (Text)
import qualified Data.Text as T
import           MOO.Types
import           MOO.Verb

data Object = Object {
    objNumber  :: ObjId
  , isPlayer   :: Bool
  , parent     :: Maybe Object
  , children   :: [Object]
  , properties :: HashMap Text Property
  , verbs      :: [Verb]
  }

instance Eq Object where
  Object { objNumber = id1 } == Object { objNumber = id2 } = id1 == id2

rootObject :: Object
rootObject = Object {
    objNumber  = (-1)
  , isPlayer   = False
  , parent     = Nothing
  , children   = []
  , properties = builtinProperties
  , verbs      = []
  }

data Property = Property {
    propName      :: Text
  , propValue     :: Maybe Value
  , propInherited :: Bool
  , propOwner     :: Object
  , propPermR     :: Bool
  , propPermW     :: Bool
  , propPermC     :: Bool
  }

builtinProperties :: HashMap Text Property
builtinProperties = HM.fromList $ map prop [
    ("name"      , Str "")
  , ("owner"     , Obj (-1))
  , ("location"  , Obj (-1))
  , ("contents"  , List V.empty)
  , ("programmer", Int 0)
  , ("wizard"    , Int 0)
  , ("r"         , Int 0)
  , ("w"         , Int 0)
  , ("f"         , Int 0)
  ]
  where prop (n, v) = (n, Property n (Just v) False rootObject True False False)

getProperty :: Object -> Text -> Maybe Property
getProperty obj pn = HM.lookup (T.toCaseFold pn) (properties obj)

getPropValue :: Object -> Text -> Maybe Value
getPropValue obj pn = get (T.toCaseFold pn) obj
  where get pn obj = do
          p <- HM.lookup pn $ properties obj
          case propValue p of
            Just v  -> return v
            Nothing -> parent obj >>= get pn

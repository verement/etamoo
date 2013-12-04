
{-# LANGUAGE OverloadedStrings #-}

module MOO.Verb ( Verb ) where

import Data.Text
import {-# SOURCE #-} MOO.Object

data Verb = Verb {
    name           :: Text
  , owner          :: Object
  , permissionR    :: Bool
  , permissionW    :: Bool
  , permissionX    :: Bool
  , permissionD    :: Bool
  , directObject   :: ObjSpec
  , preposition    :: PrepSpec
  , indirectObject :: ObjSpec
} deriving Show

data ObjSpec = ObjThis | ObjAny | ObjNone
             deriving Show

data PrepSpec = PrepNone | PrepAny
              | PrepWith_Using
              | PrepAt_To
              | PrepInFrontOf
              | PrepIn_Inside_Into
              | PrepOnTopOf_On_Onto_Upon
              | PrepOutOf_FromInside_From
              | PrepOver
              | PrepThrough
              | PrepUnder_Underneath_Beneath
              | PrepBehind
              | PrepBeside
              | PrepFor_About
              | PrepIs
              | PrepAs
              | PrepOff_OffOf
              deriving Show

prep2text :: PrepSpec -> Text
prep2text PrepNone                     = "none"
prep2text PrepAny                      = "any"
prep2text PrepWith_Using               = "with/using"
prep2text PrepAt_To                    = "at/to"
prep2text PrepInFrontOf                = "in front of"
prep2text PrepIn_Inside_Into           = "in/inside/into"
prep2text PrepOnTopOf_On_Onto_Upon     = "on top of/on/onto/upon"
prep2text PrepOutOf_FromInside_From    = "out of/from inside/from"
prep2text PrepOver                     = "over"
prep2text PrepThrough                  = "through"
prep2text PrepUnder_Underneath_Beneath = "under/underneath/beneath"
prep2text PrepBehind                   = "behind"
prep2text PrepBeside                   = "beside"
prep2text PrepFor_About                = "for/about"
prep2text PrepIs                       = "is"
prep2text PrepAs                       = "as"
prep2text PrepOff_OffOf                = "off/off of"

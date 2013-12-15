
{-# LANGUAGE OverloadedStrings #-}

module MOO.Verb ( Verb (..)
                , initVerb
                ) where

import Data.Text (Text)

import MOO.Types
import MOO.AST

data Verb = Verb {
    verbNames          :: StrT
  , verbProgram        :: Program

  , verbOwner          :: ObjId
  , verbPermR          :: Bool
  , verbPermW          :: Bool
  , verbPermX          :: Bool
  , verbPermD          :: Bool

  , verbDirectObject   :: ObjSpec
  , verbPreposition    :: PrepSpec
  , verbIndirectObject :: ObjSpec
} deriving Show

initVerb = Verb {
    verbNames          = ""
  , verbProgram        = Program []

  , verbOwner          = -1
  , verbPermR          = False
  , verbPermW          = False
  , verbPermX          = False
  , verbPermD          = False

  , verbDirectObject   = ObjNone
  , verbPreposition    = PrepNone
  , verbIndirectObject = ObjNone
}

data ObjSpec = ObjNone
             | ObjAny
             | ObjThis
             deriving (Enum, Show)

data PrepSpec = PrepAny
              | PrepNone
              | PrepWithUsing
              | PrepAtTo
              | PrepInfrontof
              | PrepInInsideInto
              | PrepOntopofOnOntoUpon
              | PrepOutofFrominsideFrom
              | PrepOver
              | PrepThrough
              | PrepUnderUnderneathBeneath
              | PrepBehind
              | PrepBeside
              | PrepForAbout
              | PrepIs
              | PrepAs
              | PrepOffOffof
              deriving (Enum, Show)

prep2text :: PrepSpec -> Text
prep2text PrepAny                    = "any"
prep2text PrepNone                   = "none"
prep2text PrepWithUsing              = "with/using"
prep2text PrepAtTo                   = "at/to"
prep2text PrepInfrontof              = "in front of"
prep2text PrepInInsideInto           = "in/inside/into"
prep2text PrepOntopofOnOntoUpon      = "on top of/on/onto/upon"
prep2text PrepOutofFrominsideFrom    = "out of/from inside/from"
prep2text PrepOver                   = "over"
prep2text PrepThrough                = "through"
prep2text PrepUnderUnderneathBeneath = "under/underneath/beneath"
prep2text PrepBehind                 = "behind"
prep2text PrepBeside                 = "beside"
prep2text PrepForAbout               = "for/about"
prep2text PrepIs                     = "is"
prep2text PrepAs                     = "as"
prep2text PrepOffOffof               = "off/off of"


{-# LANGUAGE OverloadedStrings #-}

module MOO.Verb ( Verb(..)
                , ObjSpec(..)
                , PrepSpec(..)
                , initVerb
                , obj2text
                , text2obj
                , objMatch
                , prep2text
                , text2prep
                , prepMatch
                , prepPhrases
                , verbNameMatch
                ) where

import Data.Text (Text)

import qualified Data.Text as T

import MOO.Types
import MOO.AST
import {-# SOURCE #-} MOO.Task

data Verb = Verb {
    verbNames          :: StrT
  , verbProgram        :: Program
  , verbCode           :: MOO Value

  , verbOwner          :: ObjId
  , verbPermR          :: Bool
  , verbPermW          :: Bool
  , verbPermX          :: Bool
  , verbPermD          :: Bool

  , verbDirectObject   :: ObjSpec
  , verbPreposition    :: PrepSpec
  , verbIndirectObject :: ObjSpec
}

instance Sizeable Verb where
  storageBytes verb =
    storageBytes (verbNames          verb) +
    storageBytes (verbProgram        verb) * 2 +
    -- storageBytes (verbCode           verb) +
    storageBytes (verbOwner          verb) +
    storageBytes (verbPermR          verb) +
    storageBytes (verbPermW          verb) +
    storageBytes (verbPermX          verb) +
    storageBytes (verbPermD          verb) +
    storageBytes (verbDirectObject   verb) +
    storageBytes (verbPreposition    verb) +
    storageBytes (verbIndirectObject verb)

initVerb = Verb {
    verbNames          = ""
  , verbProgram        = Program []
  , verbCode           = return nothing

  , verbOwner          = -1
  , verbPermR          = False
  , verbPermW          = False
  , verbPermX          = False
  , verbPermD          = False

  , verbDirectObject   = ObjNone
  , verbPreposition    = PrepNone
  , verbIndirectObject = ObjNone
}

-- | Argument (direct/indirect object) specifier
data ObjSpec = ObjNone  -- ^ none
             | ObjAny   -- ^ any
             | ObjThis  -- ^ this
             deriving (Enum, Bounded, Show)

instance Sizeable ObjSpec where
  storageBytes _ = storageBytes ()

obj2text :: ObjSpec -> Text
obj2text ObjNone = "none"
obj2text ObjAny  = "any"
obj2text ObjThis = "this"

text2obj :: Text -> Maybe ObjSpec
text2obj = flip lookup $ map mkAssoc [minBound ..]
  where mkAssoc objSpec = (obj2text objSpec, objSpec)

objMatch :: ObjId -> ObjSpec -> ObjId -> Bool
objMatch _    ObjNone (-1) = True
objMatch _    ObjNone _    = False
objMatch _    ObjAny  _    = True
objMatch this ObjThis oid  = oid == this

-- | Preposition specifier
data PrepSpec = PrepAny                     -- ^ any
              | PrepNone                    -- ^ none
              | PrepWithUsing               -- ^ with\/using
              | PrepAtTo                    -- ^ at\/to
              | PrepInfrontof               -- ^ in front of
              | PrepInInsideInto            -- ^ in\/inside\/into
              | PrepOntopofOnOntoUpon       -- ^ on top of\/on\/onto\/upon
              | PrepOutofFrominsideFrom     -- ^ out of\/from inside\/from
              | PrepOver                    -- ^ over
              | PrepThrough                 -- ^ through
              | PrepUnderUnderneathBeneath  -- ^ under\/underneath\/beneath
              | PrepBehind                  -- ^ behind
              | PrepBeside                  -- ^ beside
              | PrepForAbout                -- ^ for\/about
              | PrepIs                      -- ^ is
              | PrepAs                      -- ^ as
              | PrepOffOffof                -- ^ off\/off of
              deriving (Enum, Bounded, Eq, Show)

instance Sizeable PrepSpec where
  storageBytes _ = storageBytes ()

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

text2prep :: Text -> Maybe PrepSpec
text2prep = flip lookup $ concatMap mkAssoc [minBound ..]
  where mkAssoc prepSpec =
          [ (prep, prepSpec) | prep <- T.splitOn "/" $ prep2text prepSpec ] ++
          [ (T.pack $ show index, prepSpec)
          | let index = fromEnum prepSpec - fromEnum (succ PrepNone)
          , index >= 0
          ]

prepMatch :: PrepSpec -> PrepSpec -> Bool
prepMatch PrepAny _  = True
prepMatch vp      cp = vp == cp

prepPhrases :: [(PrepSpec, [Text])]
prepPhrases = [ (prepSpec, T.words prepPhrase)
              | prepSpec   <- [succ PrepNone ..]
              , prepPhrase <- T.splitOn "/" $ prep2text prepSpec
              ]

-- | Does the given verb name match any of the given aliases? Each alias may
-- use @*@ to separate required and optional text to match.
verbNameMatch :: StrT -> [StrT] -> Bool
verbNameMatch name = any matchName
  where matchName vname
          | post == ""  = name     == vname
          | post == "*" = preName  == pre
          | otherwise   = preName  == pre &&
                          postName == T.take (T.length postName) (T.tail post)
          where (pre, post)         = T.breakOn "*" vname
                (preName, postName) = T.splitAt (T.length pre) name

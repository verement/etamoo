
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module MOO.Verb ( Verb(..)
                , ObjSpec(..)
                , PrepSpec(..)
                , initVerb
                , obj2string
                , string2obj
                , objMatch
                , prep2string
                , string2prep
                , prepMatch
                , prepPhrases
                , verbNameMatch
                ) where

import Control.Applicative ((<$>), (<*>))
import Data.Typeable (Typeable)
import Database.VCache (VCacheable(put, get))

import MOO.AST
import {-# SOURCE #-} MOO.Object (nothing)
import MOO.Types

import qualified MOO.String as Str

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
} deriving Typeable

instance VCacheable Verb where
  put verb = do
    put $ verbNames          verb
    put $ verbProgram        verb
    put $ verbOwner          verb
    put $ verbPermR          verb
    put $ verbPermW          verb
    put $ verbPermX          verb
    put $ verbPermD          verb
    put $ verbDirectObject   verb
    put $ verbPreposition    verb
    put $ verbIndirectObject verb

  get = Verb <$> get <*> get
             <*> get <*> get <*> get <*> get <*> get
             <*> get <*> get <*> get

instance Sizeable Verb where
  storageBytes verb =
    storageBytes (verbNames          verb) +
    storageBytes (verbProgram        verb) * 2 +
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

  , verbOwner          = nothing
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
             deriving (Enum, Bounded, Typeable)

instance VCacheable ObjSpec where
  put = put . fromEnum
  get = toEnum <$> get

instance Sizeable ObjSpec where
  storageBytes _ = storageBytes ()

obj2string :: ObjSpec -> StrT
obj2string ObjNone = "none"
obj2string ObjAny  = "any"
obj2string ObjThis = "this"

string2obj :: StrT -> Maybe ObjSpec
string2obj = flip lookup $ map mkAssoc [minBound ..]
  where mkAssoc :: ObjSpec -> (StrT, ObjSpec)
        mkAssoc objSpec = (obj2string objSpec, objSpec)

objMatch :: ObjId -> ObjSpec -> ObjId -> Bool
objMatch _    ObjNone oid = oid == nothing
objMatch _    ObjAny  _   = True
objMatch this ObjThis oid = oid == this

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
              | PrepOffofOff                -- ^ off of\/off
              deriving (Enum, Bounded, Eq, Typeable)

instance VCacheable PrepSpec where
  put = put . fromEnum
  get = toEnum <$> get

instance Sizeable PrepSpec where
  storageBytes _ = storageBytes ()

prep2string :: PrepSpec -> StrT
prep2string PrepAny                    = "any"
prep2string PrepNone                   = "none"
prep2string PrepWithUsing              = "with/using"
prep2string PrepAtTo                   = "at/to"
prep2string PrepInfrontof              = "in front of"
prep2string PrepInInsideInto           = "in/inside/into"
prep2string PrepOntopofOnOntoUpon      = "on top of/on/onto/upon"
prep2string PrepOutofFrominsideFrom    = "out of/from inside/from"
prep2string PrepOver                   = "over"
prep2string PrepThrough                = "through"
prep2string PrepUnderUnderneathBeneath = "under/underneath/beneath"
prep2string PrepBehind                 = "behind"
prep2string PrepBeside                 = "beside"
prep2string PrepForAbout               = "for/about"
prep2string PrepIs                     = "is"
prep2string PrepAs                     = "as"
prep2string PrepOffofOff               = "off of/off"

string2prep :: StrT -> Maybe PrepSpec
string2prep = flip lookup $ concatMap mkAssoc [minBound ..]
  where mkAssoc :: PrepSpec -> [(StrT, PrepSpec)]
        mkAssoc prepSpec =
          [ (prep, prepSpec) | prep <- Str.splitOn "/" $
                                       prep2string prepSpec ] ++
          [ (Str.fromString $ show index, prepSpec)
          | let index = fromEnum prepSpec - fromEnum (succ PrepNone)
          , index >= 0
          ]

prepMatch :: PrepSpec -> PrepSpec -> Bool
prepMatch PrepAny _  = True
prepMatch vp      cp = vp == cp

prepPhrases :: [(PrepSpec, [StrT])]
prepPhrases = [ (prepSpec, Str.words prepPhrase)
              | prepSpec   <- [succ PrepNone ..]
              , prepPhrase <- Str.splitOn "/" $ prep2string prepSpec
              ]

-- | Does the given verb name match any of the given aliases? Each alias may
-- use @*@ to separate required and optional text to match.
verbNameMatch :: StrT -> [StrT] -> Bool
verbNameMatch name = any matchName
  where matchName :: StrT -> Bool
        matchName vname
          | post == ""  = name     == vname
          | post == "*" = preName  == pre
          | otherwise   = preName  == pre &&
                          postName `Str.isPrefixOf` Str.tail post
          where (pre, post)         = Str.breakOn "*" vname
                (preName, postName) = Str.splitAt (Str.length pre) name

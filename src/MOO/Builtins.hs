
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins ( builtinFunctions, callBuiltin, verifyBuiltins ) where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Data.HashMap.Lazy (HashMap)
import Data.List (transpose, inits)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import qualified Data.HashMap.Lazy as HM

import MOO.Builtins.Common
import MOO.Database
import MOO.Object
import MOO.Task
import MOO.Types

import MOO.Builtins.Extra   as Extra
import MOO.Builtins.Misc    as Misc
import MOO.Builtins.Network as Network
import MOO.Builtins.Objects as Objects
import MOO.Builtins.Tasks   as Tasks
import MOO.Builtins.Values  as Values

-- | A 'HashMap' of all built-in functions, keyed by name
builtinFunctions :: HashMap Id Builtin
builtinFunctions =
  HM.fromList $ map assoc $ Extra.builtins ++ Misc.builtins ++
  Values.builtins ++ Objects.builtins ++ Network.builtins ++ Tasks.builtins
  where assoc builtin = (builtinName builtin, builtin)

-- | Call the named built-in function with the given arguments, checking first
-- for the appropriate number and types of arguments. Raise 'E_INVARG' if the
-- built-in function is unknown.
callBuiltin :: Id -> [Value] -> MOO Value
callBuiltin func args = do
  isProtected <- ($ func) <$> serverOption protectFunction
  case (func `HM.lookup` builtinFunctions, isProtected) of
    (Just builtin, False) -> call builtin
    (Just builtin, True)  -> do
      this <- frame initialThis
      if this == systemObject then call builtin
        else callSystemVerb ("bf_" <> fromId func) args >>=
             maybe (checkWizard >> call builtin) return
    (Nothing, _) -> let name    = fromId func
                        message = "Unknown built-in function: " <> name
                    in raiseException (Err E_INVARG) message (Str name)

  where call :: Builtin -> MOO Value
        call builtin = checkArgs builtin args >> builtinFunction builtin args

        checkArgs :: Builtin -> [Value] -> MOO ()
        checkArgs Builtin { builtinMinArgs  = min
                          , builtinMaxArgs  = max
                          , builtinArgTypes = types
                          } args
          | nargs < min || maybe False (nargs >) max = raise E_ARGS
          | otherwise                                = checkTypes types args
          where nargs = length args :: Int

        checkTypes :: [Type] -> [Value] -> MOO ()
        checkTypes (t:ts) (v:vs)
          | typeMismatch t (typeOf v) = raise E_TYPE
          | otherwise                 = checkTypes ts vs
        checkTypes  _      _          = return ()

        typeMismatch :: Type -> Type -> Bool
        typeMismatch a    b    | a == b = False
        typeMismatch TAny _             = False
        typeMismatch TNum TInt          = False
        typeMismatch TNum TFlt          = False
        typeMismatch _    _             = True

-- | Perform internal consistency verification of all the built-in functions,
-- checking that each implementation actually accepts the claimed argument
-- types. Note that an inconsistency may cause the program to abort.
--
-- Assuming the program doesn't abort, this generates either a string
-- describing an inconsistency, or an integer giving the total number of
-- (verified) built-in functions.
verifyBuiltins :: Either String Int
verifyBuiltins = foldM accum 0 $ HM.elems builtinFunctions

  where accum :: Int -> Builtin -> Either String Int
        accum a b = valid b >>= Right . (+ a)

        valid :: Builtin -> Either String Int
        valid Builtin { builtinName     = name
                      , builtinMinArgs  = min
                      , builtinMaxArgs  = max
                      , builtinArgTypes = types
                      , builtinFunction = func
                      }
          | min < 0                           = invalid "arg min < 0"
          | maybe False (< min) max           = invalid "arg max < min"
          | length types /= fromMaybe min max = invalid "incorrect # types"
          | testArgs func min max types       = ok
          where invalid :: String -> Either String Int
                invalid msg = Left $ "problem with built-in function " ++
                              fromId name ++ ": " ++ msg
                ok = Right 1

        testArgs :: ([Value] -> MOO Value) -> Int -> Maybe Int -> [Type] -> Bool
        testArgs func min max types = all test argSpecs
          where argSpecs = drop min $ inits $ map mkArgs augmentedTypes
                augmentedTypes = maybe (types ++ [TAny]) (const types) max
                test argSpec = all (\args -> func args `seq` True) $
                               enumerateArgs argSpec

        enumerateArgs :: [[Value]] -> [[Value]]
        enumerateArgs [a]    = transpose [a]
        enumerateArgs (a:as) = concatMap (combine a) (enumerateArgs as)
          where combine ps rs = map (: rs) ps
        enumerateArgs []     = [[]]

        mkArgs :: Type -> [Value]
        mkArgs TAny = mkArgs TNum ++ mkArgs TStr ++ mkArgs TObj ++
                      mkArgs TErr ++ mkArgs TLst
        mkArgs TNum = mkArgs TInt ++ mkArgs TFlt
        mkArgs TInt = [Int 0]
        mkArgs TFlt = [Flt 0]
        mkArgs TStr = [emptyString]
        mkArgs TObj = [Obj 0]
        mkArgs TErr = [Err E_NONE]
        mkArgs TLst = [emptyList]

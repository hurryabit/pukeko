module CoreLang.Language.Operator
  ( Spec (..)
  , Assoc (..)
  , table
  , names
  , aprec
  , find
  )
  where

import Data.Map (Map)
import Data.Ratio
import Text.Parsec.Expr (Assoc (..))

import qualified Data.Map as Map

data Spec = MkSpec { _name :: String, _prec :: Rational, _assoc :: Assoc }

right, none :: String -> Spec
right _name = MkSpec { _name, _prec = undefined, _assoc = AssocRight }
none  _name = MkSpec { _name, _prec = undefined, _assoc = AssocNone  }

table :: [[Spec]]
table = fixPrecs
  [ [right "||"]
  , [right "&&"]
  , map none ["<", "<=", "==", "!=", ">=", ">"]
  , [right "+", none "-"]
  , [right "*", none "/"]
  ]

names :: [String]
names = map _name (concat table)

aprec :: Rational
aprec = fromIntegral (length table + 1)

dict :: Map String Spec
dict = Map.fromList $ map (\spec -> (_name spec, spec)) (concat table)

find :: String -> Spec
find name =
  case Map.lookup name dict of
    Nothing  -> error ("unknown operator: " ++ name)
    Just res -> res

fixPrecs :: [[Spec]] -> [[Spec]]
fixPrecs = zipWith (\prec -> map (\spec -> spec { _prec = prec })) [1 ..]

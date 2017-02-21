module Pukeko.Language.Operator
  ( Spec (..)
  , Assoc (..)
  , table
  , syms
  , aprec
  , find
  , findByName
  )
  where

import Data.Map (Map)
import Data.Ratio ()
import Text.Parsec.Expr (Assoc (..))

import qualified Data.List as List
import qualified Data.Map as Map

import Pukeko.Language.Ident

data Spec =
  MkSpec { _sym :: String, _name :: Ident, _prec :: Rational, _assoc :: Assoc }

mkSpec :: Assoc -> String -> String -> Spec
mkSpec _assoc _sym _name =
  MkSpec { _sym, _name = MkIdent _name, _prec = undefined, _assoc }

right, none :: String -> String -> Spec
right = mkSpec AssocRight
none  = mkSpec AssocNone

table :: [[Spec]]
table = fixPrecs
  [ [right "||" "or" ]
  , [right "&&" "and"]
  , zipWith none
    ["<" , "<=", "==", "!=", ">=", ">" ]
    ["lt", "le", "eq", "ne", "ge", "gt"]
  , [right "+" "add", none "-" "sub"]
  , [right "*" "mul", none "/" "div", none "%" "mod"]
  ]

syms :: [String]
syms = map _sym (concat table)

aprec :: Rational
aprec = fromIntegral (length table + 1)

dict :: Map String Spec
dict = Map.fromList $ map (\spec -> (_sym spec, spec)) (concat table)

find :: String -> Spec
find sym =
  case Map.lookup sym dict of
    Nothing  -> error ("unknown operator: " ++ sym)
    Just res -> res

findByName :: Ident -> Spec
findByName name =
  case List.find (\spec -> _name spec == name) (concat table) of
    Nothing  -> error ("unknown operator name: " ++ show name)
    Just res -> res

fixPrecs :: [[Spec]] -> [[Spec]]
fixPrecs = zipWith (\prec -> map (\spec -> spec { _prec = prec })) [1 ..]

module Pukeko.AST.Operator
  ( Binary
  , binary
  , Spec (..)
  , Assoc (..)
  , table
  , letters
  , aprec
  )
where

import Pukeko.Prelude

import qualified Data.Set as Set

data BinaryTag

-- TODO: Use Name for this.
type Binary = Tagged BinaryTag String

binary :: String -> Binary
binary op
  | all (`Set.member` letters) op = Tagged op
  | otherwise = impossible  -- guaranteed by parser

data Assoc = AssocLeft | AssocRight | AssocNone

data Spec = MkSpec{_sym :: Binary, _prec :: Int, _assoc :: Assoc}

letters :: Set Char
letters = Set.fromList "+-*/%=<>!&|;:∘"

mkSpec :: Assoc -> String -> Spec
mkSpec _assoc op = MkSpec{_sym = binary op, _prec = undefined, _assoc}

left, right, none :: String -> Spec
left  = mkSpec AssocLeft
right = mkSpec AssocRight
none  = mkSpec AssocNone

table :: [[Spec]]
table = fixPrecs
  [ [left ">>=", left ";"]
  , [right "||"]
  , [right "&&"]
  , map none ["<" , "<=", "==", "!=", ">=", ">" ]
  , [right "+", none "-"]
  , [right "*", none "/", none "%"]
  , [right "∘"]
  ]

aprec :: Int
aprec = length table + 1

fixPrecs :: [[Spec]] -> [[Spec]]
fixPrecs = zipWith (\prec -> map (\spec -> spec { _prec = prec })) [1 ..]

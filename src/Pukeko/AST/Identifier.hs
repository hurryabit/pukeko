module Pukeko.AST.Identifier
  ( Named (..)
  , EVar
  , evar
  , main
  , freshEVars
  , TVar
  , tvar
  , freshTVars
  -- , TCon
  -- , tcon
  , DCon
  , dcon
  -- , Clss
  -- , clss
  , annot
  )
where

import Pukeko.Prelude

import Data.Aeson
import Data.Aeson.TH
import Data.Char (isLower, isUpper)

class Named a where
  name :: a -> String

data EVar = EVar String
  deriving (Show, Eq, Ord)

evar :: String -> EVar
evar x@((isLower -> True):_) = EVar x
evar x = bugWith "invalid variable name" x

main :: EVar
main = evar "main"

freshEVars :: String -> EVar -> [EVar]
freshEVars comp (EVar x) = map (\n -> EVar (x ++ '$':comp ++ show n)) [1::Int ..]

instance Named EVar where
  name (EVar x) = x

instance Pretty EVar where
  pretty = pretty . name

data TVar
  = TVar{_name :: String}
  | IVar{_id   :: Int   }
  deriving (Show, Eq, Ord)

tvar :: String -> TVar
tvar _name@(first:_)
  | isLower first = TVar{_name}
tvar _name = bugWith "invalid type variable name" _name

freshTVars :: [TVar]
freshTVars = map IVar [1..]

instance Named TVar where
  name = \case
    TVar{_name} -> _name
    IVar{_id}   -> '_':show _id

instance Pretty TVar where
  pretty = pretty . name

data XCon tag = XCon String
  deriving (Show, Eq, Ord)

xcon :: String -> XCon tag
xcon name@(first:_)
  | isUpper first = XCon name
xcon name = bugWith "invalid constructor/clss name" name

annot :: XCon tag -> String -> XCon tag
annot (XCon x) y = XCon (x ++ "$" ++ y)

instance Named (XCon tag) where
  name (XCon n) = n

instance Pretty (XCon tag) where
  pretty = pretty . name

data DConTag
type DCon = XCon DConTag

dcon :: String -> DCon
dcon = xcon

deriveToJSON defaultOptions ''EVar
deriveToJSON defaultOptions ''TVar

instance ToJSON (XCon tag) where
  toJSON = $(mkToJSON defaultOptions ''XCon)
instance ToJSONKey EVar
instance ToJSONKey TVar
instance ToJSONKey (XCon tag)

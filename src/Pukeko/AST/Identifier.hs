module Pukeko.AST.Identifier
  ( Named (..)
  -- , EVar
  -- , evar
  -- , main
  -- , freshEVars
  , TVar
  , tvar
  , freshTVars
  )
where

import Pukeko.Prelude

import Data.Aeson
import Data.Aeson.TH
import Data.Char (isLower)

class Named a where
  name :: a -> String

data EVar = EVar String
  deriving (Show, Eq, Ord)

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

deriveToJSON defaultOptions ''EVar
deriveToJSON defaultOptions ''TVar

instance ToJSONKey EVar
instance ToJSONKey TVar

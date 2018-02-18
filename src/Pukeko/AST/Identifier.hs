module Pukeko.AST.Identifier
  ( Named (..)
  , TVar
  , tvar
  , freshTVars
  )
where

import Pukeko.Prelude
import Pukeko.Pretty

import Data.Aeson
import Data.Aeson.TH
import Data.Char (isLower)

class Named a where
  name :: a -> String

data TVar
  = TVar{_name :: String}
  | IVar{_id   :: Int   }
  deriving (Show, Eq, Ord)

tvar :: String -> TVar
tvar _name@(first:_)
  | isLower first = TVar{_name}
tvar _ = impossible  -- guaranteed by parser

freshTVars :: [TVar]
freshTVars = map IVar [1..]

instance Named TVar where
  name = \case
    TVar{_name} -> _name
    IVar{_id}   -> '_':show _id

instance Pretty TVar where
  pretty = pretty . name

deriveToJSON defaultOptions ''TVar

instance ToJSONKey TVar

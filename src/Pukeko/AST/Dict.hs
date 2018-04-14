module Pukeko.AST.Dict
  ( Dict (..)
  , DxBinder
  , dict2type
  ) where

import Pukeko.Prelude
import Pukeko.Pretty

import Data.Aeson.TH

import Pukeko.AST.Name
import Pukeko.AST.Type

data Dict
  = DVar DxVar
  | DDer DxVar [Type] [Dict]

type DxBinder ty = (DxVar, (Class, ty))

dict2type :: Traversal' Dict Type
dict2type f = \case
  DVar x -> pure (DVar x)
  DDer z ts ds -> DDer z <$> traverse f ts <*> (traverse . dict2type) f ds

instance Pretty Dict
instance PrettyPrec Dict where
  prettyPrec _prec _dict = "{Dict}"

deriving instance Show Dict

deriveToJSON defaultOptions ''Dict

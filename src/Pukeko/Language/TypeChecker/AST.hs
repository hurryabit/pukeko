module Pukeko.Language.TypeChecker.AST
  ( Module
  , TopLevel (..)
  , topLevelExpr
  , module Pukeko.Language.KindChecker.AST
  )
where

import           Control.Lens

import           Pukeko.Language.Base.AST
import           Pukeko.Language.KindChecker.AST hiding (Module, TopLevel (..))
import qualified Pukeko.Language.Ident           as Id

type Module = [TopLevel]

data TopLevel
  = Def Pos Id.EVar (Expr Id.EVar)
  | Asm Pos Id.EVar String

makePrisms ''TopLevel

instance HasLhs TopLevel where
  type Lhs TopLevel = Id.EVar
  lhs f = \case
    Def w x t -> fmap (\x' -> Def w x' t) (f x)
    Asm w x s -> fmap (\x' -> Asm w x' s) (f x)

topLevelExpr :: Traversal' TopLevel (Expr Id.EVar)
topLevelExpr = _Def . _3

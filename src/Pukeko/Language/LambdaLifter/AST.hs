{-# LANGUAGE DataKinds #-}
module Pukeko.Language.LambdaLifter.AST
  ( Module
  , TopLevel (..)
  , Defn
  , Expr
  , Case
  )
where

import qualified Data.Vector.Sized as Vec

import           Pukeko.Pretty
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.Ident        as Id

data LAMBDALIFTER

instance Stage LAMBDALIFTER where
  type HasLam    LAMBDALIFTER = 'False
  type HasMat    LAMBDALIFTER = 'False

type Module = StdModule TopLevel

data TopLevel
  = forall n. Def Pos Id.EVar (Vec.Vector n Bind) (Expr (FinScope n Id.EVar))
  |           Caf Pos Id.EVar (Expr Id.EVar)
  |           Asm Pos Id.EVar String

type Defn = StdDefn LAMBDALIFTER
type Expr = StdExpr LAMBDALIFTER
type Case = StdCase LAMBDALIFTER

instance Pretty TopLevel where
  pPrintPrec _ _ = \case
    Def _ x bs t ->
      "let" <+> hang (pretty x <+> prettyBinds bs <+> equals) 2 (pretty t)
    Caf _ x t ->
      "let" <+> hang (pretty x <+> equals) 2 (pretty t)
    Asm _ x s ->
      hsep ["external", pretty x, equals, text (show s)]

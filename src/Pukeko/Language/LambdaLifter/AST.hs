{-# LANGUAGE DataKinds #-}
module Pukeko.Language.LambdaLifter.AST
  ( ModuleInfo
  , Module
  , TopLevel
  , Defn
  , Expr
  , Case
  )
where


import           Pukeko.Pretty
import           Pukeko.Language.AST.Std

data LAMBDALIFTER

instance Stage LAMBDALIFTER where
  type StageId LAMBDALIFTER = 700

type ModuleInfo = GenModuleInfo 'True
type Module = StdModule LAMBDALIFTER
type TopLevel = StdTopLevel LAMBDALIFTER
type Defn = StdDefn LAMBDALIFTER
type Expr = StdExpr LAMBDALIFTER
type Case = StdCase LAMBDALIFTER

-- TODO: Move to Pukeko.Language.AST.Std
instance Pretty TopLevel where
  pPrintPrec _ _ = \case
    SupCom _ x bs t ->
      "let" <+> hang (pretty x <+> prettyBinds bs <+> equals) 2 (pretty t)
    Caf _ x t ->
      "let" <+> hang (pretty x <+> equals) 2 (pretty t)
    Asm _ x s ->
      hsep ["external", pretty x, equals, text (show s)]

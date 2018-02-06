module Pukeko.MiddleEnd.TypeEraser
  ( Module
  , eraseModule
  )
where

import Pukeko.Prelude

import qualified Data.Map as Map

import           Pukeko.AST.NoLambda
import           Pukeko.AST.Scope
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.SuperCore  as In
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.FrontEnd.Info

eraseModule :: In.Module -> Module
eraseModule m0@(In.MkModule _types extns supcs) = runCC m0 $ do
  extns <- traverse ccExtnDecl (toList extns)
  supcs <- traverse ccSupCDecl (toList supcs)
  pure (extns ++ supcs)

type CCState = Map Id.EVar Name

type CC = Eff [Reader ModuleInfo, State CCState]

runCC :: In.Module -> CC a -> a
runCC mod0 = run . evalState mempty . runInfo mod0

name :: Id.EVar -> Name
name = MkName . Id.mangled

bindName :: In.Bind tv -> Name
bindName = name . unlctd . In._bind2evar

ccSupCDecl :: In.FuncDecl 'In.SupC -> CC TopLevel
ccSupCDecl (In.SupCDecl (unlctd . In._bind2evar -> z) _ bs e) =
  Def (name z) (map (Just . bindName) (toList bs)) <$> ccExpr e

ccExtnDecl :: In.FuncDecl 'In.Extn -> CC TopLevel
ccExtnDecl (In.ExtnDecl b s) = do
    let n = MkName s
    modify (Map.insert (unlctd (In._bind2evar b)) n)
    pure (Asm n)

ccDefn :: (BaseEVar ev) => In.Defn tv ev -> CC Defn
ccDefn (In.MkDefn b t) = MkDefn (bindName b) <$> ccExpr t

ccExpr :: (BaseEVar ev) => In.Expr tv ev -> CC Expr
ccExpr = \case
  In.EVar x -> pure (Local (name (baseEVar x)))
  In.EVal z -> do
    external <- gets (Map.lookup z)
    case external of
      Nothing -> pure (Global (name z))
      Just s  -> pure (External s)
  In.ECon dcon  -> do
    (_tcon, MkDConDecl{_dcon2tag = tag, _dcon2flds = flds}) <- findInfo info2dcons dcon
    pure $ Pack tag (length flds)
  In.ENum n     -> pure $ Num n
  In.EApp t us  -> Ap <$> ccExpr t <*> traverse ccExpr (toList us)
  In.ELet ds t  -> Let False <$> traverse ccDefn ds <*> ccExpr t
  In.ERec ds t  -> Let True  <$> traverse ccDefn ds <*> ccExpr t
  In.ECas t  cs -> Match <$> ccExpr t <*> traverse ccCase (toList cs)
  In.ETyApp e0 _ts -> ccExpr e0
  In.ETyAbs _vs e0 -> ccExpr e0
  In.ECoe _ e0 -> ccExpr e0

ccCase :: (BaseEVar ev) => In.Case tv ev -> CC Altn
ccCase (In.MkCase _ _ bs t) = MkAltn (map (fmap name) (toList bs)) <$> ccExpr t

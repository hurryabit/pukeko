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
import qualified Pukeko.AST.Name       as In
import qualified Pukeko.AST.SuperCore  as In
import           Pukeko.FrontEnd.Info

eraseModule :: In.Module -> Module
eraseModule m0@(In.MkModule _types extns supcs) = runCC m0 $ do
  extns <- traverse ccExtnDecl (toList extns)
  supcs <- traverse ccSupCDecl (toList supcs)
  pure (extns ++ supcs)

type CCState = Map (In.Name In.EVar) Name

type CC = Eff [Reader ModuleInfo, State CCState]

runCC :: In.Module -> CC a -> a
runCC mod0 = run . evalState mempty . runInfo mod0

name :: In.Name In.EVar -> Name
name = MkName . untag . In.nameText

bindName :: In.Bind -> Name
bindName = name . In.nameOf

ccSupCDecl :: In.FuncDecl (In.Only In.SupC) -> CC TopLevel
ccSupCDecl (In.SupCDecl z _ _ bs e) =
  Def (name z) (map (Just . bindName) (toList bs)) <$> ccExpr e

ccExtnDecl :: In.FuncDecl (In.Only In.Extn) -> CC TopLevel
ccExtnDecl (In.ExtnDecl z _ s) = do
    let n = MkName s
    modify (Map.insert z n)
    pure (Asm n)

ccDefn :: (BaseEVar ev) => In.Defn ev -> CC Defn
ccDefn (In.MkDefn b t) = MkDefn (bindName b) <$> ccExpr t

ccExpr :: (BaseEVar ev) => In.Expr ev -> CC Expr
ccExpr = \case
  In.EVar x -> pure (Local (name (baseEVar x)))
  In.EVal z -> do
    external <- gets (Map.lookup z)
    case external of
      Nothing -> pure (Global (name z))
      Just s  -> pure (External s)
  In.ECon dcon  -> do
    (_tcon, MkDConDecl{_dcon2tag = tag, _dcon2fields = flds}) <- findInfo info2dcons dcon
    pure $ Pack tag (length flds)
  In.ENum n     -> pure $ Num n
  e0@In.EApp{}
    | (e1, as) <- In.unwindEApp e0 -> Ap <$> ccExpr e1 <*> traverse ccExpr as
  In.ELet ds t  -> Let False <$> traverse ccDefn ds <*> ccExpr t
  In.ERec ds t  -> Let True  <$> traverse ccDefn ds <*> ccExpr t
  In.EMat t  cs -> Match <$> ccExpr t <*> traverse ccAltn (toList cs)
  In.ETyApp e0 _ts -> ccExpr e0
  In.ETyAbs _vs e0 -> ccExpr e0
  In.ETyCoe _   e0 -> ccExpr e0
  In.ETyAnn _   e0 -> ccExpr e0

ccAltn :: (BaseEVar ev) => In.Altn ev -> CC Altn
ccAltn (In.MkAltn (In.PSimple _ _ bs) e) = MkAltn (map (fmap name) bs) <$> ccExpr e

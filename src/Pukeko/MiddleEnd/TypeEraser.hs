{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Pukeko.MiddleEnd.TypeEraser
  ( Module
  , eraseModule
  )
where

import Pukeko.Prelude

import qualified Data.Map as Map

import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Name as In
import           Pukeko.AST.NoLambda
import qualified Pukeko.AST.SuperCore as In
import qualified Pukeko.AST.Unwind as In
import           Pukeko.FrontEnd.Info

eraseModule :: In.Module -> Module
eraseModule m0@(In.MkModule _types extns supcs) = runCC m0 $ do
  extns <- traverse ccExtnDecl (toList extns)
  supcs <- traverse ccSupCDecl (toList supcs)
  pure (extns ++ supcs)

type CCState = Map In.TmVar Name

type CC = Eff [Reader ModuleInfo, State CCState]

runCC :: In.Module -> CC a -> a
runCC mod0 = run . evalState mempty . runInfo mod0

name :: In.TmVar -> Name
name = MkName . untag . In.nameText

bindName :: In.TmBinder t -> Name
bindName = name . In.nameOf

ccSupCDecl :: In.FuncDecl (In.Only In.SupC) -> CC TopLevel
ccSupCDecl (In.SupCDecl z _ bs e) =
  Def (name z) (map (Just . bindName) (toListOf (traverse . In._TmPar) bs)) <$> ccExpr e

ccExtnDecl :: In.FuncDecl (In.Only In.Extn) -> CC TopLevel
ccExtnDecl (In.ExtnDecl z _ s) = do
    let n = MkName s
    modify (Map.insert z n)
    pure (Asm n)

ccExpr :: In.Expr -> CC Expr
ccExpr = \case
  In.EVar x -> pure (Local (name x))
  In.EVal z -> do
    external <- gets (Map.lookup z)
    case external of
      Nothing -> pure (Global (name z))
      Just s  -> pure (External s)
  In.ECon dcon  -> do
    (_tcon, MkTmConDecl{_tmcon2tag = tag, _tmcon2fields = flds}) <-
      findInfo info2tmcons dcon
    pure $ Pack tag (length flds)
  In.ENum n     -> pure $ Num n
  In.EAtm{} -> impossible  -- all cases matched above
  e0@(In.EApp _ In.TmArg{})
    | (e1, as) <- In.unwindl In._ETmApp e0 -> Ap <$> ccExpr e1 <*> traverse ccExpr as
  In.ELet (In.TmNonRec b e0) e1 ->
    Let <$> (MkDefn (bindName b) <$> ccExpr e0) <*> ccExpr e1
  In.ELet (In.TmRec bs) e1 ->
    LetRec
    <$> traverse (\(b, e0) -> MkDefn (bindName b) <$> ccExpr e0) (toList bs)
    <*> ccExpr e1
  In.EMat _ e cs -> Match <$> ccExpr e <*> traverse ccAltn (toList cs)
  In.EApp e0 In.TyArg{} -> ccExpr e0
  In.ECast  _   e0 -> ccExpr e0

ccAltn :: In.Altn -> CC Altn
ccAltn (In.MkAltn (In.PSimple _ bs) e) =
  MkAltn (map (fmap (name . fst)) bs) <$> ccExpr e

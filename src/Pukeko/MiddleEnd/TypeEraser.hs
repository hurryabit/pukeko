{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Pukeko.MiddleEnd.TypeEraser
  ( Module
  , eraseModule
  )
where

import Pukeko.Prelude

import qualified Data.Map.Extended as Map

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

data CCEnv = CCEnv (Map In.TmVar Int) Int

type CC = Eff [Reader ModuleInfo, Reader CCEnv, State CCState]

runCC :: In.Module -> CC a -> a
runCC mod0 = run . evalState mempty . runReader env0 . runInfo mod0
  where
    env0 = CCEnv Map.empty 0

introVar :: Maybe In.TmVar -> CC a -> CC a
introVar mx = local $ \(CCEnv idxs dpth) ->
  let idxs' = maybe idxs (\x -> Map.insert x dpth idxs) mx
  in CCEnv idxs' (dpth+1)

introVars :: [Maybe In.TmVar] -> CC a -> CC a
introVars mxs act = foldr introVar act mxs

askVarIndex :: In.TmVar -> CC Int
askVarIndex x = do
  CCEnv idxs dpth <- ask @CCEnv
  pure (dpth - idxs Map.! x)

name :: In.TmVar -> Name
name = MkName . untag . In.nameText

bindName :: In.TmBinder t -> Name
bindName = name . In.nameOf

ccSupCDecl :: In.FuncDecl (In.Only In.SupC) -> CC TopLevel
ccSupCDecl (In.SupCDecl z _ bs e) = do
  let xs = map In.nameOf (toListOf (traverse . In._TmPar) bs)
  Def (name z) (map (Just . name) xs) <$> introVars (map Just xs) (ccExpr e)

ccExtnDecl :: In.FuncDecl (In.Only In.Extn) -> CC TopLevel
ccExtnDecl (In.ExtnDecl z _ s) = do
    let n = MkName s
    modify (Map.insert z n)
    pure (Asm n)

ccExpr :: In.Expr -> CC Expr
ccExpr = \case
  In.EVar x -> Local (name x) <$> askVarIndex x
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
  In.ELet (In.TmNonRec b e0) e1 -> do
    let x = In.nameOf b
    Let <$> (MkDefn (name x) <$> ccExpr e0) <*> introVar (Just x) (ccExpr e1)
  In.ELet (In.TmRec bs) e1 ->
    introVars (map (Just . In.nameOf) bs) $
      LetRec
      <$> traverse (\(b, e0) -> MkDefn (bindName b) <$> ccExpr e0) (toList bs)
      <*> ccExpr e1
  In.EMat _ e cs -> Match <$> ccExpr e <*> traverse ccAltn (toList cs)
  In.EApp e0 In.TyArg{} -> ccExpr e0
  In.ECast  _   e0 -> ccExpr e0

ccAltn :: In.Altn -> CC Altn
ccAltn (In.MkAltn (In.PSimple _ bs) e) =
  MkAltn (map (fmap (name . fst)) bs) <$> introVars (map (fmap In.nameOf) bs) (ccExpr e)

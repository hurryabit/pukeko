module Pukeko.MiddleEnd.TypeEraser
  ( Module
  , eraseModule
  )
where

import Pukeko.Prelude

import           Control.Lens

import           Pukeko.AST.NoLambda
import           Pukeko.AST.Scope
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.SystemF    as In
import qualified Pukeko.AST.Type       as In
import qualified Pukeko.AST.Stage      as St
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.FrontEnd.Info

type In = St.LambdaLifter

eraseModule :: In.Module In -> Module
eraseModule m0@(In.MkModule tops) = runCC (catMaybes <$> traverse ccDecl tops) m0

type CCState = Map Id.EVar Name

newtype CC a = CC{unCC :: InfoT (State CCState) a}
  deriving ( Functor, Applicative, Monad
           , MonadInfo
           , MonadState CCState
           )

runCC :: CC a -> In.Module In -> a
runCC cc decls = evalState (runInfoT (unCC cc) decls) mempty

name :: Id.EVar -> Name
name = MkName . Id.mangled

bindName :: In.Bind In.Type tv -> Name
bindName = name . In._bind2evar

ccDecl :: In.Decl In -> CC (Maybe TopLevel)
ccDecl = \case
  In.DType{} -> pure Nothing
  In.DSupC (In.MkSupCDecl _ z _ _ bs e) ->
    Just <$> Def (name z) (map (Just . bindName) (toList bs)) <$> ccExpr e
  In.DPrim (In.MkPrimDecl b s) -> do
    let n = MkName s
    at (In._bind2evar b) ?= n
    pure (Just (Asm n))

ccDefn :: (BaseEVar ev) => In.Defn In tv ev -> CC Defn
ccDefn (In.MkDefn b t) = MkDefn (bindName b) <$> ccExpr t

ccExpr :: (BaseEVar ev) => In.Expr In tv ev -> CC Expr
ccExpr = \case
  In.EVar _ x -> pure (Local (name (baseEVar x)))
  In.EVal _ z -> do
    external <- use (at z)
    case external of
      Nothing -> pure (Global (name z))
      Just s  -> pure (External s)
  In.ECon _ dcon  -> do
    Some1 (Pair1 _tcon MkDConDecl{_dcon2tag = tag, _dcon2flds = flds}) <- findDCon dcon
    pure $ Pack tag (length flds)
  In.ENum _ n     -> pure $ Num n
  In.EApp _ t us  -> Ap <$> ccExpr t <*> traverse ccExpr (toList us)
  In.ELet _ ds t  -> Let False <$> traverse ccDefn (toList ds) <*> ccExpr t
  In.ERec _ ds t  -> Let True  <$> traverse ccDefn (toList ds) <*> ccExpr t
  In.ECas _ t  cs -> Match <$> ccExpr t <*> traverse ccCase (toList cs)
  In.ETyApp _ e0 _ts -> ccExpr e0
  In.ETyAbs _ _vs e0 -> ccExpr e0

ccCase :: (BaseEVar ev) => In.Case In tv ev -> CC Altn
ccCase (In.MkCase _ _ _ bs t) =
  MkAltn (map (Just . name) (toList bs)) <$> ccExpr t

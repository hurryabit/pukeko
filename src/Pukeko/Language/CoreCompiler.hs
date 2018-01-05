{-# LANGUAGE DataKinds #-}
module Pukeko.Language.CoreCompiler
  ( Module
  , compileModule
  )
where

import           Control.Lens
import           Control.Monad.State
import           Data.Foldable     (toList)
import qualified Data.Map          as Map
import qualified Data.Vector.Sized as Vec

import           Pukeko.Core.Syntax
import           Pukeko.Language.AST.Scope
import qualified Pukeko.Language.AST.ConDecl      as Con
import qualified Pukeko.Language.AST.Std          as LL
import qualified Pukeko.Language.LambdaLifter.AST as LL
import qualified Pukeko.Language.Ident            as Id
import           Pukeko.Language.Info

type CCState = Map.Map Id.EVar Name

newtype CC a = CC{unCC :: InfoT LL.ModuleInfo (State CCState) a}
  deriving ( Functor, Applicative, Monad
           , MonadInfo LL.ModuleInfo
           , MonadState CCState
           )

runCC :: CC a -> LL.ModuleInfo -> a
runCC cc decls = evalState (runInfoT (unCC cc) decls) mempty

compileModule :: LL.Module -> Module
compileModule (LL.MkModule decls tops) = runCC (traverse ccTopLevel tops) decls

name :: Id.EVar -> Name
name = MkName . Id.mangled

ccTopLevel :: LL.TopLevel -> CC TopLevel
ccTopLevel = \case
  LL.SupCom _ x bs t -> Def (name x) (ccBinds bs) <$> ccExpr t
  LL.Caf    _ x    t -> Def (name x) []           <$> ccExpr t
  LL.Asm    _ x    s -> do
    let n = MkName s
    at x ?= n
    pure (Asm n)

ccDefn :: IsVar v => LL.Defn v -> CC Defn
ccDefn (LL.MkDefn _ v t) = MkDefn (name v) <$> ccExpr t

ccExpr :: IsVar v => LL.Expr v -> CC Expr
ccExpr = \case
  LL.Var _ v
    | not (isTotallyFree v) -> return Local{_name}
    | otherwise -> do
        external <- use (at x)
        case external of
          Nothing    -> return Global  {_name}
          Just _name -> return External{_name}
    where
      x = varName v
      _name = name x
  LL.Con _ dcon  -> do
    Con.MkDConDecl{_tag, _fields} <- findDCon dcon
    pure $ Pack _tag (length _fields)
  LL.Num _ n     -> pure $ Num n
  LL.App _ t us  -> Ap <$> ccExpr t <*> traverse ccExpr us
  LL.Let _ ds t  -> Let False <$> traverse ccDefn (toList ds) <*> ccExpr t
  LL.Rec _ ds t  -> Let True  <$> traverse ccDefn (toList ds) <*> ccExpr t
  LL.Cas _ t  cs -> Match <$> ccExpr t <*> traverse ccCase cs

ccCase :: IsVar v => LL.Case v -> CC Altn
ccCase (LL.MkCase _ _ bs t) = MkAltn (ccBinds bs) <$> ccExpr t

ccBinds :: Vec.Vector n LL.Bind -> [Maybe Name]
ccBinds = map (fmap name . LL.bindName) . toList

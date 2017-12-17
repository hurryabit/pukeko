module Pukeko.Language.CoreCompiler
  ( Module
  , compileModule
  )
where

import           Control.Lens
import           Control.Monad.State
import           Data.Foldable (toList)
import           Data.Map (Map)
import           Data.Vector.Sized (Vector)
-- import qualified Data.Map as Map

import           Pukeko.Error
-- import           Pukeko.Pretty
import qualified Pukeko.Language.Type             as Ty
import           Pukeko.Core.Syntax
import           Pukeko.Language.Base.AST         (IsVar (..))
import qualified Pukeko.Language.Base.AST         as In
import qualified Pukeko.Language.LambdaLifter.AST as In
import qualified Pukeko.Language.Ident            as Id

type CCState = Map Id.EVar Name

newtype CC a = CC{unCC :: State CCState a}
  deriving ( Functor, Applicative, Monad
           , MonadState CCState
           )

runCC :: CC a -> a
runCC cc = evalState (unCC cc) mempty

compileModule :: In.Module -> Module
compileModule = runCC . traverse ccTopLevel

name :: Id.EVar -> Name
name = MkName . Id.mangled

ccTopLevel :: In.TopLevel -> CC TopLevel
ccTopLevel = \case
  In.Def _ x bs t -> Def (name x) (ccBinds bs) <$> ccExpr t
  In.Caf _ x    t -> Def (name x) []           <$> ccExpr t
  In.Asm _ x    s -> do
    let n = MkName s
    at x ?= n
    pure $ Asm n

ccDefn :: IsVar v => In.Defn v -> CC Defn
ccDefn (In.MkDefn _ v t) = MkDefn (name v) <$> ccExpr t

ccExpr :: IsVar v => In.Expr v -> CC Expr
ccExpr = \case
  In.Var _ v
    | not (isTotallyFree v) -> return Local{_name}
    | otherwise -> do
        external <- use (at x)
        case external of
          Nothing    -> return Global  {_name}
          Just _name -> return External{_name}
    where
      x = varName v
      _name = name x
  In.Con _ c     -> pure $ Pack (Ty._tag c) (length (Ty._fields c))
  In.Num _ n     -> pure $ Num n
  In.App _ t us  -> Ap <$> ccExpr t <*> traverse ccExpr us
  In.Let _ ds t  -> Let False <$> traverse ccDefn (toList ds) <*> ccExpr t
  In.Rec _ ds t  -> Let True  <$> traverse ccDefn (toList ds) <*> ccExpr t
  In.Mat _ t  as -> Match <$> ccExpr t <*> traverse ccAltn as

ccAltn :: IsVar v => In.Altn v -> CC Altn
ccAltn (In.MkAltn _ p t) = MkAltn (ccPatn p) <$> ccExpr t

ccPatn :: In.Patn -> [Maybe Name]
ccPatn = \case
  In.Simp _ _ bs -> map (fmap name . In.bindName) bs
  _ -> bug "core compiler" "expected simple pattern" Nothing

ccBinds :: Vector n In.Bind -> [Maybe Name]
ccBinds = map (fmap name . In.bindName) . toList

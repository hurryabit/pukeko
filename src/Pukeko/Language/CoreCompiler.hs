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
import qualified Pukeko.Language.AST.Std          as LL
import qualified Pukeko.Language.LambdaLifter.AST as LL
import qualified Pukeko.Language.Ident            as Id
import qualified Pukeko.Language.Type             as Ty

type CCState = Map.Map Id.EVar Name

newtype CC a = CC{unCC :: State CCState a}
  deriving ( Functor, Applicative, Monad
           , MonadState CCState
           )

runCC :: CC a -> a
runCC cc = evalState (unCC cc) mempty

compileModule :: LL.Module -> Module
compileModule = runCC . traverse ccTopLevel

name :: Id.EVar -> Name
name = MkName . Id.mangled

ccTopLevel :: LL.TopLevel -> CC TopLevel
ccTopLevel = \case
  LL.Def _ x bs t -> Def (name x) (ccBinds bs) <$> ccExpr t
  LL.Caf _ x    t -> Def (name x) []           <$> ccExpr t
  LL.Asm _ x    s -> do
    let n = MkName s
    at x ?= n
    pure $ Asm n

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
  LL.Con _ c     -> pure $ Pack (Ty._tag c) (length (Ty._fields c))
  LL.Num _ n     -> pure $ Num n
  LL.App _ t us  -> Ap <$> ccExpr t <*> traverse ccExpr us
  LL.Let _ ds t  -> Let False <$> traverse ccDefn (toList ds) <*> ccExpr t
  LL.Rec _ ds t  -> Let True  <$> traverse ccDefn (toList ds) <*> ccExpr t
  LL.Cas _ t  cs -> Match <$> ccExpr t <*> traverse ccCase cs

ccCase :: IsVar v => LL.Case v -> CC Altn
ccCase (LL.MkCase _ _ bs t) = MkAltn (ccBinds bs) <$> ccExpr t

ccBinds :: Vec.Vector n LL.Bind -> [Maybe Name]
ccBinds = map (fmap name . LL.bindName) . toList

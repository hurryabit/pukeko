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
import qualified Pukeko.Language.AST.Std          as In
import qualified Pukeko.Language.AST.Stage        as St
import qualified Pukeko.Language.Ident            as Id
import           Pukeko.Language.Info

type In = St.LambdaLifter

type CCState = Map.Map Id.EVar Name

newtype CC a = CC{unCC :: InfoT (In.ModuleInfo In) (State CCState) a}
  deriving ( Functor, Applicative, Monad
           , MonadInfo (In.GenModuleInfo 'True 'True)
           , MonadState CCState
           )

runCC :: CC a -> In.ModuleInfo In -> a
runCC cc decls = evalState (runInfoT (unCC cc) decls) mempty

compileModule :: In.Module In -> Module
compileModule (In.MkModule decls tops) = runCC (traverse ccTopLevel tops) decls

name :: Id.EVar -> Name
name = MkName . Id.mangled

ccTopLevel :: In.TopLevel In -> CC TopLevel
ccTopLevel = \case
  In.SupCom _ x bs t -> Def (name x) (ccBinds bs) <$> ccExpr t
  In.Caf    _ x    t -> Def (name x) []           <$> ccExpr t
  In.Asm    _ x    s -> do
    let n = MkName s
    at x ?= n
    pure (Asm n)

ccDefn :: IsVar v => In.Defn In v -> CC Defn
ccDefn (In.MkDefn _ v t) = MkDefn (name v) <$> ccExpr t

ccExpr :: IsVar v => In.Expr In v -> CC Expr
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
  In.Con _ dcon  -> do
    Con.MkDConDecl{_tag, _fields} <- findDCon dcon
    pure $ Pack _tag (length _fields)
  In.Num _ n     -> pure $ Num n
  In.App _ t us  -> Ap <$> ccExpr t <*> traverse ccExpr us
  In.Let _ ds t  -> Let False <$> traverse ccDefn (toList ds) <*> ccExpr t
  In.Rec _ ds t  -> Let True  <$> traverse ccDefn (toList ds) <*> ccExpr t
  In.Cas _ t  cs -> Match <$> ccExpr t <*> traverse ccCase cs

ccCase :: IsVar v => In.Case In v -> CC Altn
ccCase (In.MkCase _ _ bs t) = MkAltn (ccBinds bs) <$> ccExpr t

ccBinds :: Vec.Vector n In.Bind -> [Maybe Name]
ccBinds = map (fmap name . In.bindName) . toList

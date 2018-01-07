module Pukeko.Language.CoreCompiler
  ( Module
  , compileModule
  )
where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable     (toList)
import qualified Data.Map          as Map

import           Pukeko.Core.Syntax
import           Pukeko.Language.AST.Classes
import           Pukeko.Language.AST.Scope
import qualified Pukeko.Language.AST.ConDecl      as Con
import qualified Pukeko.Language.AST.Std          as In
import qualified Pukeko.Language.AST.Stage        as St
import qualified Pukeko.Language.Ident            as Id
import           Pukeko.Language.Info

type In = St.LambdaLifter

type CCEnv v = v -> Maybe Id.EVar

type CCState = Map.Map Id.EVar Name

newtype CC v a = CC{unCC :: InfoT (In.ModuleInfo In) (ReaderT (CCEnv v) (State CCState)) a}
  deriving ( Functor, Applicative, Monad
           , MonadInfo (In.GenModuleInfo 'True 'True)
           , MonadReader (CCEnv v)
           , MonadState CCState
           )

runCC :: CC Id.EVar a -> In.ModuleInfo In -> a
runCC cc decls = evalState (runReaderT (runInfoT (unCC cc) decls) Just) mempty

compileModule :: In.Module In -> Module
compileModule (In.MkModule decls tops) = runCC (traverse ccTopLevel tops) decls

name :: Id.EVar -> Name
name = MkName . Id.mangled

bindName :: In.Bind In tv -> Name
bindName = name . view lhs

scoped :: CC (EScope i v) a -> CC v a
scoped = CC . mapInfoT (withReaderT (scope (const Nothing))) . unCC

ccTopLevel :: In.TopLevel In -> CC Id.EVar TopLevel
ccTopLevel = \case
  In.TLSup b bs t ->
    Def (bindName b) (map (Just . bindName) (toList bs)) <$> scoped (ccExpr t)
  In.TLCaf b    t -> Def (bindName b) [] <$> ccExpr t
  In.TLAsm b    s -> do
    let n = MkName s
    at (b^.lhs) ?= n
    pure (Asm n)

ccDefn :: IsEVar ev => In.Defn In tv ev -> CC ev Defn
ccDefn (In.MkDefn b t) = MkDefn (bindName b) <$> ccExpr t

ccExpr :: IsEVar ev => In.Expr In tv ev -> CC ev Expr
ccExpr = \case
  In.EVar _ x0 -> do
    global <- asks ($ x0)
    case global of
      Nothing -> pure (Local (name (baseName x0)))
      Just x1 -> do
        external <- use (at x1)
        case external of
          Nothing -> pure (Global (name x1))
          Just y2 -> pure (External y2)
  In.ECon _ dcon  -> do
    Con.MkDConDecl Con.MkDConDeclN{_tag, _fields} <- findDCon dcon
    pure $ Pack _tag (length _fields)
  In.ENum _ n     -> pure $ Num n
  In.EApp _ t us  -> Ap <$> ccExpr t <*> traverse ccExpr us
  In.ELet _ ds t  -> Let False <$> traverse ccDefn (toList ds) <*> scoped (ccExpr t)
  In.ERec _ ds t  -> scoped $ Let True  <$> traverse ccDefn (toList ds) <*> ccExpr t
  In.ECas _ t  cs -> Match <$> ccExpr t <*> traverse ccCase cs

ccCase :: IsEVar ev => In.Case In tv ev -> CC ev Altn
ccCase (In.MkCase _ _ _ bs t) =
  MkAltn (map (fmap name) (toList bs)) <$> scoped (ccExpr t)

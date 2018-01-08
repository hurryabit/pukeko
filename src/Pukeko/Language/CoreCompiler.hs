module Pukeko.Language.CoreCompiler
  ( Module
  , compileModule
  )
where

import           Control.Lens
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

bindName :: In.Bind In tv -> Name
bindName = name . view lhs

ccTopLevel :: In.TopLevel In -> CC TopLevel
ccTopLevel = \case
  In.TLSup b bs t ->
    Def (bindName b) (map (Just . bindName) (toList bs)) <$> ccExpr t
  In.TLCaf b    t -> Def (bindName b) [] <$> ccExpr t
  In.TLAsm b    s -> do
    let n = MkName s
    at (b^.lhs) ?= n
    pure (Asm n)

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
    Con.MkDConDecl Con.MkDConDeclN{_tag, _fields} <- findDCon dcon
    pure $ Pack _tag (length _fields)
  In.ENum _ n     -> pure $ Num n
  In.EApp _ t us  -> Ap <$> ccExpr t <*> traverse ccExpr us
  In.ELet _ ds t  -> Let False <$> traverse ccDefn (toList ds) <*> ccExpr t
  In.ERec _ ds t  -> Let True  <$> traverse ccDefn (toList ds) <*> ccExpr t
  In.ECas _ t  cs -> Match <$> ccExpr t <*> traverse ccCase (toList cs)

ccCase :: (BaseEVar ev) => In.Case In tv ev -> CC Altn
ccCase (In.MkCase _ _ _ bs t) =
  MkAltn (map (fmap name) (toList bs)) <$> ccExpr t

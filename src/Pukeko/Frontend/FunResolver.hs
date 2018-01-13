module Pukeko.FrontEnd.FunResolver
  ( resolveModule
  ) where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Map      as Map
import qualified Data.Set      as Set

import           Pukeko.Error
import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import qualified Pukeko.AST.ModuleInfo as MI
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type

type In  = St.TypeResolver
type Out = St.FunResolver

data FRState = MkFRState
  { _declared :: Map.Map Id.EVar (Pos, Type Void)
  , _defined  :: Set.Set Id.EVar
  }
makeLenses ''FRState

newtype FR a = FR{unFR :: StateT FRState (Except String) a}
  deriving ( Functor, Applicative, Monad
           , MonadState FRState
           , MonadError String
           )

runFR :: MonadError String m => FR a -> m (a, FRState)
runFR fr = runExcept (runStateT (unFR fr) st0)
  where
    st0 = MkFRState mempty mempty

resolveModule :: MonadError String m => Module In -> m (Module Out)
resolveModule (MkModule info0 tops0) = do
  (tops1, MkFRState decld defnd) <- runFR (traverse frTopLevel tops0)
  let undefnd = decld `Map.difference` Map.fromSet id defnd
  case Map.minViewWithKey undefnd of
    Just ((fun, (w, _)), _) -> throwAt w "declared but undefined function" fun
    Nothing -> do
      let info1 = info0{MI._info2funs = MI.Present decld}
      pure (MkModule info1 tops1)

declareFun :: Pos -> Id.EVar -> Type Void -> FR ()
declareFun w fun typ = do
  dup <- uses declared (has (ix fun))
  when dup (throwAt w "duplicate declaration of function" fun)
  declared . at fun ?= (w, typ)

defineFun :: Bind In Void -> FR ()
defineFun (MkBind w fun NoType) = do
  ex <- uses declared (has (ix fun))
  unless ex (throwAt w "undeclared function" fun)
  dup <- use (defined . contains fun)
  when dup (throwAt w "duplicate definition of function" fun)
  defined . contains fun .= True

frTopLevel :: TopLevel In -> FR (TopLevel Out)
frTopLevel = \case
  TLTyp w tcs -> pure (TLTyp w tcs)
  TLVal w x t -> do
    declareFun w x t
    pure (TLVal w x t)
  TLDef d -> do
    defineFun (d^.defnLhs)
    pure (TLDef (retagDefn d))
  TLAsm b s -> do
    defineFun b
    pure (TLAsm (retagBind b) s)

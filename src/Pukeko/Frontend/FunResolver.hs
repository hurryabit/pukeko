module Pukeko.FrontEnd.FunResolver
  ( resolveModule
  ) where

import Pukeko.Prelude

import           Control.Lens
import qualified Data.Map      as Map

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type

type In  = St.TypeResolver
type Out = St.FunResolver

data FRState = MkFRState
  { _declared :: Map Id.EVar (Pos, Type Void)
  , _defined  :: Set Id.EVar
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
resolveModule (MkModule tops0) = do
  (tops1, MkFRState decld defnd) <- runFR (traverse frDecl tops0)
  let undefnd = decld `Map.difference` Map.fromSet id defnd
  case Map.minViewWithKey undefnd of
    Just ((fun, (w, _)), _) -> throwAt w "declared but undefined function" fun
    Nothing -> pure (MkModule tops1)

declareFun :: SignDecl -> FR ()
declareFun (MkSignDecl w fun typ) = do
  dup <- uses declared (has (ix fun))
  when dup (throwAt w "duplicate declaration of function" fun)
  declared . at fun ?= (w, typ)

defineFun :: Bind NoType Void -> FR ()
defineFun (MkBind w fun NoType) = do
  ex <- uses declared (has (ix fun))
  unless ex (throwAt w "undeclared function" fun)
  dup <- use (defined . contains fun)
  when dup (throwAt w "duplicate definition of function" fun)
  defined . contains fun .= True

frDecl :: Decl In -> FR (Decl Out)
frDecl = \case
  DType tcs -> pure (DType tcs)
  DSign s -> do
    declareFun s
    pure (DSign s)
  DDefn d -> do
    defineFun (d^.defn2bind)
    pure (DDefn (retagDefn d))
  DPrim p -> do
    defineFun (p^.prim2bind)
    pure (DPrim p)

-- TODO: Evaluate if this stage needs its own tag on the AST. Also consider
-- mergeing it with the type resolver. The result would be a stage which checks
-- the declare-define-use cycle of all names.
module Pukeko.FrontEnd.FunResolver
  ( resolveModule
  ) where

import Pukeko.Prelude

import           Control.Lens
import qualified Data.Map      as Map

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import qualified Pukeko.AST.Identifier as Id

type In  = St.TypeResolver
type Out = St.FunResolver

data FRState = MkFRState
  { _declared :: Map Id.EVar Pos
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
    Just ((fun, w), _) -> throwAt w "declared but undefined function" fun
    Nothing -> pure (MkModule tops1)

declareFun :: SignDecl tv -> FR ()
declareFun (MkSignDecl w fun _) = do
  dup <- uses declared (has (ix fun))
  when dup (throwAt w "duplicate declaration of function" fun)
  declared . at fun ?= w

defineFun :: Pos -> Id.EVar -> FR ()
defineFun w fun = do
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
  -- FIXME: Check that classes are declared only once and before their first
  -- instantiation. Check that no class/type constructor pair is instantiated
  -- twice.
  DClss (MkClssDecl w c v ms) -> do
    for_ ms $ \m -> declareFun m *> defineFun (m^.sign2pos) (m^.sign2func)
    pure (DClss (MkClssDecl w c v ms))
  DInst (MkInstDecl w c t qvs ds) -> do
    pure (DInst (MkInstDecl w c t qvs (map retagDefn ds)))
  DDefn d -> do
    let b = d^.defn2bind
    defineFun (b^.bind2pos) (b^.bind2evar)
    pure (DDefn (retagDefn d))
  DPrim p -> do
    let b = p^.prim2bind
    defineFun (b^.bind2pos) (b^.bind2evar)
    pure (DPrim p)

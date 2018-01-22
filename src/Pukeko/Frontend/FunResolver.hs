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

type In  = St.Renamer
type Out = St.Renamer

data FRState = MkFRState
  { _declared :: Map Id.EVar Pos
  , _defined  :: Set Id.EVar
  }
makeLenses ''FRState

newtype FR a = FR{unFR :: StateT FRState (HereT (Except Doc)) a}
  deriving ( Functor, Applicative, Monad
           , MonadState FRState
           , MonadError Doc
           , MonadHere
           )

runFR :: FR a -> Either Doc a
runFR fr = runExcept (runHereT (evalStateT (unFR fr) st0))
  where
    st0 = MkFRState mempty mempty

resolveModule :: Module In -> Either Doc (Module Out)
resolveModule (MkModule tops0) = runFR $ do
  tops1 <- traverseHeres frDecl tops0
  MkFRState decld defnd <- get
  let undefnd = decld `Map.difference` Map.fromSet id defnd
  case Map.minViewWithKey undefnd of
    Just ((fun, pos), _) ->
      here pos (throwHere ("declared but undefined function:" <+> pretty fun))
    Nothing -> pure (MkModule tops1)

declareFun :: SignDecl tv -> FR ()
declareFun (MkSignDecl fun _) = do
  dup <- uses declared (has (ix fun))
  when dup (throwHere ("duplicate declaration of function:" <+> pretty fun))
  pos <- where_
  declared . at fun ?= pos

defineFun :: Id.EVar -> FR ()
defineFun fun = do
  ex <- uses declared (has (ix fun))
  unless ex (throwHere ("undeclared function:" <+> pretty fun))
  dup <- use (defined . contains fun)
  when dup (throwHere ("duplicate definition of function:" <+> pretty fun))
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
  DClss (MkClssDecl c v ms) -> do
    for_ ms $ \m -> declareFun m *> defineFun (m^.sign2func)
    pure (DClss (MkClssDecl c v ms))
  DInst (MkInstDecl c t qvs ds) -> do
    pure (DInst (MkInstDecl c t qvs ds))
  DDefn d -> do
    defineFun (d^.defn2bind.bind2evar)
    pure (DDefn d)
  DPrim p -> do
    defineFun (p^.prim2bind.bind2evar)
    pure (DPrim p)

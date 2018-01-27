-- TODO: Evaluate if this stage needs its own tag on the AST. Also consider
-- mergeing it with the type resolver. The result would be a stage which checks
-- the declare-define-use cycle of all names.
module Pukeko.FrontEnd.FunResolver
  ( resolveModule
  ) where

import Pukeko.Prelude

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import qualified Pukeko.AST.Identifier as Id

type In  = St.Renamer
type Out = St.Renamer

data FRState = MkFRState
  { _declared :: Map Id.EVar SourcePos
  , _defined  :: Set Id.EVar
  }
makeLenses ''FRState

type FR = Eff [State FRState, Reader SourcePos, Error Doc]

runFR :: FR a -> Either Doc a
runFR = run . runError . runReader noPos . evalState st0
  where
    st0 = MkFRState mempty mempty

resolveModule :: Module In -> Either Doc (Module Out)
resolveModule (MkModule tops0) = runFR $ do
  tops1 <- (traverse . lctd) frDecl tops0
  MkFRState decld defnd <- get
  let undefnd = decld `Map.difference` Map.fromSet id defnd
  case Map.minViewWithKey undefnd of
    Just ((fun, pos), _) ->
      here pos (throwHere ("declared but undefined function:" <+> pretty fun))
    Nothing -> pure (MkModule tops1)

declareFun :: SignDecl tv -> FR ()
declareFun (MkSignDecl fun _) = do
  dup <- uses declared (Map.member fun)
  when dup (throwHere ("duplicate declaration of function:" <+> pretty fun))
  pos <- where_
  modifying declared (Map.insert fun pos)

defineFun :: Id.EVar -> FR ()
defineFun fun = do
  ex <- uses declared (Map.member fun)
  unless ex (throwHere ("undeclared function:" <+> pretty fun))
  dup <- uses defined (Set.member fun)
  when dup (throwHere ("duplicate definition of function:" <+> pretty fun))
  modifying defined (Set.insert fun)

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

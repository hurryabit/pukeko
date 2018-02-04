-- TODO: Evaluate if this stage needs its own tag on the AST. Also consider
-- mergeing it with the type resolver. The result would be a stage which checks
-- the declare-define-use cycle of all names.
module Pukeko.FrontEnd.FunResolver
  ( resolveModule
  ) where

import Pukeko.Prelude

import           Control.Monad.Extra
import qualified Data.Map      as Map
import qualified Data.Set      as Set

import           Pukeko.AST.SystemF
import           Pukeko.AST.Language
import qualified Pukeko.AST.Identifier as Id

type In  = Surface

data FRState = MkFRState
  { _declared :: Map Id.EVar SourcePos
  , _defined  :: Set Id.EVar
  }
makeLenses ''FRState

type FR = Eff [State FRState, Reader SourcePos, Error Failure]

runFR :: FR a -> Either Failure a
runFR = run . runError . runReader noPos . evalState st0
  where
    st0 = MkFRState mempty mempty

resolveModule :: Module In -> Either Failure ()
resolveModule (MkModule decls) = runFR $ do
  traverse_ frDecl decls
  MkFRState decld defnd <- get
  let undefnd = decld `Map.difference` Map.fromSet id defnd
  whenJust (Map.minViewWithKey undefnd) $ \((fun, pos), _) ->
    here_ pos (throwHere ("declared but undefined function:" <+> pretty fun))

declareFun :: SignDecl tv -> FR ()
declareFun = here' $ \(MkSignDecl (unlctd -> fun) _) -> do
  whenM (uses declared (Map.member fun)) $
    throwHere ("duplicate declaration of function:" <+> pretty fun)
  pos <- where_
  modifying declared (Map.insert fun pos)

defineFun :: Lctd Id.EVar -> FR ()
defineFun = here' $ \(unlctd -> fun) -> do
  unlessM (uses declared (Map.member fun)) $
    throwHere ("undeclared function:" <+> pretty fun)
  whenM (uses defined (Set.member fun)) $
    throwHere ("duplicate definition of function:" <+> pretty fun)
  modifying defined (Set.insert fun)

frDecl :: Decl In -> FR ()
frDecl = \case
  DType _ -> pure ()
  DSign s -> declareFun s
  -- FIXME: Check that classes are declared only once and before their first
  -- instantiation. Check that no class/type constructor pair is instantiated
  -- twice.
  DClss c -> for_ (_clss2mthds c) $ \m -> declareFun m *> defineFun (m^.sign2func)
  DInst _ -> pure ()
  DDefn d -> defineFun (d^.defn2bind.bind2evar)
  DExtn p -> defineFun (p^.extn2bind.bind2evar)

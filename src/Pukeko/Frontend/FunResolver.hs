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

type CanFR effs = Members [State FRState, Reader SourcePos, Error Failure] effs

resolveModule :: Member (Error Failure) effs => Module In -> Eff effs ()
resolveModule (MkModule decls) = runReader noPos . evalState st0 $ do
  traverse_ frDecl decls
  MkFRState decld defnd <- get
  let undefnd = decld `Map.difference` Map.fromSet id defnd
  whenJust (Map.minViewWithKey undefnd) $ \((fun, pos), _) ->
    here_ pos (throwHere ("declared but undefined function:" <+> pretty fun))
  where
    st0 = MkFRState mempty mempty

declareFun :: CanFR effs => SignDecl tv -> Eff effs ()
declareFun = here' $ \(MkSignDecl (unlctd -> fun) _) -> do
  whenM (uses declared (Map.member fun)) $
    throwHere ("duplicate declaration of function:" <+> pretty fun)
  pos <- where_
  modifying declared (Map.insert fun pos)

defineFun :: CanFR effs => Lctd Id.EVar -> Eff effs ()
defineFun = here' $ \(unlctd -> fun) -> do
  unlessM (uses declared (Map.member fun)) $
    throwHere ("undeclared function:" <+> pretty fun)
  whenM (uses defined (Set.member fun)) $
    throwHere ("duplicate definition of function:" <+> pretty fun)
  modifying defined (Set.insert fun)

frDecl :: CanFR effs => Decl In -> Eff effs ()
frDecl = \case
  DType _ -> pure ()
  DSign sign -> declareFun sign
  DFunc func -> defineFun (func^.func2name)
  DExtn extn -> defineFun (extn^.extn2name)
  -- FIXME: Check that classes are declared only once and before their first
  -- instantiation. Check that no class/type constructor pair is instantiated
  -- twice.
  DClss c -> for_ (_clss2methods c) $ \m -> declareFun m *> defineFun (m^.sign2name)
  DInst _ -> pure ()

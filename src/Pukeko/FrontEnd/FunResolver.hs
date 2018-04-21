{-# LANGUAGE TemplateHaskell #-}

-- TODO: Evaluate if this stage needs its own tag on the AST. Also consider
-- mergeing it with the type resolver. The result would be a stage which checks
-- the declare-define-use cycle of all names.
module Pukeko.FrontEnd.FunResolver
  ( resolveModule
  ) where

import Pukeko.Prelude

import           Control.Monad.Extra
import qualified Data.Map as Map
import qualified Data.Set as Set

import Pukeko.AST.Language
import Pukeko.AST.Name
import Pukeko.AST.SystemF

type In  = Surface

data FRState = MkFRState
  { _declared :: Map TmVar SourcePos
  , _defined  :: Set TmVar
  }
makeLenses ''FRState

type CanFR effs = Members [State FRState, Reader SourcePos, Error Failure] effs

resolveModule :: Member (Error Failure) effs => Module In -> Eff effs ()
resolveModule (MkModule decls) = evalState st0 $ do
  for_ decls $ \decl -> runReader (getPos decl) (frDecl decl)
  MkFRState decld defnd <- get
  let undefnd = decld `Map.difference` Map.fromSet id defnd
  whenJust (Map.minViewWithKey undefnd) $ \((fun, pos), _) ->
    throwAt pos ("declared but undefined function:" <+> pretty fun)
  where
    st0 = MkFRState mempty mempty

declareFun :: CanFR effs => SignDecl tv -> Eff effs ()
declareFun = here' $ \(MkSignDecl fun _) -> do
  whenM (uses declared (Map.member fun)) $
    throwHere ("duplicate declaration of function:" <+> pretty fun)
  pos <- where_
  modifying declared (Map.insert fun pos)

defineFun :: (CanFR effs, NameSpaceOf a ~ 'TmVar, HasName a) => a -> Eff effs ()
defineFun (nameOf -> fun) = here fun $ do
  unlessM (uses declared (Map.member fun)) $
    throwHere ("undeclared function:" <+> pretty fun)
  whenM (uses defined (Set.member fun)) $
    throwHere ("duplicate definition of function:" <+> pretty fun)
  modifying defined (Set.insert fun)

frDecl :: CanFR effs => Decl In -> Eff effs ()
frDecl = \case
  DType _ -> pure ()
  DSign sign -> declareFun sign
  DFunc func -> defineFun func
  DExtn extn -> defineFun extn
  -- FIXME: Check that classes are declared only once and before their first
  -- instantiation. Check that no class/type constructor pair is instantiated
  -- twice.
  DClss c -> for_ (_clss2methods c) $ \m -> declareFun m *> defineFun m
  DInst _ -> pure ()

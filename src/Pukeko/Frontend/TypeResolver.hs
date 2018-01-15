module Pukeko.FrontEnd.TypeResolver
  ( resolveModule
  ) where

import Pukeko.Prelude

import           Control.Lens

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import           Pukeko.AST.ConDecl
import           Pukeko.AST.Type
import qualified Pukeko.AST.Identifier as Id

type In  = St.Renamer
type Out = St.TypeResolver

data TRState = MkTRState
  { _st2tcons :: Map Id.TCon (Some1 TConDecl)
  , _st2dcons :: Map Id.DCon (Some1 (Pair1 TConDecl DConDecl))
  }
makeLenses ''TRState

newtype TR a = TR {unTR :: StateT TRState (Except String) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadState TRState
           )

evalTR :: MonadError String m => TR a -> m a
evalTR tr =
  let st = MkTRState mempty mempty
  in  runExcept (evalStateT (unTR tr) st)

-- TODO: Use proper terminology in error messages.
trType :: Pos -> Type tv -> TR (Type tv)
trType w = type2tcon $ \tcon -> do
  ex <- uses st2tcons (has (ix tcon))
  unless ex (throwAt w "unknown type cons" tcon)
  pure tcon

insertTCon :: TConDecl n -> TR ()
insertTCon tcon@MkTConDecl{_tcon2pos = w, _tcon2name = tname} = do
  old <- use (st2tcons . at tname)
  when (isJust old) $ throwAt w "duplicate type cons" tname
  st2tcons . at tname ?= Some1 tcon

insertDCon :: KnownNat n => TConDecl n -> DConDecl n -> TR ()
insertDCon tcon dcon@MkDConDecl{_dcon2pos = w, _dcon2name = dname} = do
  old <- use (st2dcons . at dname)
  when (isJust old) $ throwAt w "duplicate term cons" dname
  st2dcons . at dname ?= Some1 (Pair1 tcon dcon)

findDCon :: Pos -> Id.DCon -> TR Id.DCon
findDCon w dcon = do
  ex <- uses st2dcons (has (ix dcon))
  unless ex (throwAt w "unknown term cons" dcon)
  pure dcon

trDefn :: Defn In tv ev -> TR (Defn Out tv ev)
trDefn = itraverseOf defn2dcon findDCon

trDecl :: Decl In -> TR (Decl Out)
trDecl top = case top of
  DType tconDecls -> do
    for_ tconDecls (\(Some1 tcon) -> insertTCon tcon)
    for_ tconDecls $ \(Some1 tcon@MkTConDecl{_tcon2dcons = dconDecls}) -> do
      for_ dconDecls $ \dcon@MkDConDecl{_dcon2pos = w, _dcon2flds = flds} -> do
        for_ flds (trType w)
        insertDCon tcon dcon
    pure (DType tconDecls)
  DSign (MkSignDecl w x t)  -> DSign <$> MkSignDecl w x <$> trType w t
  -- FIXME: Resolve types in class declarations and instance definitions.
  DClss c -> pure (DClss c)
  DInst i -> DInst <$> inst2defn trDefn i
  DDefn d -> DDefn <$> trDefn d
  DPrim p -> pure (DPrim p)

resolveModule :: MonadError String m => Module In -> m (Module Out)
resolveModule m0 = evalTR (module2decls (traverse trDecl) m0)

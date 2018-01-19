{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.FrontEnd.Info
  ( MonadInfo (..)
  , info2tcons
  , info2dcons
  , info2signs
  , info2insts
  , InfoT
  , runInfoT
  , mapInfoT
  , lookupInfo
  , findInfo
  , typeOfDCon
  , liftCatch
  ) where

import Pukeko.Prelude

import           Control.Lens
import qualified Control.Monad.Trans.Reader  as Reader
import           Control.Monad.Signatures    (Catch)
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import qualified Data.Vector.Sized           as Vec

import           Pukeko.AST.SystemF
import           Pukeko.AST.Stage
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type


data ModuleInfo = MkModuleInfo
  { _info2tcons :: Map Id.TCon (Some1 TConDecl)
  , _info2dcons :: Map Id.DCon (Some1 (Pair1 TConDecl DConDecl))
  , _info2signs :: Map Id.EVar (SignDecl Void)
  , _info2insts :: Map (Id.Clss, Id.TCon) [QVar]
  }

class Monad m => MonadInfo m where
  askInfo :: m ModuleInfo

newtype InfoT m a = InfoT{unInfoT :: ReaderT ModuleInfo m a}
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadError e
           , MonadState s
           , MonadSupply s
           , MonadWriter w
           )

makeLenses ''ModuleInfo
makePrisms ''ModuleInfo

runInfoT :: (IsType (StageType st)) => InfoT m a -> Module st -> m a
runInfoT act = runReaderT (unInfoT act) . collectInfo

mapInfoT :: (m a -> n b) -> InfoT m a -> InfoT n b
mapInfoT f = InfoT . mapReaderT f . unInfoT

lookupInfo :: (MonadInfo m, Ord k) => Lens' ModuleInfo (Map k v) -> k -> m (Maybe v)
lookupInfo l k = Map.lookup k . view l <$> askInfo

findInfo :: (MonadInfo m, Ord k, Show k) => Lens' ModuleInfo (Map k v) -> k -> m v
findInfo l k = Map.findWithDefault (bugWith "findInfo" k) k . view l <$> askInfo

typeOfDCon :: MonadInfo m => Id.DCon -> m (Type Void)
typeOfDCon dcon = do
  Some1 (Pair1 tconDecl dconDecl) <- findInfo info2dcons dcon
  pure (typeOfDConDecl tconDecl dconDecl)

liftCatch :: Catch e m a -> Catch e (InfoT m) a
liftCatch f m h = InfoT $ Reader.liftCatch f (unInfoT m) (unInfoT . h)

collectInfo :: forall st. (IsType (StageType st)) => Module st -> ModuleInfo
collectInfo (MkModule tops) = foldFor tops $ \case
  DType tcons ->
    foldFor tcons $ \(Some1 tcon) ->
      let dis =
            foldFor (tcon^.tcon2dcons) $ \dcon ->
              item info2dcons (dcon^.dcon2name) (Some1 (Pair1 tcon dcon))
      in  item info2tcons (tcon^.tcon2name) (Some1 tcon) <> dis
  DSign s -> sign s
  DClss (MkClssDecl _ c v ms) ->
    foldFor ms $ \(MkSignDecl w z t0) ->
      let t1 = mkTUni (Vec.singleton (MkQVar (Set.singleton c) v)) t0
      in  sign (MkSignDecl w z t1)
  -- FIXME: Collect type class instances.
  DInst (MkInstDecl _ c t qvs _) -> item info2insts (c, t) (toList qvs)
  DDefn (MkDefn b _) -> signBind b
  DSupC (MkSupCDecl w z xs t _ _) -> sign (MkSignDecl w z (mkTUni xs t))
  DPrim (MkPrimDecl b _) -> signBind b
  where
    foldFor :: (Foldable t, Monoid m) => t a -> (a -> m) -> m
    foldFor = flip foldMap
    item :: (Ord k) => Lens' ModuleInfo (Map k v) -> k -> v -> ModuleInfo
    item l k v = set l (Map.singleton k v) mempty
    sign :: SignDecl Void -> ModuleInfo
    sign s = item info2signs (s^.sign2func) s
    signBind :: IsType ty => Bind ty Void -> ModuleInfo
    signBind (MkBind w z t) = maybe mempty (sign . MkSignDecl w z) (isType t)

instance Monad m => MonadInfo (InfoT m) where
  askInfo = InfoT ask

instance MonadInfo m => MonadInfo (ReaderT r m) where
  askInfo = lift askInfo

instance MonadInfo m => MonadInfo (SupplyT s m) where
  askInfo = lift askInfo

instance MonadReader r m => MonadReader r (InfoT m) where
  ask = lift ask
  local = mapInfoT . local
  reader = lift . reader

instance Monoid ModuleInfo where
  mempty = MkModuleInfo mempty mempty mempty mempty
  mi1 `mappend` mi2 =
    ((mi1^._MkModuleInfo) `mappend` (mi2^._MkModuleInfo))^.re _MkModuleInfo

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.FrontEnd.Info
  ( MonadInfo (..)
  , SomeInstDecl (..)
  , info2tcons
  , info2dcons
  , info2signs
  , info2clsss
  , info2mthds
  , info2insts
  , InfoT
  , runInfoT
   ,localInfo
  , mapInfoT
  , lookupInfo
  , findInfo
  , typeOfDCon
  , typeOfFunc
  , tconDeclInfo
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
  , _info2clsss :: Map Id.Clss ClssDecl
  , _info2mthds :: Map Id.EVar (ClssDecl, SignDecl (TFinScope 1 Void))
  , _info2insts :: Map (Id.Clss, Id.TCon) SomeInstDecl
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

data SomeInstDecl = forall st. SomeInstDecl (InstDecl st)

makeLenses ''ModuleInfo
makePrisms ''ModuleInfo

runInfoT :: (IsType (StageType st)) => InfoT m a -> Module st -> m a
runInfoT act = runReaderT (unInfoT act) . collectInfo

mapInfoT :: (m a -> n b) -> InfoT m a -> InfoT n b
mapInfoT f = InfoT . mapReaderT f . unInfoT

localInfo :: Monad m => ModuleInfo -> InfoT m a -> InfoT m a
localInfo mi = InfoT . local (<> mi) . unInfoT

lookupInfo :: (MonadInfo m, Ord k) => Lens' ModuleInfo (Map k v) -> k -> m (Maybe v)
lookupInfo l k = Map.lookup k . view l <$> askInfo

findInfo :: (HasCallStack, MonadInfo m, Ord k, Show k) => Lens' ModuleInfo (Map k v) -> k -> m v
findInfo l k = Map.findWithDefault (bugWith "findInfo" k) k . view l <$> askInfo

typeOfDCon :: (HasCallStack, MonadInfo m) => Id.DCon -> m (Type tv)
typeOfDCon dcon = do
  Some1 (Pair1 tconDecl dconDecl) <- findInfo info2dcons dcon
  pure (fmap absurd (typeOfDConDecl tconDecl dconDecl))

typeOfFunc :: (HasCallStack, MonadInfo m) => Id.EVar -> m (Type tv)
typeOfFunc func = fmap absurd . _sign2type <$> findInfo info2signs func

liftCatch :: Catch e m a -> Catch e (InfoT m) a
liftCatch f m h = InfoT $ Reader.liftCatch f (unInfoT m) (unInfoT . h)

itemInfo :: (Ord k) => Lens' ModuleInfo (Map k v) -> k -> v -> ModuleInfo
itemInfo l k v = set l (Map.singleton k v) mempty

tconDeclInfo :: Some1 TConDecl -> ModuleInfo
tconDeclInfo (Some1 tcon) =
  let dis =
        flip foldMap (tcon^.tcon2dcons) $ \dcon ->
          itemInfo info2dcons (dcon^.dcon2name) (Some1 (Pair1 tcon dcon))
  in  itemInfo info2tcons (tcon^.tcon2name) (Some1 tcon) <> dis

collectInfo :: forall st. (IsType (StageType st)) => Module st -> ModuleInfo
collectInfo (MkModule tops) = foldFor tops $ \case
  DType tcons -> foldMap tconDeclInfo tcons
  DSign s -> sign s
  DClss clss@(MkClssDecl _ c v ms) ->
    let mthds_info = foldFor ms $ \mthd@(MkSignDecl w z t0) ->
          let t1 = mkTUni (Vec.singleton (MkQVar (Set.singleton c) v)) t0
          in  sign (MkSignDecl w z t1) <> itemInfo info2mthds z (clss, mthd)
    in  itemInfo info2clsss c clss <> mthds_info
  DInst inst@(MkInstDecl _ c t _ _) -> itemInfo info2insts (c, t) (SomeInstDecl inst)
  DDefn (MkDefn b _) -> signBind b
  DSupC (MkSupCDecl w z xs t _ _) -> sign (MkSignDecl w z (mkTUni xs t))
  DPrim (MkPrimDecl b _) -> signBind b
  where
    foldFor :: (Foldable t, Monoid m) => t a -> (a -> m) -> m
    foldFor = flip foldMap
    -- FIXME: Detect multiple definitions.
    sign :: SignDecl Void -> ModuleInfo
    sign s = itemInfo info2signs (s^.sign2func) s
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
  mempty = MkModuleInfo mempty mempty mempty mempty mempty mempty
  MkModuleInfo a1 b1 c1 d1 e1 f1 `mappend` MkModuleInfo a2 b2 c2 d2 e2 f2 =
    MkModuleInfo
      (a1 `mappend` a2)
      (b1 `mappend` b2)
      (c1 `mappend` c2)
      (d1 `mappend` d2)
      (e1 `mappend` e2)
      (f1 `mappend` f2)

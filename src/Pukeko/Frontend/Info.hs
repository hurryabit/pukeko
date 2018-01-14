{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.FrontEnd.Info
  ( MonadInfo (..)
  , InfoT
  , runInfoT
  , mapInfoT
  , findTCon
  , findDCon
  , findSign
  , typeOfDCon
  , liftCatch
  ) where

import Pukeko.Prelude

import           Control.Lens
import qualified Control.Monad.Trans.Reader  as Reader
import           Control.Monad.Signatures    (Catch)
import qualified Data.Map                    as Map

import           Pukeko.AST.SystemF
import           Pukeko.AST.Stage
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type

class Monad m => MonadInfo m where
  allTCons :: m (Map Id.TCon (Some1 TConDecl))
  allDCons :: m (Map Id.DCon (Some1 (Pair1 TConDecl DConDecl)))
  allSigns :: m (Map Id.EVar SignDecl)


data ModuleInfo = MkModuleInfo
  { _info2tcons :: Map Id.TCon (Some1 TConDecl)
  , _info2dcons :: Map Id.DCon (Some1 (Pair1 TConDecl DConDecl))
  , _info2signs :: Map Id.EVar SignDecl
  }

newtype InfoT m a = InfoT{unInfoT :: ReaderT ModuleInfo m a}
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadError e
           , MonadState s
           , MonadSupply s
           , MonadWriter w
           )

makePrisms ''ModuleInfo



runInfoT :: (IsType (StageType st)) => InfoT m a -> Module st -> m a
runInfoT act = runReaderT (unInfoT act) . collectInfo

mapInfoT :: (m a -> n b) -> InfoT m a -> InfoT n b
mapInfoT f = InfoT . mapReaderT f . unInfoT

info :: Monad m => (ModuleInfo -> a) -> InfoT m a
info = InfoT . asks

findTCon :: (HasCallStack, MonadInfo m) => Id.TCon -> m (Some1 TConDecl)
findTCon tcon = Map.findWithDefault (bugWith "findTCon" tcon) tcon <$> allTCons

findDCon ::
  (HasCallStack, MonadInfo m) => Id.DCon -> m (Some1 (Pair1 TConDecl DConDecl))
findDCon dcon = Map.findWithDefault (bugWith "findDCon" dcon) dcon <$> allDCons

findSign :: (HasCallStack, MonadInfo m) => Id.EVar -> m SignDecl
findSign fun = Map.findWithDefault (bugWith "findFun" fun) fun <$> allSigns

typeOfDCon :: MonadInfo m => Id.DCon -> m (Type Void)
typeOfDCon dcon = do
  Some1 (Pair1 tconDecl dconDecl) <- findDCon dcon
  pure (typeOfDConDecl tconDecl dconDecl)

liftCatch :: Catch e m a -> Catch e (InfoT m) a
liftCatch f m h = InfoT $ Reader.liftCatch f (unInfoT m) (unInfoT . h)

collectInfo :: forall st. (IsType (StageType st)) => Module st -> ModuleInfo
collectInfo (MkModule tops) = foldMap f tops
  where
    f :: Decl st -> ModuleInfo
    f = \case
      DType ds -> foldMap g ds
        where
          g :: Some1 TConDecl -> ModuleInfo
          g (Some1 tcon@(MkTConDecl _ tname _ dcons)) =
            MkModuleInfo (Map.singleton tname (Some1 tcon)) (foldMap h dcons) mempty
            where
              h dcon@(MkDConDecl _ _ dname _ _) =
                Map.singleton dname (Some1 (Pair1 tcon dcon))
      DSign s -> sign (Just s)
      DDefn (MkDefn (MkBind w z t) _) -> sign (MkSignDecl w z <$> isType t)
      DSupC (MkSupCDecl w z xs t _ _) -> sign (Just (MkSignDecl w z (mkTUni xs t)))
      DPrim (MkPrimDecl (MkBind w z t) _) -> sign (MkSignDecl w z <$> isType t)
      where
        sign :: Maybe SignDecl -> ModuleInfo
        sign =
          MkModuleInfo mempty mempty . maybe mempty (\s -> Map.singleton (s^.sign2func) s)

instance Monad m => MonadInfo (InfoT m) where
  allTCons = info _info2tcons
  allDCons = info _info2dcons
  allSigns = info _info2signs

instance MonadInfo m => MonadInfo (ReaderT r m) where
  allTCons = lift allTCons
  allDCons = lift allDCons
  allSigns = lift allSigns

instance MonadReader r m => MonadReader r (InfoT m) where
  ask = lift ask
  local = mapInfoT . local
  reader = lift . reader

instance Monoid ModuleInfo where
  mempty = MkModuleInfo mempty mempty mempty
  mi1 `mappend` mi2 =
    ((mi1^._MkModuleInfo) `mappend` (mi2^._MkModuleInfo))^.re _MkModuleInfo

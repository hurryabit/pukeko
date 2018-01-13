{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.FrontEnd.Info
  ( MonadInfo (..)
  , InfoT
  , runInfoT
  , mapInfoT
  , findTCon
  , findDCon
  , findFun
  , typeOfDCon
  , liftCatch
  ) where

import           Control.Lens
import           Control.Monad.Error.Class
import           Control.Monad.State.Class
import           Control.Monad.Writer.Class
import           Control.Monad.Reader
import           Control.Monad.Supply
import qualified Control.Monad.Trans.Reader  as Reader
import           Control.Monad.Signatures    (Catch)
import           Data.CallStack
import qualified Data.Map                    as Map

import           Pukeko.Error
import           Pukeko.AST.SystemF
import           Pukeko.AST.Stage
import qualified Pukeko.AST.ConDecl    as Con
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type

class Monad m => MonadInfo m where
  allTCons :: m (Map.Map Id.TCon (Some1 Con.TConDecl))
  allDCons :: m (Map.Map Id.DCon (Some1 (Pair1 Con.TConDecl Con.DConDecl)))
  allFuns  :: m (Map.Map Id.EVar (Pos, Type Void))


data ModuleInfo = MkModuleInfo
  { _info2tcons :: Map.Map Id.TCon (Some1 Con.TConDecl)
  , _info2dcons :: Map.Map Id.DCon (Some1 (Pair1 Con.TConDecl Con.DConDecl))
  , _info2funs  :: Map.Map Id.EVar (Pos, Type Void)
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

findTCon :: (HasCallStack, MonadInfo m) => Id.TCon -> m (Some1 Con.TConDecl)
findTCon tcon = Map.findWithDefault (bugWith "findTCon" tcon) tcon <$> allTCons

findDCon ::
  (HasCallStack, MonadInfo m) => Id.DCon -> m (Some1 (Pair1 Con.TConDecl Con.DConDecl))
findDCon dcon = Map.findWithDefault (bugWith "findDCon" dcon) dcon <$> allDCons

findFun :: (HasCallStack, MonadInfo m) => Id.EVar -> m (Pos, Type Void)
findFun fun = Map.findWithDefault (bugWith "findFun" fun) fun <$> allFuns

typeOfDCon :: MonadInfo m => Id.DCon -> m (Type Void)
typeOfDCon dcon = do
  Some1 (Pair1 tconDecl dconDecl) <- findDCon dcon
  pure (Con.typeOf tconDecl dconDecl)

liftCatch :: Catch e m a -> Catch e (InfoT m) a
liftCatch f m h = InfoT $ Reader.liftCatch f (unInfoT m) (unInfoT . h)

collectInfo :: forall st. (IsType (StageType st)) => Module st -> ModuleInfo
collectInfo (MkModule tops) = foldMap f tops
  where
    f :: TopLevel st -> ModuleInfo
    f = \case
      TLTyp _ ds -> foldMap g ds
        where
          g :: Some1 Con.TConDecl -> ModuleInfo
          g (Some1 tcon@(Con.MkTConDecl tname _ dcons)) =
            MkModuleInfo (Map.singleton tname (Some1 tcon)) (foldMap h dcons) mempty
            where
              h dcon@(Con.MkDConDecl _ dname _ _) =
                Map.singleton dname (Some1 (Pair1 tcon dcon))
      TLVal w z t -> val w z (Just t)
      TLDef (MkDefn (MkBind w z t) _) -> val w z (isType t)
      TLSup w z xs t _ _ -> val w z (Just (mkTUni xs t))
      TLAsm (MkBind w z t) _ -> val w z (isType t)
      where
        val w z t_mb =
          MkModuleInfo mempty mempty (maybe mempty (Map.singleton z . (,) w) t_mb)

instance Monad m => MonadInfo (InfoT m) where
  allTCons = info _info2tcons
  allDCons = info _info2dcons
  allFuns  = info _info2funs

instance MonadInfo m => MonadInfo (ReaderT r m) where
  allTCons = lift allTCons
  allDCons = lift allDCons
  allFuns  = lift allFuns

instance MonadReader r m => MonadReader r (InfoT m) where
  ask = lift ask
  local = mapInfoT . local
  reader = lift . reader

instance Monoid ModuleInfo where
  mempty = MkModuleInfo mempty mempty mempty
  mi1 `mappend` mi2 =
    ((mi1^._MkModuleInfo) `mappend` (mi2^._MkModuleInfo))^.re _MkModuleInfo

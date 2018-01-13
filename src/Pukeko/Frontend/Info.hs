{-# LANGUAGE FunctionalDependencies #-}
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

import           Control.Monad.Error.Class
import           Control.Monad.State.Class
import           Control.Monad.Writer.Class
import           Control.Monad.Reader
import           Control.Monad.Supply
import qualified Control.Monad.Trans.Reader  as Reader
import           Control.Monad.Signatures    (Catch)
import qualified Data.Map                    as Map

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.ConDecl    as Con
import           Pukeko.AST.ModuleInfo as MI
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type

class Monad m => MonadInfo i m | m -> i where
  allTCons ::
    (i ~ GenModuleInfo 'True funs) =>
    m (Map.Map Id.TCon (Some1 Con.TConDecl))
  allDCons ::
    (i ~ GenModuleInfo 'True funs) =>
    m (Map.Map Id.DCon (Some1 (Pair1 Con.TConDecl Con.DConDecl)))
  allFuns  :: (i ~ GenModuleInfo cons 'True) => m (Map.Map Id.EVar (Pos, Type Void))

newtype InfoT i m a = InfoT{unInfoT :: ReaderT i m a}
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadError e
           , MonadState s
           , MonadSupply s
           , MonadWriter w
           )

runInfoT :: InfoT i m a -> i -> m a
runInfoT = runReaderT . unInfoT

mapInfoT :: (m a -> n b) -> InfoT i m a -> InfoT i n b
mapInfoT f = InfoT . mapReaderT f . unInfoT

info :: Monad m => (i -> a) -> InfoT i m a
info = InfoT . asks

findTCon :: MonadInfo (GenModuleInfo 'True funs) m => Id.TCon -> m (Some1 Con.TConDecl)
findTCon tcon = (Map.! tcon) <$> allTCons

findDCon ::
  MonadInfo (GenModuleInfo 'True funs) m =>
  Id.DCon -> m (Some1 (Pair1 Con.TConDecl Con.DConDecl))
findDCon tcon = (Map.! tcon) <$> allDCons

findFun :: MonadInfo (GenModuleInfo cons 'True) m => Id.EVar -> m (Pos, Type Void)
findFun fun = (Map.! fun) <$> allFuns

typeOfDCon :: MonadInfo (GenModuleInfo 'True funs) m => Id.DCon -> m (Type Void)
typeOfDCon dcon = do
  Some1 (Pair1 tconDecl dconDecl) <- findDCon dcon
  pure (Con.typeOf tconDecl dconDecl)

liftCatch :: Catch e m a -> Catch e (InfoT i m) a
liftCatch f m h = InfoT $ Reader.liftCatch f (unInfoT m) (unInfoT . h)

instance Monad m => MonadInfo i (InfoT i m) where
  allTCons = info MI.tcons
  allDCons = info MI.dcons
  allFuns  = info MI.funs

instance MonadInfo i m => MonadInfo i (ReaderT r m) where
  allTCons = lift allTCons
  allDCons = lift allDCons
  allFuns  = lift allFuns

instance MonadReader r m => MonadReader r (InfoT i m) where
  ask = lift ask
  local = mapInfoT . local
  reader = lift . reader
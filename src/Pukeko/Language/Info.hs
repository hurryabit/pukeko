{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.Language.Info
  ( MonadInfo (..)
  , InfoT
  , runInfoT
  , mapInfoT
  ) where

import           Control.Monad.Error.Class
import           Control.Monad.State.Class
import           Control.Monad.Reader
import qualified Control.Monad.Trans.Reader  as Reader
import           Control.Monad.Signatures    (Catch)

import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.AST.ConDecl    as Con
import           Pukeko.Language.AST.ModuleInfo as MI
import qualified Pukeko.Language.Ident          as Id

class Monad m => MonadInfo i m | m -> i where
  findTCon :: (i ~ GenModuleInfo 'True) => Id.TCon -> m Con.TConDecl
  findDCon :: (i ~ GenModuleInfo 'True) => Id.DCon -> m Con.DConDecl

newtype InfoT i m a = InfoT{unInfoT :: ReaderT i m a}
  deriving (Functor, Applicative, Monad, MonadTrans)

runInfoT :: InfoT i m a -> i -> m a
runInfoT = runReaderT . unInfoT

mapInfoT :: (m a -> n b) -> InfoT i m a -> InfoT i n b
mapInfoT f = InfoT . mapReaderT f . unInfoT

liftCatch :: Catch e m a -> Catch e (InfoT i m) a
liftCatch f m h = InfoT $ Reader.liftCatch f (unInfoT m) (unInfoT . h)

instance Monad m => MonadInfo i (InfoT i m) where
  findTCon tcon = InfoT $ asks (MI.getTCon tcon)
  findDCon dcon = InfoT $ asks (MI.getDCon dcon)

instance MonadReader r m => MonadReader r (InfoT i m) where
  ask = lift ask
  local = mapInfoT . local
  reader = lift . reader

instance MonadError e m => MonadError e (InfoT i m) where
  throwError = lift . throwError
  catchError = liftCatch catchError

instance MonadState s m => MonadState s (InfoT i m) where
  get = lift get
  put = lift . put
  state = lift . state

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.Language.ConInfo
  ( MonadConInfo (..)
  , ConInfoT
  , Con.ConDecls
  , runConInfoT
  , mapConInfoT
  ) where

import           Control.Monad.Error.Class
import           Control.Monad.State.Class
import           Control.Monad.Reader
import qualified Control.Monad.Trans.Reader  as Reader
import           Control.Monad.Signatures    (Catch)

import qualified Pukeko.Language.AST.ConDecl as Con
import qualified Pukeko.Language.Ident       as Id

class Monad m => MonadConInfo m where
  findTCon :: Id.TCon -> m Con.TConDecl
  findDCon :: Id.DCon -> m Con.DConDecl

newtype ConInfoT m a = ConInfoT{unConInfoT :: ReaderT Con.ConDecls m a}
  deriving (Functor, Applicative, Monad, MonadTrans)

runConInfoT :: ConInfoT m a -> Con.ConDecls -> m a
runConInfoT = runReaderT . unConInfoT

mapConInfoT :: (m a -> n b) -> ConInfoT m a -> ConInfoT n b
mapConInfoT f = ConInfoT . mapReaderT f . unConInfoT

liftCatch :: Catch e m a -> Catch e (ConInfoT m) a
liftCatch f m h = ConInfoT $ Reader.liftCatch f (unConInfoT m) (unConInfoT . h)

instance Monad m => MonadConInfo (ConInfoT m) where
  findTCon tcon = ConInfoT $ asks (Con.getTCon tcon)
  findDCon dcon = ConInfoT $ asks (Con.getDCon dcon)

instance MonadReader r m => MonadReader r (ConInfoT m) where
  ask = lift ask
  local = mapConInfoT . local
  reader = lift . reader

instance MonadError e m => MonadError e (ConInfoT m) where
  throwError = lift . throwError
  catchError = liftCatch catchError

instance MonadState s m => MonadState s (ConInfoT m) where
  get = lift get
  put = lift . put
  state = lift . state

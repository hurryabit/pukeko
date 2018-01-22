{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Pukeko.Prelude
  ( module X

  , Doc
  , Pretty (..)
  , (<+>)
  , pretty
  , prettyShow
  , shown
  , text
  , render

  , Pos
  , mkPos
  , noPos

  , MonadHere (..)
  , HereT
  , runHereT
  , mapHereT
  , throwHere

  , Loc (..)
  , foldHere
  , traverseHere
  , forHere
  , fmapHeres
  , traverseHeres
  , forHeres
  , traverseHeres_
  , forHeres_

  , HereTraversal
  , HereTraversal'
  , overHere
  , unhere

  , bug
  , bugWith
  ) where

import Prelude               as X

import Control.Applicative   as X
import Control.Lens          as X
       ( Iso, Lens, Lens', Traversal, Traversal', Prism, Prism'
       , (^.), (.~), (%~)
       , over, view, views
       , iso, prism
       , iall, ifoldMap, imap, itoList, itraverse
       , makeLenses, makePrisms
       )
import Control.Monad         as X
import Control.Monad.Except  as X
import Control.Monad.Reader  as X
import Control.Monad.RWS     as X
import Control.Monad.State   as X
import Control.Monad.Supply  as X
import Control.Monad.Writer  as X

import Data.Bifunctor        as X
import Data.CallStack        as X (HasCallStack)
import Data.DList            as X (DList)
import Data.Either           as X (partitionEithers)
import Data.Foldable         as X
import Data.Function         as X
import Data.Functor.Identity as X
import Data.List             as X (sort)
import Data.Map              as X (Map)
import Data.Maybe            as X (catMaybes, isJust, isNothing, mapMaybe)
import Data.Monoid           as X (Monoid (..), (<>))
import Data.List.NonEmpty    as X (NonEmpty (..))
import Data.Proxy            as X (Proxy (..))
import Data.Sequence         as X (Seq)
import Data.Set              as X (Set)
import Data.Set.Lens         as X (setOf)
import Data.Traversable      as X
import Data.Type.Equality    as X ((:~:) (Refl))
import Data.Void             as X (Void, absurd)

import GHC.TypeLits          as X ( type (+), type (<=?), Nat, KnownNat
                                  , natVal, sameNat
                                  )

import Data.Finite           as X (Finite)
import Data.Vector.Sized     as X (Vector)

import qualified Text.PrettyPrint.HughesPJClass as PP
import           Text.PrettyPrint.HughesPJClass ( Doc, Pretty (..)
                                                , colon, prettyShow, text, render
                                                )
import           Text.Parsec                    (SourcePos)

infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
(<+>) = (PP.<+>)
{-# INLINE (<+>) #-}

pretty :: Pretty a => a -> Doc
pretty = pPrint

shown :: (Show a) => a -> Doc
shown = text . show

newtype Pos = Pos (Maybe SourcePos)

mkPos :: SourcePos -> Pos
mkPos = Pos . Just

noPos :: Pos
noPos = Pos Nothing

class (Applicative m) => MonadHere m where
  where_ :: m Pos
  here :: Pos -> m a -> m a

newtype HereT m a = HereT{unHereT :: Pos -> m a}
  deriving (Functor)

instance (Applicative m) => MonadHere (HereT m) where
  where_ = HereT pure
  here pos = HereT . local (const pos) . unHereT

-- FIXME: Pass in 'Pos' rather than fix it to 'noPos'.
runHereT :: HereT m a -> m a
runHereT m = unHereT m noPos

mapHereT :: (m a -> n b) -> HereT m a -> HereT n b
mapHereT f mx = HereT $ f . unHereT mx

throwHere :: (MonadHere m, MonadError Doc m) => Doc -> m a
throwHere msg = do
  pos <- where_
  throwError (pretty pos <> colon <+> msg)

data Loc a = Loc{pos :: Pos, unloc :: a}
  deriving (Show, Foldable, Functor, Traversable)

foldHere :: (MonadHere m) => (a -> m b) -> Loc a -> m b
foldHere f (Loc pos thing) = here pos (f thing)

traverseHere :: (MonadHere m) => (a -> m b) -> Loc a -> m (Loc b)
traverseHere f (Loc pos thing) = here pos (Loc pos <$> f thing)

fmapHeres :: (Functor f) => (a -> b) -> f (Loc a) -> f (Loc b)
fmapHeres = fmap . fmap

traverseHeres ::
  (MonadHere m, Traversable t) => (a -> m b) -> t (Loc a) -> m (t (Loc b))
traverseHeres = traverse . traverseHere

traverseHeres_ :: (MonadHere m, Foldable t) => (a -> m ()) -> t (Loc a) -> m ()
traverseHeres_ = traverse_ . foldHere

forHere :: (MonadHere m) => Loc a -> (a -> m b) -> m (Loc b)
forHere = flip traverseHere

forHeres :: (MonadHere m, Traversable t) => t (Loc a) -> (a -> m b) -> m (t (Loc b))
forHeres = flip traverseHeres

forHeres_ :: (MonadHere m, Foldable t) => t (Loc a) -> (a -> m ()) -> m ()
forHeres_ = flip traverseHeres_

type HereTraversal s t a b = forall m. (MonadHere m) => (a -> m b) -> s -> m t

type HereTraversal' s a = HereTraversal s s a a

overHere :: HereTraversal s t a b -> (a -> b) -> (s -> t)
overHere t f = runIdentity . runHereT . t (pure . f)

unhere :: HereTraversal s t a b -> Traversal s t a b
unhere t f = runHereT . t (HereT . const . f)

bug :: HasCallStack => String -> a
bug msg = error ("BUG! " ++ msg)

bugWith :: (HasCallStack, Show b) => String -> b -> a
bugWith msg x = bug (msg ++ " (" ++ show x ++ ")")

instance Pretty Pos where
  pPrintPrec _ _ (Pos p) = maybe "no position" (text . show) p

instance Show Pos where
  show = prettyShow

instance Pretty a => Pretty (Loc a) where
  pPrintPrec lvl prec = pPrintPrec lvl prec . unloc

instance (Applicative m) => Applicative (HereT m) where
  pure = HereT . const . pure
  mf <*> mx = HereT $ \pos -> unHereT mf pos <*> unHereT mx pos

instance (Monad m) => Monad (HereT m) where
  mx >>= f = HereT $ \pos -> unHereT mx pos >>= \x -> unHereT (f x) pos

instance MonadTrans HereT where
  lift = HereT . const

instance (Monad m, MonadHere m) => MonadHere (ReaderT r m) where
  where_ = lift where_
  here = mapReaderT . here

instance (Monad m, MonadHere m) => MonadHere (StateT s m) where
  where_ = lift where_
  here = mapStateT . here

instance (Monad m, MonadHere m, Monoid w) => MonadHere (RWST r w s m) where
  where_ = lift where_
  here = mapRWST . here

instance (MonadReader r m) => MonadReader r (HereT m) where
  ask = lift ask
  local = mapHereT . local
  reader = lift . reader

instance (MonadWriter w m) => MonadWriter w (HereT m) where
  writer = lift . writer
  tell = lift . tell
  listen = mapHereT listen
  pass = mapHereT pass

instance (MonadState s m) => MonadState s (HereT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance (MonadError e m) => MonadError e (HereT m) where
  throwError = lift . throwError
  mx `catchError` h =
    HereT $ \pos -> unHereT mx pos `catchError` \e -> unHereT (h e) pos

instance (MonadSupply s m) => MonadSupply s (HereT m) where
  fresh = lift fresh
  unfresh = lift . unfresh
  reset = lift reset
  resetWith = lift . resetWith

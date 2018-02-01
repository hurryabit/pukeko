{-# LANGUAGE DefaultSignatures #-}
module Pukeko.AST.Functor2
  ( Functor2 (..)
  , Traversable2 (..)
  , Base2
  , Recursive2 (..)
  , Corecursive2 (..)
  , cata2
  , cataM2
  , cataM2'
  , ana2
  )
where

import Pukeko.Prelude

import Control.Natural
import Data.Functor.Compose

class Functor2 φ where
  fmap2 :: (s ~> t) -> φ s ~> φ t
  default fmap2 :: Traversable2 φ => (s ~> t) -> φ s ~> φ t
  fmap2 f = runIdentity . traverse2 (Identity . f)

class Traversable2 φ where
  traverse2 :: Applicative f => (forall a. s a -> f (t a)) -> φ s a -> f (φ t a)

type family Base2 (f :: * -> *) :: (* -> *) -> * -> *

class Functor2 (Base2 f) => Recursive2 f where
  project2 :: f a -> Base2 f f a

class Functor2 (Base2 f) => Corecursive2 f where
  embed2 :: Base2 f f a -> f a

cata2 :: Recursive2 f => (Base2 f s ~> s) -> f ~> s
cata2 f = f . fmap2 (cata2 f) . project2

cataM2 ::
  (Recursive2 f, Traversable2 (Base2 f), Monad m) =>
  (forall a. Base2 f s a -> m (s a)) -> f a -> m (s a)
cataM2 f = getCompose . cataM2' (Compose . f)

cataM2' ::
  (Recursive2 f, Traversable2 (Base2 f), Monad m) =>
  (Base2 f s ~> Compose m s) -> f ~> Compose m s
cataM2' f x =
  Compose $ traverse2 getCompose (fmap2 (cataM2' f) (project2 x)) >>= getCompose . f

-- f :: Base2 f s ~> Compose m s
-- x :: f a0
-- project2 x :: Base2 f f a0
-- cataA2 f :: f ~> Compose m s
-- fmap2 (cataA2 f) :: Base2 f f ~> Base2 f (Compose m s)
-- fmap2 (cataA2 f) (proect2 x) :: Base2 f (Compose m s) a0
--
--
--

ana2 :: Corecursive2 f => (s ~> Base2 f s) -> s ~> f
ana2 f = embed2 . fmap2 (ana2 f) . f


data Var v = B | F v
  deriving (Eq, Ord, Foldable, Functor, Traversable)

data Expr v = Var v | App (Expr v) (Expr v) | Lam (Expr (Var v))
  deriving (Foldable, Functor, Traversable)

data ExprF expr v = VarF v | AppF (expr v) (expr v) | LamF (expr (Var v))
  deriving (Foldable, Functor, Traversable)

type instance Base2 Expr = ExprF

instance Recursive2 Expr where
  project2 = \case
    Var v     -> VarF v
    App e1 e2 -> AppF e1 e2
    Lam e0    -> LamF e0

instance Corecursive2 Expr where
  embed2 = \case
    VarF v     -> Var v
    AppF e1 e2 -> App e1 e2
    LamF e0    -> Lam e0

instance Functor2 ExprF
instance Traversable2 ExprF where
  traverse2 f = \case
    VarF v     -> pure (VarF v)
    AppF e1 e2 -> AppF <$> f e1 <*> f e2
    LamF e0    -> LamF <$> f e0

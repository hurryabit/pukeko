{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.Prelude
  ( module X

  , Doc
  , Pretty (..)
  , PrettyPrec (..)
  , (<+>)
  -- , shown
  -- , text
  , render

  , local'

  , SourcePos
  , noPos

  , Failure
  , throwFailure

  , Where (..)
  , throwHere
  , WhereLens
  , WhereLens'
  , WhereTraversal
  , WhereTraversal'
  , unWhere

  , Lctd (..)
  , lctd
  , lctd_

  , bug
  , bugWith
  ) where

import Prelude               as X

import Control.Applicative   as X
import Control.Lens          as X
       ( Iso, Lens, Lens', Traversal, Traversal', Prism, Prism'
       , (^.), (.~), (%~)
       , over, set
       , iso, prism, to
       , iall, ifoldMap, imap, itoList, itraverse
       , makeLenses, makePrisms
       )
import Control.Lens.Freer         as X
import Control.Monad              as X
import Control.Monad.Freer        as X
import Control.Monad.Freer.Error  as X
import Control.Monad.Freer.Reader as X
import Control.Monad.Freer.State  as X
import Control.Monad.Freer.Writer as X

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
import Data.List.NonEmpty    as X (NonEmpty (..))
import Data.Proxy            as X (Proxy (..))
import Data.Sequence         as X (Seq)
import Data.Semigroup        as X (Monoid (..), Semigroup (..))
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

import           Data.Functor.Compose           (Compose (..))
import qualified Text.PrettyPrint.Annotated     as PP
import           Text.PrettyPrint.Annotated     (Doc, render)
import           Text.Megaparsec.Pos            (SourcePos, initialPos, sourcePosPretty)

type Failure = Doc ()

throwFailure :: (Member (Error Failure) effs) => Failure -> Eff effs a
throwFailure = throwError

infixr 6 <+>

(<+>) :: Doc ann -> Doc ann -> Doc ann
(<+>) = (PP.<+>)
{-# INLINE (<+>) #-}

class Pretty a where
  pretty :: a -> Doc ann

class Pretty a => PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann

-- shown :: (Show a) => a -> Doc
-- shown = text . show

noPos :: SourcePos
noPos = initialPos ""

class Where f where
  where_ :: f SourcePos
  here :: SourcePos -> f a -> f a

instance (Member (Reader SourcePos) effs) => Where (Eff effs) where
  where_ = ask
  here pos = local (const pos)

instance (Applicative f) => Where (Compose ((->) SourcePos) f) where
  where_ = Compose pure
  here pos (Compose f) = Compose (const (f pos))

throwHere :: (Members [Reader SourcePos, Error Failure] effs) => Failure -> Eff effs a
throwHere msg = do
  pos <- where_
  throwFailure (pretty pos <> ":" <+> msg)

type WhereLens s t a b =
  forall f. (Functor f, Where f) => (a -> f b) -> s -> f t

type WhereLens' s a = WhereLens s s a a

type WhereTraversal s t a b =
  forall f. (Applicative f, Where f) => (a -> f b) -> s -> f t

type WhereTraversal' s a = WhereTraversal s s a a

unWhere :: WhereTraversal s t a b -> Traversal s t a b
unWhere t f s = getCompose (t (Compose . const . f) s) noPos

data Lctd a = Lctd{pos :: SourcePos, unlctd :: a}
  deriving (Show, Foldable, Functor, Traversable)

lctd :: WhereLens (Lctd a) (Lctd b) a b
lctd f (Lctd pos thng) = Lctd pos <$> here pos (f thng)

lctd_ :: (Where f) => (a -> f b) -> Lctd a -> f b
lctd_ f (Lctd pos thng) = here pos (f thng)

bug :: HasCallStack => String -> a
bug msg = error ("BUG! " ++ msg)

bugWith :: (HasCallStack, Show b) => String -> b -> a
bugWith msg x = bug (msg ++ " (" ++ show x ++ ")")

local' :: (r1 -> r2) -> Eff (Reader r2 : effs) a -> Eff (Reader r1 : effs) a
local' f = reinterpret (\Ask -> asks f)

instance Pretty String where
  pretty = PP.text

instance Pretty Int where
  pretty = PP.int

instance Pretty Void where
  pretty = absurd

instance Pretty SourcePos where
  pretty = pretty . sourcePosPretty

instance Pretty a => Pretty (Lctd a) where
  pretty = pretty . unlctd

instance PrettyPrec a => PrettyPrec (Lctd a) where
  prettyPrec prec = prettyPrec prec . unlctd

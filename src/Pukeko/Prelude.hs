{-# LANGUAGE DefaultSignatures #-}
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
  , renderFailure

  , HasPos (..)
  , Where (..)
  , here
  , here'
  , throwHere
  , WhereLens
  , WhereLens'
  , WhereTraversal
  , WhereTraversal'

  , Lctd (..)
  , lctd

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
       , _1, _2, _3, _Right
       , forOf_, toListOf, traverseOf_
       , iall, ifoldMap, ifor, ifor_, imap, itoList, itraverse
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
import Data.Functor          as X
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
import Data.Tagged           as X
import Data.Traversable      as X
import Data.Vector           as X (Vector)
import Data.Void             as X (Void, absurd)

import           Data.Aeson
import           Data.Functor.Compose           (Compose (..))
import qualified Text.PrettyPrint.Annotated     as PP
import           Text.PrettyPrint.Annotated     (Doc, render)
import           Text.Megaparsec.Pos            (SourcePos, initialPos, sourcePosPretty)

import           Pukeko.Orphans ()

type Failure = Doc ()

throwFailure :: (Member (Error Failure) effs) => Failure -> Eff effs a
throwFailure = throwError

renderFailure :: Failure -> String
renderFailure = render

infixr 6 <+>

(<+>) :: Doc ann -> Doc ann -> Doc ann
(<+>) = (PP.<+>)
{-# INLINE (<+>) #-}

class Pretty a where
  pretty :: a -> Doc ann
  default pretty :: PrettyPrec a => a -> Doc ann
  pretty = prettyPrec 0

class Pretty a => PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann

-- shown :: (Show a) => a -> Doc
-- shown = text . show

noPos :: SourcePos
noPos = initialPos ""
class HasPos a where
  getPos :: a -> SourcePos

class Where f where
  where_ :: f SourcePos
  here_ :: SourcePos -> f a -> f a

instance (Member (Reader SourcePos) effs) => Where (Eff effs) where
  where_ = ask
  here_ pos = local (const pos)

instance (Applicative f) => Where (Compose ((->) SourcePos) f) where
  where_ = Compose pure
  here_ pos (Compose f) = Compose (const (f pos))

here :: (Where f, HasPos a) => a -> f b -> f b
here = here_ . getPos

here' :: (Where f, HasPos a) => (a -> f b) -> a -> f b
here' f x = here x (f x)

type CanThrowHere effs = Members [Reader SourcePos, Error Failure] effs

throwHere :: CanThrowHere effs => Failure -> Eff effs a
throwHere msg = do
  pos <- where_
  throwFailure (pretty pos <> ":" <+> msg)

type WhereLens s t a b =
  forall f. (Functor f, Where f) => (a -> f b) -> s -> f t

type WhereLens' s a = WhereLens s s a a

type WhereTraversal s t a b =
  forall f. (Applicative f, Where f) => (a -> f b) -> s -> f t

type WhereTraversal' s a = WhereTraversal s s a a

data Lctd a = Lctd{pos :: SourcePos, unlctd :: a}
  deriving (Show, Foldable, Functor, Traversable)

lctd :: Lens (Lctd a) (Lctd b) a b
lctd f (Lctd pos thng) = Lctd pos <$> f thng

bug :: HasCallStack => String -> a
bug msg = error ("BUG! " ++ msg)

bugWith :: (HasCallStack, Show b) => String -> b -> a
bugWith msg x = bug (msg ++ " (" ++ show x ++ ")")

local' :: (r1 -> r2) -> Eff (Reader r2 : effs) a -> Eff (Reader r1 : effs) a
local' f = reinterpret (\Ask -> asks f)

instance HasPos (Lctd a) where
  getPos = pos

instance Pretty String where
  pretty = PP.text

instance Pretty Int where
  pretty = PP.int

instance Pretty a => Pretty (Tagged tag a) where
  pretty = pretty . untag

instance Pretty Void where
  pretty = absurd

instance Pretty SourcePos where
  pretty = pretty . sourcePosPretty

instance Pretty a => Pretty (Lctd a) where
  pretty = pretty . unlctd

instance PrettyPrec a => PrettyPrec (Lctd a) where
  prettyPrec prec = prettyPrec prec . unlctd

instance ToJSON a => ToJSON (Lctd a) where
  toJSON = toJSON . unlctd

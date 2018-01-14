module Pukeko.Prelude
  ( module X

  , runExcept
  , runExceptT

  , Doc
  , Pretty (..)
  , (<+>)
  , pretty
  , prettyShow
  , text

  , Pos
  , mkPos
  , noPos
  , HasPos (..)

  , throwDoc
  , throwDocAt
  , throwErrorAt
  , throw
  , throwAt
  , bug
  , bugWith
  , here
  ) where

import Prelude               as X

import Control.Applicative   as X
import Control.Lens          as X
       ( Iso, Lens, Lens', Traversal, Traversal', Prism, Prism'
       , (^.), over
       , iso, prism
       , iall, ifoldMap, itoList, itraverse
       , makeLenses, makePrisms
       )
import Control.Monad         as X
import Control.Monad.Except  as X hiding (runExcept, runExceptT)
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
import Data.Functor.Identity as X
import Data.List             as X (sort)
import Data.Map              as X (Map)
import Data.Maybe            as X (catMaybes, isJust, isNothing, mapMaybe)
import Data.Monoid           as X (Monoid (..), (<>))
import Data.List.NonEmpty    as X (NonEmpty (..))
import Data.Proxy            as X (Proxy (..))
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

import qualified Control.Monad.Except as Except
import qualified Text.PrettyPrint.HughesPJClass as PP
import           Text.PrettyPrint.HughesPJClass ( Doc, Pretty (..)
                                                , colon, prettyShow, quotes, render, text
                                                )
import           Text.Parsec                    (SourcePos)

infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
(<+>) = (PP.<+>)
{-# INLINE (<+>) #-}

pretty :: Pretty a => a -> Doc
pretty = pPrint

newtype Pos = Pos (Maybe SourcePos)

mkPos :: SourcePos -> Pos
mkPos = Pos . Just

noPos :: Pos
noPos = Pos Nothing

class HasPos a where
  pos :: Lens' a Pos

throwDoc :: MonadError String m => Doc -> m a
throwDoc = throwError . render

throwDocAt :: MonadError String m => Pos -> Doc -> m a
throwDocAt posn msg = throwDoc $ pretty posn <> colon <+> msg

throwErrorAt :: MonadError String m => Pos -> String -> m a
throwErrorAt posn = throwDocAt posn . text

throw :: (MonadError String m, Pretty a) => String -> a -> m b
throw thing name = throwDoc $ text thing <+> quotes (pretty name)

throwAt :: (MonadError String m, Pretty a)
        => Pos -> String -> a -> m b
throwAt posn thing name = throwDocAt posn $ text thing <+> quotes (pretty name)

bug :: HasCallStack => String -> a
bug msg = error ("BUG! " ++ msg)

bugWith :: (HasCallStack, Show b) => String -> b -> a
bugWith msg x = bug (msg ++ " (" ++ show x ++ ")")

runExcept :: MonadError e m => Except e a -> m a
runExcept = either throwError return . Except.runExcept

runExceptT :: (MonadError e m, Monad n) => ExceptT e n a -> n (m a)
runExceptT = fmap (either throwError return) . Except.runExceptT

here :: MonadError String m => Pos -> m a -> m a
here pos act = act `catchError` throwErrorAt pos

instance Pretty Pos where
  pPrintPrec _ _ (Pos p) = maybe "no position" (text . show) p

instance Show Pos where
  show = prettyShow

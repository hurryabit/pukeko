{-# LANGUAGE UndecidableInstances #-}
module Pukeko.Prelude
  ( module X

  , Failure
  , CanThrowHere
  , throwFailure
  , renderFailure

  , throwAt
  , throwHere

  , bug
  , bugWith
  , assert
  , assertM
  , impossible
  , traceJSON
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
import Data.List.Extra       as X (zipFrom, zipWithFrom)
import Data.List.NonEmpty    as X (NonEmpty (..))
import Data.Proxy            as X (Proxy (..))
import Data.Sequence         as X (Seq)
import Data.Semigroup        as X (Monoid (..), Semigroup (..))
import Data.Set              as X (Set)
import Data.Set.Lens         as X (setOf)
import Data.Tagged           as X
import Data.Traversable      as X
import Data.Vector           as X (Vector)
import Data.Void             as X
import Safe.Exact            as X (zipExact, zipWithExact)

import Pukeko.AST.Pos        as X
import Pukeko.Pretty         as X (pretty, (<+>), (<:~>))

import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Debug.Trace

import           Pukeko.Pretty
import           Pukeko.Orphans ()

type Failure = Doc ()

throwFailure :: (Member (Error Failure) effs) => Failure -> Eff effs a
throwFailure = throwError

renderFailure :: Failure -> String
renderFailure = render

throwAt :: (Member (Error Failure) effs, HasPos x) => x -> Failure -> Eff effs a
throwAt x msg = throwFailure (pretty (getPos x) <:~> msg)

type CanThrowHere effs = Members [Reader SourcePos, Error Failure] effs

throwHere :: CanThrowHere effs => Failure -> Eff effs a
throwHere msg = do
  pos <- where_
  throwAt pos msg

bug :: HasCallStack => String -> a
bug msg = error ("BUG! " ++ msg)

bugWith :: (HasCallStack, Show b) => String -> b -> a
bugWith msg x = bug (msg ++ " (" ++ show x ++ ")")

assert :: HasCallStack => Bool -> a -> a
assert True  = id
assert False = error "ASSERTION FAILED!"

assertM :: (HasCallStack, Applicative m) => Bool -> m ()
assertM True  = pure ()
assertM False = error "ASSERTION FAILED!"

impossible :: HasCallStack => a
impossible = error "IMPOSSIBLE CODE REACHED!"

traceJSON :: ToJSON a => a -> b -> b
traceJSON =
  let config = Aeson.defConfig{Aeson.confIndent = Aeson.Spaces 2}
  in  trace . BSL.unpack . Aeson.encodePretty' config . toJSON

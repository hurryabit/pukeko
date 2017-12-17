{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- | This module provides types and functions used by many ASTs.
module Pukeko.Language.Base.AST
  ( -- * Position in the input file
    Pos
    -- * Usefull type classes
  , HasPos (..)
  , HasLhs (..)
  , HasRhs (..)
  -- , HasLhs1 (..)
  , HasRhs1 (..)
  , HasRhs2 (..)

    -- * Standard types for certain ASTs
  , StdDefn (MkDefn)
  , StdAltn (MkAltn)
  , altnRhs
  , mapAltnRhs
  , StdPatn (..)
  , pattern Simp
  , _Bind
  , _Dest
  , patnToBind
  , patnBind
  , Bind (..)
  , _Wild
  , _Name
  , bindName

    -- * Type safe de Bruijn indices
  , Scope (..)
  , FinScope
  , _Bound
  , _Free
  , bound
  , free
  , strengthen
  , weaken1
  , abstract1
  , unscope
  , IsVar (..)

    -- * Common types
  , Finite
  , Vector
  , KnownNat
  )
  where

import           Control.Lens
import           Data.Finite       (Finite)
import           Data.Foldable     (toList)
import           Data.Forget
import           Data.Vector.Sized (Vector)
import           GHC.TypeLits      (KnownNat)
import           Text.Parsec       (SourcePos)

import           Pukeko.Error      (bug)
import           Pukeko.Pretty
import qualified Pukeko.Language.Ident as Id

type Pos = SourcePos

class HasPos a where
  pos :: Lens' a Pos

class HasLhs a where
  type Lhs a
  lhs :: Lens' a (Lhs a)

class HasRhs a where
  type Rhs a
  rhs :: Lens' a (Rhs a)

-- class HasLhs1 (t :: * -> *) where
--   type Lhs1 t :: * -> *
--   lhs1 :: Lens (t a) (t b) (Lhs1 t a) (Lhs1 t b)

class HasRhs1 (t :: * -> *) where
  type Rhs1 t :: * -> *
  rhs1 :: Lens (t a) (t b) (Rhs1 t a) (Rhs1 t b)

class HasRhs2 (t :: (* -> *) -> * -> *) where
  rhs2 :: Lens (t f a) (t g b) (f a) (g b)

data StdDefn (expr :: * -> *) v = MkDefn
  { _defnPos :: Pos
  , _defnLhs :: Id.EVar
  , _defnRhs :: expr v
  }
  deriving (Functor, Foldable, Traversable)

data StdAltn con (expr :: * -> *) v =
  MkAltn Pos (StdPatn con) (expr (Scope Id.EVar v))

data StdPatn con
   = Bind     Bind
   | Dest Pos con [StdPatn con]

pattern Simp :: Pos -> con -> [Bind] -> StdPatn con
pattern Simp w c bs <- Dest w c (traverse patnToBind -> Just bs)
  where Simp w c bs = Dest w c (map Bind bs)

altnRhs
  :: Functor f
  => (forall i. Ord i => expr1 (Scope i v1) -> f (expr2 (Scope i v2)))
  -> StdAltn con expr1 v1 -> f (StdAltn con expr2 v2)
altnRhs f (MkAltn w p t) = MkAltn w p <$> f t

mapAltnRhs
  :: (forall i. expr (Scope i v1) -> expr (Scope i v2))
  -> StdAltn con expr v1 -> StdAltn con expr v2
mapAltnRhs f = runIdentity . altnRhs (Identity . f)

patnToBind :: StdPatn con -> Maybe Bind
patnToBind = \case
  Bind b -> Just b
  Dest{} -> Nothing

patnBind :: Traversal' (StdPatn con) Bind
patnBind f = \case
  Bind   b    -> Bind <$> f b
  Dest w c ps -> Dest w c <$> (traverse . patnBind) f ps

data Bind
  = Wild Pos
  | Name Pos Id.EVar

bindName :: Bind -> Maybe Id.EVar
bindName = \case
  Wild _   -> Nothing
  Name _ x -> Just x

data Scope i v
  = Bound i (Forget Id.EVar)
  | Free v
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

type FinScope n = Scope (Finite n)

bound :: i -> Id.EVar -> Scope i v
bound i x = Bound i (Forget x)

free :: v -> Scope i v
free = Free

strengthen :: String -> Scope i v -> v
strengthen component = \case
  Bound _ (Forget x) -> bug component "cannot strengthen" (Just (show x))
  Free  x            -> x

weaken1 :: Scope j v -> Scope j (Scope i v)
weaken1 = \case
  Bound j x -> Bound j x
  Free  x   -> Free  (Free x)

abstract1 :: (j -> Maybe i) -> Scope j v -> Scope j (Scope i v)
abstract1 f = \case
  Bound (f -> Just i) x -> Free (Bound i x)
  Bound j             x -> Bound j x
  Free  x               -> Free (Free x)

-- TODO: Find out where we use the inverse 'scope' without naming it like this.
-- It's probably in the de Bruijn indexer.
unscope :: Scope i Id.EVar -> Id.EVar
unscope = \case
  Bound _ (Forget x) -> x
  Free  x            -> x

class (Eq v, Ord v, Pretty v) => IsVar v where
  varName :: v -> Id.EVar
  isTotallyFree :: v -> Bool
  mkTotallyFree :: Id.EVar -> v

instance IsVar Id.EVar where
  varName = id
  isTotallyFree = const True
  mkTotallyFree = id

instance (Ord i, IsVar v) => IsVar (Scope i v) where
  varName = \case
    Bound _ (Forget x) -> x
    Free  v            -> varName v
  isTotallyFree = \case
    Bound _ _ -> False
    Free  v   -> isTotallyFree v
  mkTotallyFree = Free . mkTotallyFree

-- * Optics
makeLenses ''StdDefn

makePrisms ''Bind
makePrisms ''Scope
makePrisms ''StdPatn

-- * Instances
instance HasPos (StdDefn expr v) where
  pos = defnPos

instance HasLhs (StdDefn expr v) where
  type Lhs (StdDefn expr v) = Id.EVar
  lhs = defnLhs

instance HasRhs (StdDefn expr v) where
  type Rhs (StdDefn expr v) = expr v
  rhs = defnRhs

instance HasRhs1 (StdDefn expr) where
  type Rhs1 (StdDefn expr) = expr
  rhs1 = defnRhs

instance HasRhs2 StdDefn where
  rhs2 = defnRhs

instance HasPos (StdAltn con expr v) where
  pos f (MkAltn w p t) = fmap (\w' -> MkAltn w' p t) (f w)


instance Pretty v => Pretty (Scope i v) where
  pPrint = \case
    Bound _ (Forget x) -> pretty x
    Free v -> pretty v

instance Pretty con => Pretty (StdPatn con) where
  pPrintPrec lvl prec = \case
    Bind   b    -> pretty b
    Dest _ c ps ->
      maybeParens (prec > 0 && not (null ps)) $
      pretty c <+> hsep (map (pPrintPrec lvl 1) (toList ps))

instance Pretty Bind where
  pPrint = \case
    Wild _   -> "_"
    Name _ x -> pretty x

deriving instance Traversable expr => Functor     (StdAltn con expr)
deriving instance Traversable expr => Foldable    (StdAltn con expr)
deriving instance Traversable expr => Traversable (StdAltn con expr)

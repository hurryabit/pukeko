{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- | This module provides types and functions used by many ASTs.
module Pukeko.Language.Base.AST
  ( -- * Position in the input file
    Pos

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

  , module Pukeko.Language.AST.Classes
  , module Pukeko.Language.AST.Scope

    -- * Common types
  , Finite
  , Vector
  , KnownNat
  )
  where

import           Control.Lens
import           Data.Finite       (Finite)
import           Data.Foldable     (toList)
import           Data.Vector.Sized (Vector)
import           GHC.TypeLits      (KnownNat)

import           Pukeko.Pos
import           Pukeko.Pretty
import           Pukeko.Language.AST.Classes
import           Pukeko.Language.AST.Scope
import qualified Pukeko.Language.Ident as Id

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


-- * Optics
makeLenses ''StdDefn

makePrisms ''Bind
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

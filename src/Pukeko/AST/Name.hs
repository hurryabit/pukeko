{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module Pukeko.AST.Name
  ( NameSpace (..)
  , TmVar
  , TyVar
  , DxVar
  , TmCon
  , TyCon
  , Class
  , type (?:>)
  , Super (..)
  , Any
  , Only
  , Name
  , NameSource
  , nameText
  , namePos
  , runNameSource
  , runSTBelowNameSource
  , mkName
  , copyName
  , copyName'
  , synthName
  , mkDxVar
  , type NameSpaceOf
  , HasName (..)
  ) where

import Prelude

import           Control.Lens (Lens')
import           Control.Monad.Freer
import           Control.Monad.Freer.State
import           Control.Monad.ST
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char (toLower)
import           Data.Function
import           Data.Kind
import           Data.Tagged
import           Text.Megaparsec.Pos (SourcePos)

import Pukeko.AST.Pos
import Pukeko.Orphans ()
import Pukeko.Pretty

-- | The four different name spaces. To be used as a kind.
data NameSpace
  = TmVar
  | TyVar
  | TmCon
  | TyCon

-- Saves us a few spaces, but more importantly tons of parentheses.
type TmVar = Name 'TmVar
type TyVar = Name 'TyVar
type DxVar = Name 'TmVar

type TmCon = Name 'TmCon
type TyCon = Name 'TyCon

type Class = Name 'TyCon

data Super s = Any | Only s
type Any  = 'Any
type Only = 'Only

type family (?:>) (nsp1 :: Super k) (nsp2 :: k) :: Constraint where
  Any       ?:> nsp2 = ()
  Only nsp1 ?:> nsp2 = nsp1 ~ nsp2

-- | A name which shall be unique across one run of the compiler.
data Name (nsp :: NameSpace) = Name
  { _id   :: Int
  , _text :: String
  , _pos  :: SourcePos
  } deriving (Show)

nameText :: Name nsp -> Tagged nsp String
nameText = Tagged . _text

namePos :: Lens' (Name nsp) SourcePos
namePos f (Name i t p) = Name i t <$> f p

newtype NameSource a = NameSource{toState :: State Int a}

runNameSource :: forall effs a. Eff (NameSource : effs) a -> Eff effs a
runNameSource = evalState 1 . translate toState

runSTBelowNameSource :: Member NameSource effs =>
  (forall s. Eff [NameSource, ST s] a) -> Eff effs a
runSTBelowNameSource act = do
  n1 <- send (NameSource Get)
  let (res, n2) = runST (act & translate toState & runState n1 & runM)
  send (NameSource (Put n2))
  pure res

mkName :: Member NameSource effs => Lctd (Tagged nsp String) -> Eff effs (Name nsp)
mkName (Lctd pos (Tagged text)) = do
  n <- send (NameSource Get)
  send (NameSource (Put (n+1)))
  pure (Name n text pos)

copyName :: Member NameSource effs => Name nsp -> Eff effs (Name nsp)
copyName = copyName' id

copyName'
  :: Member NameSource effs
  => (String -> String) -> Name nsp1 -> Eff effs (Name nsp2)
copyName' f (Name _ text _) = mkName (Lctd noPos (Tagged (f text)))

synthName
  :: Member NameSource effs
  => SourcePos -> Name nsp1 -> Name nsp2 -> Eff effs (Name nsp3)
synthName pos name1 name2 = do
  let name3 = _text name1 ++ "." ++ _text name2
  mkName (Lctd pos (Tagged name3))

-- TODO: Make it "a.C" rather than "C.a".
mkDxVar :: Member NameSource effs => Class -> TyVar -> Eff effs DxVar
mkDxVar clss tvar = do
  let name0 = uncap (untag (nameText clss)) ++ "." ++ untag (nameText tvar)
  mkName (Lctd noPos (Tagged name0))
  where
    uncap = \case
      x:xs -> toLower x:xs
      _ -> error "IMPOSSIBLE"

type family NameSpaceOf (a :: Type) :: NameSpace

type instance NameSpaceOf (Name nsp) = nsp
type instance NameSpaceOf (Lctd a) = NameSpaceOf a
type instance NameSpaceOf (a, b) = NameSpaceOf a

class HasName a where
  nameOf :: a -> Name (NameSpaceOf a)

instance HasName (Name nsp) where
  nameOf = id

instance HasName a => HasName (Lctd a) where
  nameOf = nameOf . unlctd

instance HasName a => HasName (a, b) where
  nameOf = nameOf . fst

instance HasPos (Name name) where
  getPos = _pos

instance Eq (Name nsp) where
  (==) = (==) `on` _id

instance Ord (Name nsp) where
  compare = compare `on` _id

instance Pretty (Name nsp) where
  pretty (Name i t _) = annotateId i (pretty t)

-- NOTE: This is necessary to make ''Name avaliable below.
$(return [])

instance ToJSON (Name nsp) where
  toJSON = $(mkToJSON defaultOptions ''Name)

instance ToJSONKey (Name nsp)

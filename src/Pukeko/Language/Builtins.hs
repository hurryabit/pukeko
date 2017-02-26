module Pukeko.Language.Builtins
  ( primitives
  , everything
  , unit
  , bool
  , pair
  , list
  , ADT (..)
  , Constructor (..)
  , adts
  , findConstructor
  )
  where

import Control.Monad.Except
import Data.Map (Map)

import qualified Data.Map as Map
import Pukeko.Language.Syntax (Ident (..))
import Pukeko.Language.Type

everything :: [(Ident, Type Closed)]
everything = map (\(i, t) -> (MkIdent i, t)) primitives ++
  concatMap constructors adts

alpha, beta :: Type Closed
alpha = var (MkIdent "a")
beta  = var (MkIdent "b")

primitives :: [(String, Type Closed)]
primitives =
  [ ("neg", int  ~> int)
  , ("prefix_add", int  ~> int  ~> int )
  , ("prefix_sub", int  ~> int  ~> int )
  , ("prefix_mul", int  ~> int  ~> int )
  , ("prefix_div", int  ~> int  ~> int )
  , ("prefix_mod", int  ~> int  ~> int )
  , ("prefix_lt" , int  ~> int  ~> bool)
  , ("prefix_le" , int  ~> int  ~> bool)
  , ("prefix_eq" , int  ~> int  ~> bool)
  , ("prefix_ne" , int  ~> int  ~> bool)
  , ("prefix_ge" , int  ~> int  ~> bool)
  , ("prefix_gt" , int  ~> int  ~> bool)
  , ("return", alpha ~> io alpha)
  , ("print" , int ~> io unit)
  , ("prefix_bind", io alpha ~> (alpha ~> io beta) ~> io beta)
  , ("abort" , alpha)
  ]

adts :: [ADT]
adts = [unitADT, boolADT, pairADT, listADT]

unit :: Type Closed
unit = adt unitADT []

unitADT :: ADT
unitADT = mkADT "Unit" [] [mkConstructor "Unit" []]

bool :: Type Closed
bool = adt boolADT []

boolADT :: ADT
boolADT = mkADT "Bool" []
  [ mkConstructor "False" []
  , mkConstructor "True"  []
  ]

pair :: Type Closed -> Type Closed -> Type Closed
pair t1 t2 = adt pairADT [t1, t2]

pairADT :: ADT
pairADT = mkADT "Pair" [alpha, beta] [mkConstructor "Pair" [alpha, beta]]

list :: Type Closed -> Type Closed
list t = adt listADT [t]

listADT :: ADT
listADT = mkADT "List" [alpha]
  [ mkConstructor "Nil"  []
  , mkConstructor "Cons" [alpha, list alpha]
  ]

data ADT = MkADT
  { _name         :: Ident
  , _params       :: [Type Closed]
  , _constructors :: [Constructor]
  }

mkADT :: String -> [Type Closed] -> [Constructor] -> ADT
mkADT name _params _constructors =
  MkADT { _name = MkIdent name, _params, _constructors }

data Constructor = MkConstructor
  { _name   :: Ident
  , _fields :: [Type Closed]
  }

mkConstructor :: String -> [Type Closed] -> Constructor
mkConstructor name _fields =
  MkConstructor { _name = MkIdent name, _fields }

adt :: ADT -> [Type Closed] -> Type Closed
adt MkADT{ _name } = app _name

constructors :: ADT -> [(Ident, Type Closed)]
constructors t@MkADT{ _params, _constructors } = map f _constructors
  where
    f MkConstructor{ _name, _fields } =
      (_name, foldr (~>) (adt t _params) _fields)

dictConstructors :: Map Ident (Constructor, ADT)
dictConstructors = Map.fromList $ do
  t@MkADT{ _constructors } <- adts
  c@MkConstructor{ _name } <- _constructors
  return (_name, (c, t))

findConstructor :: MonadError String m => Ident -> m (Constructor, ADT)
findConstructor name =
  case Map.lookup name dictConstructors of
    Nothing  -> throwError $ "unknown constructor: " ++ show name
    Just res -> return res

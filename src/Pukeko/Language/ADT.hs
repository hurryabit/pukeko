module Pukeko.Language.ADT
  ( ADT (..)
  , Constructor (..)
  , constructors
  , mkADT
  , mkConstructor
  , typeOf
  )where

import Pukeko.Language.Ident
import Pukeko.Language.Type

data ADT = MkADT
  { _name         :: Ident
  , _params       :: [Ident]
  , _constructors :: [Constructor]
  }

mkADT :: Ident -> [Ident] -> [Constructor] -> ADT
mkADT _name _params constructors =
  let _adt = MkADT
        { _name
        , _params
        , _constructors =
          zipWith (\_tag constr -> constr { _adt, _tag }) [0..] constructors
        }
  in  _adt

data Constructor = MkConstructor
  { _adt    :: ADT
  , _name   :: Ident
  , _tag    :: Int
  , _fields :: [Type Closed]
  }

mkConstructor :: Ident -> [Type Closed] -> Constructor
mkConstructor _name _fields =
  MkConstructor { _adt = undefined, _name, _tag = undefined, _fields }

adt :: ADT -> [Type Closed] -> Type Closed
adt MkADT{ _name } = app _name

constructors :: ADT -> [(Ident, Type Closed)]
constructors t@MkADT{ _params, _constructors } = map f _constructors
  where
    f MkConstructor{ _name, _fields } =
      (_name, foldr (~>) (adt t $ map var _params) _fields)

typeOf :: Constructor -> Type Closed
typeOf MkConstructor{ _adt, _fields } =
  foldr (~>) (adt _adt $ map var $ _params _adt) _fields

module Pukeko.Language.ADT
  ( ADT (..)
  , Constructor (..)
  , constructors
  , mkADT
  , mkConstructor
  , typeOf
  )
where

import Pukeko.Language.Ident (Con)
import Pukeko.Language.Type
import qualified Pukeko.Language.Ident as Ident

data ADT = MkADT
  { _name         :: Ident.Con
  , _params       :: [Ident.Var]
  , _constructors :: [Constructor]
  }

mkADT :: Ident.Con -> [Ident.Var] -> [Constructor] -> ADT
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
  , _name   :: Ident.Con
  , _tag    :: Int
  , _fields :: [Type Con Closed]
  }

mkConstructor :: Ident.Con -> [Type Con Closed] -> Constructor
mkConstructor _name _fields =
  MkConstructor { _adt = undefined, _name, _tag = undefined, _fields }

adt :: ADT -> [Type Con Closed] -> Type Con Closed
adt MkADT{ _name } = app _name

constructors :: ADT -> [(Ident.Con, Type Con Closed)]
constructors t@MkADT{ _params, _constructors } = map f _constructors
  where
    f MkConstructor{ _name, _fields } =
      (_name, foldr (~>) (adt t $ map var _params) _fields)

typeOf :: Constructor -> Type Con Closed
typeOf MkConstructor{ _adt, _fields } =
  foldr (~>) (adt _adt $ map var $ _params _adt) _fields

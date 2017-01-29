module CoreLang.Language.Type
  ( TypeVar
  , typeVars
  , Type (..)
  , var
  , cons
  , int
  , bool
  , fun, (~>)
  , pair
  , list
  , unify
  , unifyMany
  )
  where

import Control.Monad.Except
import Data.Char (isLower, isUpper)
import Data.List (intercalate)
import Text.Printf

import qualified Data.Set as Set

import CoreLang.Language.Term

infixl 6 `pair`
infixr 5 ~>, `fun`

newtype TypeVar = MkVar String
  deriving (Eq, Ord)

typeVars :: [TypeVar]
typeVars = map MkVar (tail vars)
  where
    vars = "_":[ xs ++ [x] | xs <- vars, x <- ['A'..'Z'] ]

data Type
  = TypeVar  TypeVar
  | TypeCons String [Type]
  deriving (Eq)

-- vars must start with a capital letter
var :: String -> Type
var name@(start:_) 
  | isUpper start = TypeVar (MkVar name)
var name          = error (printf "%s is not a valid type variable name" name)

cons :: String -> [Type] -> Type
cons name@(start:_) ts
  | isLower start = TypeCons name ts
cons name _       = error (printf "%s is not a valid type constructor name" name)

int, bool :: Type
int  = cons "int"  []
bool = cons "bool" []

fun, (~>) :: Type -> Type -> Type
fun t1 t2 = cons "fun" [t1, t2]
(~>) = fun

pair :: Type -> Type -> Type
pair t1 t2 = cons "pair" [t1, t2]

list :: Type -> Type
list t = cons "list" [t]



extend :: MonadError String m => Subst Type -> TypeVar -> Type -> m (Subst Type)
extend phi v t =
  case t of
    TypeVar u
      | u == v                    -> return phi
    _
      | Set.member v (freeVars t) -> throwError ("cyclic type variable " ++ show v)
      | otherwise                 -> return (assign v t <> phi)

unifyVar :: MonadError String m => Subst Type -> TypeVar -> Type -> m (Subst Type)
unifyVar phi v t =
  case substVar phi v of
    TypeVar u
      | u == v -> extend phi v phi_t
    phi_v      -> unify phi phi_v phi_t
    where
      phi_t = subst phi t

unify :: MonadError String m => Subst Type -> Type -> Type -> m (Subst Type)
unify phi t1 t2 =
  case (t1, t2) of
    (TypeVar  v1    , _              ) -> unifyVar phi v1 t2
    (TypeCons _ _   , TypeVar  v2    ) -> unifyVar phi v2 t1
    (TypeCons c1 ts1, TypeCons c2 ts2)
      | c1 /= c2                       -> throwError (printf "mismatching types %s and %s" (show t1) (show t2))
      | otherwise                      -> unifyMany phi (zip ts1 ts2)

unifyMany :: MonadError String m => Subst Type -> [(Type, Type)] -> m (Subst Type)
unifyMany = foldM (uncurry . unify)



type instance TermVar Type = TypeVar

instance Term Type where
  promoteVar = TypeVar
  freeVars t =
    case t of
      TypeVar  v    -> Set.singleton v
      TypeCons _ ts -> freeVars' ts
  subst phi t =
    case t of
      TypeVar  v    -> substVar phi v
      TypeCons c ts -> TypeCons c (subst' phi ts)

instance TermLike [Type] where
  type BaseTerm [Type] = Type
  freeVars' = foldMap freeVars
  subst' = fmap . subst


instance Show Type where
  show t =
    case t of
      TypeVar  v               -> show v
      TypeCons "list" [t1]     -> printf "[%s]" (show t1)
      TypeCons "pair" [t1, t2] -> printf "%s * %s" (show t1) (show t2)
      TypeCons "fun"  [_ , _ ] -> printf "(%s)" (intercalate " -> " . map show . collect $ t)
      TypeCons c      []       -> c
      TypeCons c      ts       -> printf "(%s %s)" c (unwords (map show ts))
    where
      collect t =
        case t of
          TypeCons "fun" [t1, t2] -> t1 : collect t2
          _                   -> [t]

instance Show TypeVar where
  show (MkVar s) = s

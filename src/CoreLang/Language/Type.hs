module CoreLang.Language.Type
  ( Type (..)
  , var
  , fun, (~>)
  , app
  , int
  , bool
  , pair
  , list
  , unify
  , unifyMany
  , module CoreLang.Language.Term
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

data Type
  = Var (Var Type)
  | Fun Type Type
  | App String [Type]
  deriving (Eq)

instance Term Type where
  newtype Var Type = MkVar String
    deriving (Eq, Ord)
  supply = map MkVar (tail vars)
    where
      vars = "_":[ xs ++ [x] | xs <- vars, x <- ['A'..'Z'] ]
  promote = Var
  freeVars t =
    case t of
      Var v     -> Set.singleton v
      Fun tx ty -> Set.union (freeVars tx) (freeVars ty)
      App _  ts -> Set.unions (map freeVars ts)
  subst phi t =
    case t of
      Var v     -> substVar phi v
      Fun tx ty -> Fun (subst phi tx) (subst phi ty)
      App c  ts -> App c (map (subst phi) ts)

var :: String -> Type
var name@(start:_) 
  | isUpper start = Var (MkVar name)
var name          = error (printf "%s is not a valid type variable name" name)

fun, (~>) :: Type -> Type -> Type
fun = Fun
(~>) = fun

app :: String -> [Type] -> Type
app name@(start:_) ts
  | isLower start = App name ts
app name _        = error (printf "%s is not a valid type constructor name" name)

int, bool :: Type
int  = app "int"  []
bool = app "bool" []

pair :: Type -> Type -> Type
pair t1 t2 = app "pair" [t1, t2]

list :: Type -> Type
list t = app "list" [t]


extend :: MonadError String m => Subst Type -> Var Type -> Type -> m (Subst Type)
extend phi v t =
  case t of
    Var u
      | u == v                    -> return phi
    _
      | Set.member v (freeVars t) -> throwError ("cyclic type variable " ++ show v)
      | otherwise                 -> return (assign v t <> phi)

unifyVar :: MonadError String m => Subst Type -> Var Type -> Type -> m (Subst Type)
unifyVar phi v t =
  case substVar phi v of
    Var u
      | u == v -> extend phi v phi_t
    phi_v      -> unify phi phi_v phi_t
    where
      phi_t = subst phi t

unify :: MonadError String m => Subst Type -> Type -> Type -> m (Subst Type)
unify phi t1 t2 =
  case (t1, t2) of
    (Var v1     , _          ) -> unifyVar phi v1 t2
    (_          , Var v2     ) -> unifyVar phi v2 t1
    (Fun tx1 ty1, Fun tx2 ty2) -> unifyMany phi [(tx1, tx2), (ty1, ty2)]
    (App c1  ts1, App c2  ts2)
      | c1 == c2               -> unifyMany phi (zip ts1 ts2)
    _                          ->
      throwError (printf "mismatching types %s and %s" (show t1) (show t2))

unifyMany :: MonadError String m => Subst Type -> [(Type, Type)] -> m (Subst Type)
unifyMany = foldM (uncurry . unify)





instance Show Type where
  show t =
    case t of
      Var v    -> show v
      Fun _ _  -> printf "(%s)" (intercalate " -> " . map show . collect $ t)
      App c [] -> c
      App c ts -> printf "(%s %s)" c (unwords (map show ts))
    where
      collect t =
        case t of
          Fun t1 t2 -> t1 : collect t2
          _         -> [t]

instance Show (Var Type) where
  show (MkVar s) = s

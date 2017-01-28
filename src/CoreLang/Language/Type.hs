module CoreLang.Language.Type
  ( Var
  , vars
  , Expr (..)
  , alpha
  , beta
  , gamma
  , delta
  , int
  , bool
  , fun, (~>)
  , pair
  , list
  , Scheme (..)
  , exprScheme
  , Environment
  , FreeVars (..)
  )
  where

import Data.List (intercalate)
import Data.Map (Map)
import Data.Set (Set)
import Text.Printf

import qualified Data.Set as Set

import CoreLang.Language.Syntax (Identifier)

infixl 6 `pair`
infixr 5 ~>, `fun`


newtype Var = MkVar String
  deriving (Eq, Ord)

vars :: [Var]
vars = map MkVar vars
  where
    vars = "":[ xs ++ [x] | xs <- vars, x <- ['A'..'H'] ++ ['J'..'N'] ++ ['P'..'Z'] ]


data Expr
  = Var  Var
  | Cons String [Expr]
  deriving (Eq)

alpha, beta, gamma, delta :: Expr
alpha = Var (MkVar "a")
beta  = Var (MkVar "b")
gamma = Var (MkVar "c")
delta = Var (MkVar "d")

int, bool :: Expr
int  = Cons "int"  []
bool = Cons "bool" []

fun, (~>) :: Expr -> Expr -> Expr
fun t1 t2 = Cons "fun" [t1, t2]
(~>) = fun

pair :: Expr -> Expr -> Expr
pair t1 t2 = Cons "pair" [t1, t2]

list :: Expr -> Expr
list t = Cons "list" [t]


data Scheme = MkScheme { _schematicVars :: Set Var, _expr :: Expr }

exprScheme :: Expr -> Scheme
exprScheme t = MkScheme { _schematicVars = Set.empty, _expr = t }


type Environment = Map Identifier Scheme


class FreeVars a where
  freeVars :: a -> Set Var

instance (Foldable t, FreeVars a) => FreeVars (t a) where
  freeVars = foldMap freeVars

instance FreeVars Expr where
  freeVars t =
    case t of
      Var  v    -> Set.singleton v
      Cons _ ts -> freeVars ts

instance FreeVars Scheme where
  freeVars (MkScheme { _schematicVars, _expr }) = 
    Set.difference (freeVars _expr) _schematicVars


instance Show Expr where
  show t =
    case t of
      Var  v               -> show v
      Cons "list" [t1]     -> printf "[%s]" (show t1)
      Cons "pair" [t1, t2] -> printf "%s * %s" (show t1) (show t2)
      Cons "fun"  [_ , _ ] -> printf "(%s)" (intercalate " -> " . map show . collect $ t)
      Cons c      []       -> c
      Cons c      ts       -> printf "(%s %s)" c (unwords (map show ts))
    where
      collect t =
        case t of
          Cons "fun" [t1, t2] -> t1 : collect t2
          _                   -> [t]

instance Show Var where
  show (MkVar s) = s

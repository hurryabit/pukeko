{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module CoreLang.Language.TypeChecker where

import Control.Monad
import Control.Monad.Except
import Control.Monad.RWS
import Data.Map (Map)
import Data.Monoid (Monoid (..), (<>))
import Data.Set (Set)
import Text.Printf

import qualified Data.Map as Map
import qualified Data.Set as Set

import CoreLang.Language.Syntax

import CoreLang.Language.Parser (parseExpr)

newtype TypeVar = MkTypeVar String
  deriving (Eq, Ord)

data TypeExpr
  = TypeVar TypeVar
  | TypeCons String [TypeExpr]

int, bool :: TypeExpr
int  = TypeCons "int"  []
bool = TypeCons "bool" []

list :: TypeExpr -> TypeExpr
list t = TypeCons "list" [t]

pair :: TypeExpr -> TypeExpr -> TypeExpr
pair t1 t2 = TypeCons "pair" [t1, t2]

fun, (~>) :: TypeExpr -> TypeExpr -> TypeExpr
fun t1 t2 = TypeCons "fun" [t1, t2]
(~>) = fun

alpha, beta, gamma :: TypeExpr
alpha = TypeVar (MkTypeVar "a")
beta  = TypeVar (MkTypeVar "b")
gamma = TypeVar (MkTypeVar "c")

infixl 6 `pair`
infixr 5 ~>, `fun`

builtins = 
  [ ("false"  , bool)
  , ("true"   , bool)
  , ("mk_pair", alpha ~> beta ~> pair alpha beta)
  , ("nil"    , list alpha)
  , ("cons"   , alpha ~> list alpha ~> list alpha)
  , ("neg", int  ~> int        )
  , ("+"  , int  ~> int  ~> int )
  , ("-"  , int  ~> int  ~> int )
  , ("*"  , int  ~> int  ~> int )
  , ("/"  , int  ~> int  ~> int )
  , ("<"  , int  ~> int  ~> bool)
  , ("<=" , int  ~> int  ~> bool)
  , ("==" , int  ~> int  ~> bool)
  , ("!=" , int  ~> int  ~> bool)
  , (">=" , int  ~> int  ~> bool)
  , (">"  , int  ~> int  ~> bool)
  , ("not", bool ~> bool        )
  , ("&&" , bool ~> bool ~> bool)
  , ("||" , bool ~> bool ~> bool)
  , ("if"       , bool ~> alpha ~> alpha ~> alpha)
  , ("case_pair", pair alpha beta ~> (alpha ~> beta ~> gamma) ~> gamma)
  , ("case_list", list alpha ~> beta ~> (alpha ~> list alpha ~> beta) ~> beta)
  , ("print"    , int ~> alpha ~> alpha)
  , ("abort"    , alpha)
  ]



variables :: TypeExpr -> Set TypeVar
variables t =
  case t of
    TypeVar v     -> Set.singleton v
    TypeCons _ ts -> Set.unions (map variables ts)

newtype Subst = MkSubst { runSubst :: TypeVar -> TypeExpr }

class ApplySubst t where
  subst :: Subst -> t -> t

instance ApplySubst TypeExpr where
  subst phi t =
    case t of
      TypeVar  v    -> runSubst phi v
      TypeCons c ts -> TypeCons c (map (subst phi) ts) 

instance Monoid Subst where
  mempty = MkSubst $ TypeVar
  phi `mappend` psi = MkSubst $ subst phi . runSubst psi

fromMap :: Map TypeVar TypeExpr -> Subst
fromMap m = MkSubst $ \v -> Map.findWithDefault (TypeVar v) v m

delta :: TypeVar -> TypeExpr -> Subst
delta v t = MkSubst $ \u -> if u == v then t else TypeVar u

extend :: MonadError String m => Subst -> TypeVar -> TypeExpr -> m Subst
extend phi v t =
  case t of
    TypeVar u
      | u == v                     -> return phi
    _
      | Set.member v (variables t) -> throwError ("cyclic type variable " ++ show v)
      | otherwise                  -> return (delta v t <> phi)

exclude :: Subst -> Set TypeVar -> Subst
exclude phi vs = MkSubst $ \v ->
  if Set.member v vs then TypeVar v else runSubst phi v

unifyVar :: MonadError String m => Subst -> TypeVar -> TypeExpr -> m Subst
unifyVar phi v t =
  case runSubst phi v of
    TypeVar u
      | u == v -> extend phi v phi_t
    phi_v      -> unify phi phi_v phi_t
    where
      phi_t = subst phi t

unify :: MonadError String m => Subst -> TypeExpr -> TypeExpr -> m Subst
unify phi t1 t2 =
  case (t1, t2) of
    (TypeVar v1     , _              ) -> unifyVar phi v1 t2
    (TypeCons _ _   , TypeVar v2     ) -> unifyVar phi v2 t1
    (TypeCons c1 ts1, TypeCons c2 ts2)
      | c1 /= c2                       -> throwError (printf "mismatching types %s and %s" (show t1) (show t2))
      | otherwise                      -> unifyMany phi (zip ts1 ts2)

-- TODO: Inline unifyMany in unify?
unifyMany :: MonadError String m => Subst -> [(TypeExpr, TypeExpr)] -> m Subst
unifyMany = foldM (uncurry . unify)


class Unknowns s where
  unknowns :: s -> Set TypeVar

data TypeScheme = MkTypeScheme { schematic :: Set TypeVar, body :: TypeExpr }

freshScheme :: TypeExpr -> TypeScheme
freshScheme v = MkTypeScheme Set.empty v

instance Unknowns TypeScheme where
  unknowns (MkTypeScheme { schematic, body }) = Set.difference (variables body) schematic

instance ApplySubst TypeScheme where
  subst phi (scheme@MkTypeScheme { schematic, body }) =
    scheme { body = subst (exclude phi schematic) body }

type Environment = Map Identifier TypeScheme

instance Unknowns Environment where
  unknowns = Set.unions . map unknowns . Map.elems

instance ApplySubst Environment where
  subst = fmap . subst


newtype TC a = TC { unTC :: ExceptT String (RWS Environment () [String]) a }
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader Environment
           )

runTC :: Environment -> TC a -> Either String a
runTC env checker = 
  let vars = "_":liftM2 (\xs x -> xs ++ [x]) vars ['a'..'y']
  in  fst (evalRWS (runExceptT (unTC checker)) env (tail vars))

freshVar :: TC TypeExpr
freshVar = TC $ state (\(v:vs) -> (TypeVar (MkTypeVar v), vs))

testTC code = do
  let env =
        [ (fun, MkTypeScheme (variables t) t) | (fun, t) <- builtins ]
  expr <- parseExpr code
  (_, t) <- runTC (Map.fromList env) (tc expr)
  return t
    

tc :: Expr -> TC (Subst, TypeExpr)
tc e =
  case e of
    Var x -> do
      env <- ask
      case Map.lookup x env of
        Nothing     -> throwError (printf "unknown identifier %s" x)
        Just scheme -> do
          MkTypeScheme { body } <- instantiate scheme
          return (mempty,body)
    Num _ -> return (mempty, int)
    Pack _ _ -> throwError "type checking constructors not implemented"
    Ap ef ex -> do
      (phi, [tf, tx]) <- tcMany [ef, ex]
      vy <- freshVar
      phi <- unify phi tf (tx ~> vy)
      return (phi, subst phi vy)
    Lam []     e -> tc e
    Lam (x:xs) e -> do
      let e' = Lam xs e
      v <- freshVar
      let scheme = freshScheme v
      (phi, t) <- local (Map.insert x scheme) (tc e')
      return (phi, subst phi v ~> t)
    Let NonRecursive xes f -> do
      let (xs, es) = unzip xes
      (phi, ts) <- tcMany es
      local (subst phi) $ do
        localDecls xs ts $ do
          (psi, t) <- tc f
          return (psi <> phi, t)
    Let Recursive xes f -> do
      let (xs, es) = unzip xes
      vs <- mapM (\_ -> freshVar) xs
      let env = Map.fromList (zipWith (\x v -> (x, freshScheme v)) xs vs)
      (phi, ts) <- local (Map.union env) (tcMany es)
      local (subst phi) $ do
        let phi_vs = map (subst phi) vs
        psi <- unifyMany phi (zip ts phi_vs)
        local (subst psi) $ do
          localDecls xs (map (subst psi) phi_vs) $ do
            (rho, t) <- tc f
            return (rho <> psi, t)
    Case _ _ -> throwError "type checking pattern matches not implemented"

tcMany :: [Expr] -> TC (Subst, [TypeExpr])
tcMany [] = return (mempty, [])
tcMany (e:es) = do
  (phi, t)  <- tc e
  (psi, ts) <- local (subst phi) (tcMany es)
  return (psi <> phi, subst psi t : ts)

localDecls :: [Identifier] -> [TypeExpr] -> TC a -> TC a
localDecls xs ts cont = do
  unks <- asks unknowns
  schemes <-
    forM ts $ \t ->
      instantiate $ MkTypeScheme
        { schematic = Set.difference (variables t) unks
        , body      = t
        }
  let env = Map.fromList (zip xs schemes)
  local (Map.union env) cont

instantiate :: TypeScheme -> TC TypeScheme
instantiate (MkTypeScheme { schematic, body }) = do
  bindings <- mapM (\_ ->freshVar) (Map.fromSet id schematic)
  return $
    MkTypeScheme
      { schematic = Set.fromList [ v | TypeVar v <- Map.elems bindings ]
      , body      = subst (fromMap bindings) body
      }

-- Show instances
instance Show TypeVar where
  show (MkTypeVar s) = s
  
instance Show TypeExpr where
  show t =
    case t of
      TypeVar  v               -> show v
      TypeCons "list" [t1]     -> printf "[%s]"       (show t1)
      TypeCons "pair" [t1, t2] -> printf "%s * %s"   (show t1) (show t2)
      TypeCons "fun"  [t1, t2] -> printf "(%s -> %s)" (show t1) (show t2)
      TypeCons c      []       -> c
      TypeCons c      ts       -> printf "(%s %s)"    c (unwords (map show ts))

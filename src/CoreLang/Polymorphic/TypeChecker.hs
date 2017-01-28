module CoreLang.Polymorphic.TypeChecker
  ( inferExpr
  )
  where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Supply
import Data.Map (Map)
import Data.Set (Set)
import Text.Printf

import qualified Data.Map as Map
import qualified Data.Set as Set

import CoreLang.Language.Syntax (Expr, Identifier)
import CoreLang.Language.Term
import CoreLang.Language.Type (Type, TypeVar, (~>))

import qualified CoreLang.Language.Syntax      as Syntax
import qualified CoreLang.Language.Type        as Type
import qualified CoreLang.Polymorphic.Builtins as Builtins


inferExpr :: MonadError String m => Expr -> m Type
inferExpr expr = do
  let  env = [ (fun, MkScheme (freeVars t) t) | (fun, t) <- Builtins.everything ]
  (_, t) <- runTI (Map.fromList env) (infer expr)
  return t


data TypeScheme = MkScheme { _schematicVars :: Set TypeVar, _type :: Type }

type Environment = Map Identifier TypeScheme


newtype TI a =
  TI  { unTI :: ExceptT String (ReaderT Environment (Supply TypeVar)) a }
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader Environment
           , MonadSupply TypeVar
           )

runTI :: MonadError String m => Environment -> TI a -> m a
runTI env ti =
  case evalSupply (runReaderT (runExceptT (unTI ti)) env) Type.typeVars of
    Left  e -> throwError e
    Right x -> return x

freshVar :: TI Type
freshVar = Type.TypeVar <$> fresh

infer :: Expr -> TI (Subst Type, Type)
infer e =
  case e of
    Syntax.Var x -> do
      env <- ask
      case Map.lookup x env of
        Nothing     -> throwError (printf "unknown identifier %s" x)
        Just scheme -> do
          MkScheme { _type } <- instantiate scheme
          return (mempty, _type)
    Syntax.Num _ -> return (mempty, Type.int)
    Syntax.Pack _ _ -> throwError "type checking constructors not implemented"
    Syntax.Ap ef ex -> do
      (phi, [tf, tx]) <- inferMany [ef, ex]
      vy <- freshVar
      phi <- Type.unify phi tf (tx ~> vy)
      return (phi, subst phi vy)
    Syntax.Lam xs e0 -> do
      (phi, vs, t0) <- localFreshVars xs (infer e0)
      return (phi, foldr (~>) t0 vs)
    Syntax.Let Syntax.NonRecursive xes e0 -> do
      let (xs, es) = unzip xes
      (phi, ts) <- inferMany es
      local (subst' phi) $ do
        localDecls xs ts $ do
          (psi, t) <- infer e0
          return (psi <> phi, t)
    Syntax.Let Syntax.Recursive xes e0 -> do
      let (xs, es) = unzip xes
      (phi, vs, ts) <- localFreshVars xs (inferMany es)
      psi <- Type.unifyMany phi (zip ts vs)
      local (subst' (psi <> phi)) $ do -- TODO: Is phi really needed here?
        localDecls xs (map (subst psi) ts) $ do
          (rho, t) <- infer e0
          return (rho <> psi, t)

inferMany :: [Expr] -> TI (Subst Type, [Type])
inferMany [] = return (mempty, [])
inferMany (e:es) = do
  (phi, t)  <- infer e
  (psi, ts) <- local (subst' phi) (inferMany es)
  return (psi <> phi, subst psi t : ts)


instantiate :: TypeScheme -> TI TypeScheme
instantiate (MkScheme { _schematicVars, _type }) = do
  bindings <- mapM (\_ -> freshVar) (Map.fromSet id _schematicVars)
  return $ MkScheme
    { _schematicVars = Set.fromList [ v | Type.TypeVar v <- Map.elems bindings ]
    , _type          = subst (mkSubst bindings) _type
    }

localDecls :: [Identifier] -> [Type] -> TI a -> TI a
localDecls xs ts cont = do
  fvs <- asks freeVars'
  schemes <-
    forM ts $ \t ->
      instantiate $ MkScheme
        { _schematicVars = Set.difference (freeVars t) fvs
        , _type          = t
        }
  let env = Map.fromList (zip xs schemes)
  local (Map.union env) cont

localFreshVars :: [Identifier] -> TI (Subst Type, a) -> TI (Subst Type, [Type], a)
localFreshVars xs cont = do
  vs <- mapM (\_ -> freshVar) xs
  let mkScheme v = MkScheme { _schematicVars = Set.empty, _type = v }
      env = Map.fromList (zipWith (\x v -> (x, mkScheme v)) xs vs)
  (phi, t) <- local (Map.union env) cont
  return (phi, map (subst phi) vs, t)


instance TermLike TypeScheme where
  type BaseTerm TypeScheme = Type
  freeVars' (MkScheme { _schematicVars, _type }) = 
    Set.difference (freeVars _type) _schematicVars
  subst' phi (scheme@MkScheme { _schematicVars, _type }) =
    scheme { _type = subst (phi `exclude` _schematicVars) _type }

instance TermLike Environment where
  type BaseTerm Environment = Type
  freeVars' = foldMap freeVars'
  subst' = fmap . subst'
module CoreLang.Language.TypeChecker
  ( inferExpr
  , checkProgram
  )
  where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Monoid (Monoid (..), (<>))
import Text.Printf

import qualified Data.Map as Map
import qualified Data.Set as Set

import CoreLang.Language.Subst (Subst, subst, unify, unifyMany)
import CoreLang.Language.Supply
import CoreLang.Language.Type (Scheme (..), (~>))

import qualified CoreLang.Language.Builtins as Builtins
import qualified CoreLang.Language.Subst    as Subst
import qualified CoreLang.Language.Syntax   as Syntax
import qualified CoreLang.Language.Type     as Type


checkProgram :: MonadError String m => Syntax.Program -> m ()
checkProgram defs = do
  let local (fun, args, body) = (fun, Syntax.Lam args body)
      expr = Syntax.Let Syntax.Recursive (map local defs) (Syntax.Var "main")
  t <- inferExpr expr
  if t /= Type.int then
    return ()
  else
    throwError $ printf "main has type %s instead of %s" (show t) (show Type.int)

inferExpr :: MonadError String m => Syntax.Expr -> m Type.Expr
inferExpr expr = do
  let  env = [ (fun, MkScheme (Type.freeVars t) t) | (fun, t) <- Builtins.everything ]
  (_, t) <- runTI (Map.fromList env) (infer expr)
  return t

newtype TI a =
  TI  { unTI :: ExceptT String (ReaderT Type.Environment (Supply Type.Var)) a }
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader Type.Environment
           , MonadSupply Type.Var
           )

runTI :: MonadError String m => Type.Environment -> TI a -> m a
runTI env ti =
  case evalSupply (runReaderT (runExceptT (unTI ti)) env) Type.vars of
    Left  e -> throwError e
    Right x -> return x

freshVar :: TI Type.Expr
freshVar = Type.Var <$> fresh

infer :: Syntax.Expr -> TI (Subst, Type.Expr)
infer e =
  case e of
    Syntax.Var x -> do
      env <- ask
      case Map.lookup x env of
        Nothing     -> throwError (printf "unknown identifier %s" x)
        Just scheme -> do
          MkScheme { _expr } <- instantiate scheme
          return (mempty, _expr)
    Syntax.Num _ -> return (mempty, Type.int)
    Syntax.Pack _ _ -> throwError "type checking constructors not implemented"
    Syntax.Ap ef ex -> do
      (phi, [tf, tx]) <- inferMany [ef, ex]
      vy <- freshVar
      phi <- unify phi tf (tx ~> vy)
      return (phi, subst phi vy)
    Syntax.Lam xs e0 -> do
      (phi, vs, t0) <- localFreshVars xs (infer e0)
      return (phi, foldr (~>) t0 vs)
    Syntax.Let Syntax.NonRecursive xes e0 -> do
      let (xs, es) = unzip xes
      (phi, ts) <- inferMany es
      local (subst phi) $ do
        localDecls xs ts $ do
          (psi, t) <- infer e0
          return (psi <> phi, t)
    Syntax.Let Syntax.Recursive xes e0 -> do
      let (xs, es) = unzip xes
      (phi, vs, ts) <- localFreshVars xs (inferMany es)
      psi <- unifyMany phi (zip ts vs)
      local (subst (psi <> phi)) $ do -- TODO: Is phi really needed here?
        localDecls xs (map (subst psi) ts) $ do
          (rho, t) <- infer e0
          return (rho <> psi, t)

inferMany :: [Syntax.Expr] -> TI (Subst, [Type.Expr])
inferMany [] = return (mempty, [])
inferMany (e:es) = do
  (phi, t)  <- infer e
  (psi, ts) <- local (subst phi) (inferMany es)
  return (psi <> phi, subst psi t : ts)


instantiate :: Type.Scheme -> TI Type.Scheme
instantiate (MkScheme { _schematicVars, _expr }) = do
  bindings <- mapM (\_ -> freshVar) (Map.fromSet id _schematicVars)
  return $ MkScheme
    { _schematicVars = Set.fromList [ v | Type.Var v <- Map.elems bindings ]
    , _expr          = subst (Subst.fromMap bindings) _expr
    }

localDecls :: [Syntax.Identifier] -> [Type.Expr] -> TI a -> TI a
localDecls xs ts cont = do
  fvs <- asks Type.freeVars
  schemes <-
    forM ts $ \t ->
      instantiate $ MkScheme
        { _schematicVars = Set.difference (Type.freeVars t) fvs
        , _expr          = t
        }
  let env = Map.fromList (zip xs schemes)
  local (Map.union env) cont

localFreshVars :: [Syntax.Identifier] -> TI (Subst, a) -> TI (Subst, [Type.Expr], a)
localFreshVars xs cont = do
  vs <- mapM (\_ -> freshVar) xs
  let env = Map.fromList (zipWith (\x v -> (x, Type.exprScheme v)) xs vs)
  (phi, t) <- local (Map.union env) cont
  return (phi, map (subst phi) vs, t)

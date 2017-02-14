{-# LANGUAGE MultiParamTypeClasses #-}
module CoreLang.Language.TypeChecker
  ( inferExpr
  )
  where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.Supply
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

import CoreLang.Pretty
import CoreLang.Language.Syntax
import CoreLang.Language.Term
import CoreLang.Language.Type (Type, Var, (~>), unify, unifyMany)

import qualified CoreLang.Language.Builtins as Builtins (everything)
import qualified CoreLang.Language.Type as Type

inferExpr :: MonadError String m => Expr a -> m Type
inferExpr expr = do
  let  env = map (\(i, t) -> (i, mkBoundScheme t)) Builtins.everything
  (_, t) <- runTI (Map.fromList env) (infer expr)
  return t


data Scheme = MkScheme { _boundVars :: Set (Var Type), _type :: Type }

mkBoundScheme, mkFreeScheme :: Type -> Scheme
mkBoundScheme _type = MkScheme { _boundVars = freeVars _type, _type }
mkFreeScheme  _type = MkScheme { _boundVars = Set.empty     , _type }

type Environment = Map Ident Scheme


newtype TI a =
  TI  { unTI :: ExceptT String (ReaderT Environment (Supply (Var Type))) a }
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader Environment
           , MonadSupply (Var Type)
           )

instance MonadFail TI where
  fail = throwError

runTI :: MonadError String m => Environment -> TI a -> m a
runTI env ti =
  case evalSupply (runReaderT (runExceptT (unTI ti)) env) Type.supply of
    Left  e -> throwError e
    Right x -> return x

freshVar :: TI Type
freshVar = Type.Var <$> fresh

lookupAndInstantiate :: Ident -> String -> TI Type
lookupAndInstantiate ident kind = do
  t_ident_env <- asks (Map.lookup ident)
  case t_ident_env of
    Nothing -> pthrow (text "unknown" <+> text kind <> colon <+> pretty ident)
    Just scheme -> do
      MkScheme { _type } <- instantiateScheme scheme
      return _type


infer :: Expr a -> TI (Subst Type, Type)
infer expr =
  case expr of
    Var { _ident } -> do
      t_ident <- lookupAndInstantiate _ident "identifier"
      return (mempty, t_ident)
    Num { } -> return (mempty, Type.int)
    Ap { _fun, _arg } -> do
      (phi, [t_fun, t_arg]) <- inferMany [_fun, _arg]
      t_res <- freshVar
      psi <- unify phi t_fun (t_arg ~> t_res)
      return (psi, subst psi t_res)
    ApOp { _op, _arg1, _arg2 } -> do
      t_op <- lookupAndInstantiate _op "operator"
      (phi, [t_arg1, t_arg2]) <- inferMany [_arg1, _arg2]
      t_res <- freshVar
      psi <- unify phi t_op (t_arg1 ~> t_arg2 ~> t_res)
      return (psi, subst psi t_res)
    Lam { _patns, _body } -> do
      (phi, t_patns, t_body) <- introduceInstantiated _patns (infer _body)
      return (phi, foldr (~>) t_body t_patns)
    -- TODO: Remove code duplication.
    Let { _isrec = False, _defns, _body } -> do
      let (patns, exprs) = unzipDefns _defns
      (phi, t_exprs) <- inferMany exprs
      t_patns <- mapM (\(MkPatn { _type }) -> instantiateAnnot _type) patns
      psi <- unifyMany phi (zip t_patns t_exprs)
      local (subst' psi) $ do
        introduceGeneralized patns (map (subst psi) t_patns) $ do
          (rho, t_body) <- infer _body
          return (rho <> psi, t_body)
    Let { _isrec = True, _defns, _body } -> do
      let (patns, exprs) = unzipDefns _defns
      (phi, t_patns, t_exprs) <- introduceInstantiated patns (inferMany exprs)
      psi <- unifyMany phi (zip t_patns t_exprs)
      local (subst' psi) $ do
        introduceGeneralized patns (map (subst psi) t_patns) $ do
          (rho, t_body) <- infer _body
          return (rho <> psi, t_body)
    If { _cond, _then, _else } -> do
      (phi, t_cond) <- infer _cond
      psi <- unify phi Type.bool t_cond
      local (subst' psi) $ do
        (rho, [t_then, t_else]) <- inferMany [_then, _else]
        chi <- unify (rho <> psi) t_then t_else
        return (chi, subst chi t_then)
    Rec  { _defns } -> do
      let (patns, exprs) = unzipDefns _defns
      (phi, t_exprs) <- inferMany exprs
      t_patns <- mapM (\(MkPatn { _type }) -> instantiateAnnot _type) patns
      psi <- unifyMany phi (zip t_patns t_exprs)
      let t_rec = Type.record $
            zipWith (\(MkPatn { _ident }) t_patn -> (_ident, subst psi t_patn)) patns t_patns
      return (psi, t_rec)
    Sel  { } -> pthrow (text "type checking of record selectors not implemented")
    Pack { } -> pthrow (text "type checking of constructors not implemented")

inferMany :: [Expr a] -> TI (Subst Type, [Type])
inferMany [] = return (mempty, [])
inferMany (e:es) = do
  (phi, t)  <- infer e
  (psi, ts) <- local (subst' phi) (inferMany es)
  return (psi <> phi, subst psi t : ts)

-- | In the result, @_boundVars@ contains the new names of the old bound variables.
instantiateScheme :: Scheme -> TI Scheme
instantiateScheme (MkScheme { _boundVars, _type }) = do
  bindings <- mapM (\_ -> freshVar) (Map.fromSet id _boundVars)
  return $ MkScheme
    { _boundVars = Set.fromList [ v | Type.Var v <- Map.elems bindings ]
    , _type      = subst (mkSubst bindings) _type
    }

instantiateAnnot :: Maybe Type -> TI Type
instantiateAnnot t_annot =
  case t_annot of
    Nothing     -> freshVar
    Just t_patn -> do
      MkScheme { _type } <- instantiateScheme (mkBoundScheme t_patn)
      return _type

introduceInstantiated :: [Patn a] -> TI (Subst Type, t) -> TI (Subst Type, [Type], t)
introduceInstantiated patns sub = do
  let entry (MkPatn { _ident, _type }) = do
        t_ident <- instantiateAnnot _type
        let scheme = mkFreeScheme t_ident
        return (t_ident, (_ident, scheme))
  (t_idents, env_list) <- unzip <$> mapM entry patns
  let env = Map.fromList env_list
  (phi, t_sub) <- local (Map.union env) sub
  return (phi, map (subst phi) t_idents, t_sub)

introduceGeneralized :: [Patn a] -> [Type] -> TI t -> TI t
introduceGeneralized patns types sub = do
  free_vars <- asks freeVars'
  let entry (MkPatn { _ident }) _type = do
        scheme <- instantiateScheme $ MkScheme
          { _boundVars = Set.difference (freeVars _type) free_vars
          , _type
          }
        return (_ident, scheme)
  env <- Map.fromList <$> zipWithM entry patns types
  local (Map.union env) sub


instance TermCollection Type Scheme where
  freeVars' (MkScheme { _boundVars, _type }) =
    Set.difference (freeVars _type) _boundVars
  subst' phi (scheme@MkScheme { _boundVars, _type }) =
    scheme { _type = subst (phi `exclude` _boundVars) _type }

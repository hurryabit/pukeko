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

import qualified Data.Map as Map
import qualified Data.Set as Set

import CoreLang.Pretty
import CoreLang.Language.Syntax
import CoreLang.Language.Term
import CoreLang.Language.Type (Type, Var, (~>))

import qualified CoreLang.Language.Type as Type
import qualified CoreLang.Polymorphic.Builtins as Builtins (everything)

inferExpr :: MonadError String m => Expr -> m Type
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

runTI :: MonadError String m => Environment -> TI a -> m a
runTI env ti =
  case evalSupply (runReaderT (runExceptT (unTI ti)) env) Type.supply of
    Left  e -> throwError e
    Right x -> return x

freshVar :: TI Type
freshVar = Type.Var <$> fresh

infer :: Expr -> TI (Subst Type, Type)
infer expr =
  case expr of
    Var { _ident } -> do
      env <- ask
      case Map.lookup _ident env of
        Nothing     -> pthrow (text "unknown identifier:" <+> pretty _ident)
        Just scheme -> do
          MkScheme { _type } <- instantiate scheme
          return (mempty, _type)
    Num { } -> return (mempty, Type.int)
    Ap { _fun, _arg } -> do
      (phi, [t_fun, t_arg]) <- inferMany [_fun, _arg]
      t_res <- freshVar
      psi <- Type.unify phi t_fun (t_arg ~> t_res)
      return (psi, subst psi t_res)
    Lam { _decls, _body } -> do
      (phi, t_decls, t_body) <- introduceInstantiated _decls (infer _body)
      return (phi, foldr (~>) t_body t_decls)
    Let { _defns, _body } -> do
      let (decls, exprs) = unzipDefns _defns
      (phi, t_exprs) <- inferMany exprs
      let (_, t_decls) = unzipDecls decls
      psi <- Type.unifyMany phi [ (t_decl, t_expr) | (Just t_decl, t_expr) <- zip t_decls t_exprs ]
      local (subst' psi) $ do
        introduceGeneralized decls (map (subst psi) t_exprs) $ do
          (rho, t_body) <- infer _body
          return (rho <> psi, t_body)
    LetRec { _defns, _body } -> do
      let (decls, exprs) = unzipDefns _defns
      (phi, t_decls, t_exprs) <- introduceInstantiated decls (inferMany exprs)
      psi <- Type.unifyMany phi (zip t_decls t_exprs)
      local (subst' psi) $ do
        introduceGeneralized decls (map (subst psi) t_decls) $ do
          (rho, t_body) <- infer _body
          return (rho <> psi, t_body)
    Pack { } -> pthrow (text "type checking of constructors not implemented")
    If   { } -> pthrow (text "type checking for if-then-else not implemented")
    Rec  { } -> pthrow (text "type checking of records not implemented")
    Sel  { } -> pthrow (text "type checking of records not implemented")

inferMany :: [Expr] -> TI (Subst Type, [Type])
inferMany [] = return (mempty, [])
inferMany (e:es) = do
  (phi, t)  <- infer e
  (psi, ts) <- local (subst' phi) (inferMany es)
  return (psi <> phi, subst psi t : ts)

-- | In the result, @_boundVars@ contains the new names of the old bound variables.
instantiate :: Scheme -> TI Scheme
instantiate (MkScheme { _boundVars, _type }) = do
  bindings <- mapM (\_ -> freshVar) (Map.fromSet id _boundVars)
  return $ MkScheme
    { _boundVars = Set.fromList [ v | Type.Var v <- Map.elems bindings ]
    , _type      = subst (mkSubst bindings) _type
    }

introduceInstantiated :: [Decl] -> TI (Subst Type, a) -> TI (Subst Type, [Type], a)
introduceInstantiated decls sub = do
  let entry (MkDecl { _ident, _type }) = do
        t_ident <- 
          case _type of
            Nothing -> freshVar
            Just t_decl -> do
              MkScheme { _type } <- instantiate (mkBoundScheme t_decl)
              return _type
        let scheme = mkFreeScheme t_ident
        return (t_ident, (_ident, scheme))
  (t_idents, env_list) <- unzip <$> mapM entry decls
  let env = Map.fromList env_list
  (phi, t_sub) <- local (Map.union env) sub
  return (phi, map (subst phi) t_idents, t_sub)

introduceGeneralized :: [Decl] -> [Type] -> TI a -> TI a
introduceGeneralized decls types sub = do
  free_vars <- asks freeVars'
  let entry (MkDecl { _ident }) t_found = do
        scheme <- instantiate $ MkScheme
          { _boundVars = Set.difference (freeVars t_found) free_vars
          , _type      = t_found
          }
        return (_ident, scheme)
  env <- Map.fromList <$> zipWithM entry decls types
  local (Map.union env) sub


instance TermLike Scheme where
  type BaseTerm Scheme = Type
  freeVars' (MkScheme { _boundVars, _type }) = 
    Set.difference (freeVars _type) _boundVars
  subst' phi (scheme@MkScheme { _boundVars, _type }) =
    scheme { _type = subst (phi `exclude` _boundVars) _type }

instance TermLike Environment where
  type BaseTerm Environment = Type
  freeVars' = foldMap freeVars'
  subst' = fmap . subst'
module CoreLang.Monomorphic.Inferrer
  ( inferExpr
  )
  where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Supply
import Data.Map (Map)
import Text.Printf

import qualified Data.Map as Map

import CoreLang.Language.Syntax
import CoreLang.Language.Term
import CoreLang.Language.Type (Type, (~>))

import qualified CoreLang.Language.Syntax      as Syntax
import qualified CoreLang.Language.Type        as Type
import qualified CoreLang.Monomorphic.Builtins as Builtins


inferExpr :: MonadError String m => Expr -> m Type
inferExpr expr = do
  (_, t) <- runTI (Map.fromList Builtins.everything) (infer expr)
  return t


type Environment = Map Identifier Type


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
infer e =
  case e of
    Syntax.Var x -> do
      env <- ask
      case Map.lookup x env of
        Nothing     -> throwError (printf "unknown identifier %s" x)
        Just _type ->
          return (mempty, _type)
    Syntax.Num _ -> return (mempty, Type.int)
    Syntax.Ap ef ex -> do
      (phi, [tf, tx]) <- inferMany [ef, ex]
      vy <- freshVar
      phi <- Type.unify phi tf (tx ~> vy)
      return (phi, subst phi vy)
    Syntax.Lam xs e0 -> do
      (phi, vs, t0) <- localFreshVars xs (infer e0)
      return (phi, foldr (~>) t0 vs)
    Syntax.Let xes e0 -> do
      let (xs, es) = unzip xes
      (phi, ts) <- inferMany es
      local (subst' phi) $ do
        localPatns xs ts $ do
          (psi, t) <- infer e0
          return (psi <> phi, t)
    Syntax.LetRec xes e0 -> do
      let (xs, es) = unzip xes
      (phi, vs, ts) <- localFreshVars xs (inferMany es)
      psi <- Type.unifyMany phi (zip ts vs)
      local (subst' (psi <> phi)) $ do -- TODO: Is phi really needed here?
        localPatns xs (map (subst psi) ts) $ do
          (rho, t) <- infer e0
          return (rho <> psi, t)
    Syntax.If   _ _ _ -> throwError "type for if-then-else not implemented"
    Syntax.Pack _ _   -> throwError "type checking constructors not implemented"
      

inferMany :: [Expr] -> TI (Subst Type, [Type])
inferMany [] = return (mempty, [])
inferMany (e:es) = do
  (phi, t)  <- infer e
  (psi, ts) <- local (subst' phi) (inferMany es)
  return (psi <> phi, subst psi t : ts)


localPatns :: [Declaration] -> [Type] -> TI a -> TI a
localPatns xs ts cont = do
  let env = Map.fromList (zipWith (\(x,_) t -> (x,t)) xs ts)
  local (Map.union env) cont

localFreshVars :: [Declaration] -> TI (Subst Type, a) -> TI (Subst Type, [Type], a)
localFreshVars xs cont = do
  vs <- mapM (\_ -> freshVar) xs
  (phi, t) <- localPatns xs vs cont
  return (phi, map (subst phi) vs, t)

module CoreLang.Monomorphic.Checker
  ( checkExpr
  )
  where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map (Map)

import qualified Data.Map as Map

import CoreLang.Language.Syntax (Declaration, Expr, Identifier)
import CoreLang.Language.Type (Type, (~>))

import qualified CoreLang.Language.Syntax      as Syntax
import qualified CoreLang.Language.Type        as Type
import qualified CoreLang.Monomorphic.Builtins as Builtins


checkExpr :: MonadError String m => Expr -> m Type
checkExpr expr = runTI (Map.fromList Builtins.everything) (check expr)


type Environment = Map Identifier Type

newtype TI a =
  TI  { unTI :: ExceptT String (Reader Environment) a }
  deriving ( Functor, Applicative, Monad
           , MonadError  String
           , MonadReader Environment
           )

runTI :: MonadError String m => Environment -> TI a -> m a
runTI env ti =
  case runReader (runExceptT (unTI ti)) env of
    Left  e -> throwError e
    Right x -> return x

check :: Expr -> TI Type
check e =
  case e of
    Syntax.Var x -> do
      env <- ask
      case Map.lookup x env of
        Nothing -> throwError $ "unknown identifier: " ++ show x
        Just  t -> return t
    Syntax.Num _ -> return Type.int
    Syntax.Pack _ _ -> throwError "type checking constructors not implemented"
    Syntax.Ap e1 e2 -> do
      t1 <- check e1
      t2 <- check e2
      case t1 of
        Type.TypeFun tx ty
          | t2 == tx  -> return ty
          | otherwise -> throwError $ "expected " ++ show tx ++ ", found " ++ show t2
        _ -> throwError $ "expected function, found " ++ show t1
    Syntax.Lam xs e0 -> do
      (ts, t0) <- localDecls xs (check e0)
      return $ foldr (~>) t0 ts
    Syntax.Let Syntax.NonRecursive xes e0 -> do
      env_list <- forM xes $ \((xi, ti_opt), ei) -> do
        ti <- check ei
        case ti_opt of
          Just ti'
            | ti /= ti' -> throwError $ "expected " ++ show ti' ++ ", found " ++ show ti
          _             -> return (xi, ti)
      let env = Map.fromList env_list
      local (Map.union env) (check e0)
    Syntax.Let Syntax.Recursive xes e0 -> do
      (_, t) <- localDecls (map fst xes) (check (Syntax.Let Syntax.NonRecursive xes e0))
      return t

localDecls :: [Declaration] -> TI a -> TI ([Type], a)
localDecls xs cont = do
  env_list <- forM xs $ \(x, t_opt) ->
    case t_opt of
      Nothing -> throwError $ x ++ " lacks a type annotation"
      Just t  -> return (x,t)
  let env = Map.fromList env_list
  x <- local (Map.union env) cont
  return (map snd env_list, x)

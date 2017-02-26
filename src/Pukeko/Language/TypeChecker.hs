{-# LANGUAGE TemplateHaskell #-}
module Pukeko.Language.TypeChecker
  ( checkExpr
  )
  where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Reader hiding (asks, local)
import Control.Monad.State
import Control.Monad.ST
import Data.Label (mkLabels)
import Data.Label.Monadic
import Data.Map (Map)
import Data.STRef
import Text.Parsec (SourcePos)


import qualified Data.Map as Map

import Pukeko.Pretty hiding (int)
import Pukeko.Language.Builtins
import Pukeko.Language.Syntax
import Pukeko.Language.Type

import qualified Pukeko.Language.Builtins as Builtins

data Environment s = MkEnvironment
  { _binds :: Map Ident (Type (Open s))
  , _level :: Int
  , _fresh :: STRef s [Ident]
  }
mkLabels [''Environment]

newtype TI s a =
  TI { unTI :: ExceptT String (ReaderT (Environment s) (ST s)) a }
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader (Environment s)
           )

instance MonadFail (TI s) where
  fail = throwError

liftST :: ST s a -> TI s a
liftST = TI . lift . lift

checkExpr :: MonadError String m => Type Closed -> Expr SourcePos -> m ()
checkExpr t_want expr =
  case checkExpr' t_want expr of
    Left error -> throwError error
    Right ()   -> return ()

checkExpr' :: Type Closed -> Expr SourcePos -> Either String ()
checkExpr' t_want expr = runST $ do
  let vars = "$":[ xs ++ [x] | xs <- vars, x <- ['a'..'z'] ]
  _fresh <- newSTRef (map MkIdent vars)
  let env = MkEnvironment
        { _binds = Map.fromList $ map (fmap open) Builtins.everything
        , _level = 1
        , _fresh
        }
  runReaderT (runExceptT (unTI (check t_want expr))) env

freshVar :: TI s (Type (Open s))
freshVar = do
  _level <- asks level
  _fresh <- asks fresh
  _ident:idents <- liftST $ readSTRef _fresh
  liftST $ writeSTRef _fresh idents
  var <- liftST $ newSTRef $ Free { _ident, _level }
  return (TVar var)

occursCheck :: (STRef s (TypeVar (Open s))) -> Type (Open s) -> TI s ()
occursCheck tvr1 t2 =
  case t2 of
    TVar tvr2
      | tvr1 == tvr2 -> throwError "occurs check"
      | otherwise    -> do
          tv2 <- liftST $ readSTRef tvr2
          case tv2 of
            Free{ _ident, _level = level2 } -> do
              Free{ _level = level1 } <- liftST $ readSTRef tvr1
              liftST $ writeSTRef tvr2 $
                Free { _ident, _level = min level1 level2 }
            Link t2' -> occursCheck tvr1 t2'
    QVar _ -> return ()
    TFun tx ty -> occursCheck tvr1 tx >> occursCheck tvr1 ty
    TApp _ ts -> mapM_ (occursCheck tvr1) ts

-- TODO: link compression
unwind :: Type (Open s) -> TI s (Type (Open s))
unwind t =
  case t of
    TVar tvr -> do
      tv <- liftST $ readSTRef tvr
      case tv of
        Link{ _type } -> unwind _type
        Free{} -> return t
    _ -> return t

unify :: Type (Open s) -> Type (Open s) -> TI s ()
unify t1 t2 = do
  t1 <- unwind t1
  t2 <- unwind t2
  case (t1, t2) of
    (TVar tvr1, TVar tvr2) | tvr1 == tvr2 -> return ()
    (TVar tvr1, _) -> do
      Free{ _ident } <- liftST $ readSTRef tvr1
      occursCheck tvr1 t2 `catchError` \_ -> do
        p2 <- liftST $ prettyType prettyNormal 0 t2
        pthrow $ hsep [pretty _ident, text "occurs in", p2]
      liftST $ writeSTRef tvr1 $ Link { _type = t2 }
    (_, TVar _) -> unify t2 t1
    (TFun tx1 ty1, TFun tx2 ty2) -> unify tx1 tx2 >> unify ty1 ty2
    (TApp c1  ts1, TApp c2  ts2)
      | c1 == c2 && length ts1 == length ts2 -> zipWithM_ unify ts1 ts2
    _ -> do
      p1 <- liftST $ prettyType prettyNormal 0 t1
      p2 <- liftST $ prettyType prettyNormal 0 t2
      pthrow $ hsep [text "mismatching types", p1, text "and", p2]

generalize :: Type (Open s) -> TI s (Type (Open s))
generalize t =
  case t of
    TVar tvr -> do
      tv <- liftST $ readSTRef tvr
      case tv of
        Free{ _ident, _level = tv_level } -> do
          cur_level <- asks level
          if tv_level > cur_level
            then return $ QVar _ident
            else return t
        Link{ _type } -> generalize _type
    QVar _ -> return t
    TFun tx ty -> TFun <$> generalize tx <*> generalize ty
    TApp c  ts -> TApp c <$> mapM generalize ts

instantiateMany :: [Type (Open s)] -> TI s [Type (Open s)]
instantiateMany ts = evalStateT (mapM inst ts) Map.empty
  where
    inst :: Type (Open s)
         -> StateT (Map Ident (Type (Open s))) (TI s) (Type (Open s))
    inst t =
      case t of
        TVar tvr -> do
          tv <- lift $ liftST $ readSTRef tvr
          case tv of
            Free{} -> return t
            Link{ _type } -> inst _type
        QVar name -> do
          subst <- get
          case Map.lookup name subst of
            Just t' -> return t'
            Nothing -> do
              t' <- lift freshVar
              put $ Map.insert name t' subst
              return t'
        TFun tx ty -> TFun <$> inst tx <*> inst ty
        TApp c  ts -> TApp c <$> mapM inst ts

instantiate :: Type (Open s) -> TI s (Type (Open s))
instantiate t = head <$> instantiateMany [t]

instantiateAnnots :: [Maybe (Type Closed)] -> TI s [Type (Open s)]
instantiateAnnots = mapM $ \t_opt ->
  case t_opt of
    Nothing -> freshVar
    Just t  -> instantiate (open t)

infer :: Expr SourcePos -> TI s (Type (Open s))
infer expr = do
  let unifyHere t1 t2 =
        unify t1 t2 `catchError` \msg -> throwError (show (annot expr) ++ ": " ++ msg)
  case expr of
    Var{ _ident } -> do
      t_opt <- Map.lookup _ident <$> asks binds
      case t_opt of
        Just t  -> instantiate t
        Nothing -> pthrow $ text "unknown variable:" <+> pretty _ident
    Num{} -> return int
    Pack{} -> pthrow $
      text "Pack expressions should only be introduced after type checking!"
    Ap{ _fun, _arg } -> do
      t_fun <- infer _fun
      t_arg <- infer _arg
      t_res <- freshVar
      unifyHere t_fun (TFun t_arg t_res)
      return t_res
    ApOp{} -> infer (desugarApOp expr)
    Lam{ _patns, _body } -> do
      let (idents, t_annots) = unzipPatns _patns
      t_idents <- instantiateAnnots t_annots
      let env = Map.fromList (zip idents t_idents)
      t_body <- local binds (Map.union env) (infer _body)
      return $ foldr TFun t_body t_idents
    Let{ _isrec = False, _defns, _body } -> do
      let (idents, t_annots, rhss) = unzipDefns3 _defns
      t_rhss <- local level succ $ do
        t_lhss <- instantiateAnnots t_annots
        t_rhss <- mapM infer rhss
        zipWithM_ unifyHere t_lhss t_rhss
        return t_rhss
      t_idents <- mapM generalize t_rhss
      let env = Map.fromList (zip idents t_idents)
      local binds (Map.union env) (infer _body)
    Let{ _isrec = True, _defns, _body } -> do
      let (idents, t_annots, rhss) = unzipDefns3 _defns
      t_rhss <- local level succ $ do
        t_lhss <- instantiateAnnots t_annots
        let env = Map.fromList (zip idents t_lhss)
        t_rhss <- local binds (Map.union env) (mapM infer rhss)
        zipWithM_ unifyHere t_lhss t_rhss
        return t_rhss
      t_idents <- mapM generalize t_rhss
      let env = Map.fromList (zip idents t_idents)
      local binds (Map.union env) (infer _body)
    If{} -> infer (desugarIf expr)
    Match{ _expr, _altns } -> do
      t_expr <- infer _expr
      t_res <- freshVar
      forM_ _altns $ \MkAltn{ _annot, _cons, _patns, _rhs } -> do
        (MkConstructor{ _fields }, MkADT{ _name, _params }) <-
          findConstructor _cons
        if length _patns /= length _fields
          then throwError $ show _annot
               ++ ": wrong number of arguments for constructor " ++ show _cons
          else do
          let (idents, _) = unzipPatns _patns
          (t_params, t_fields) <-
            splitAt (length _params) <$>
            instantiateMany (map open $ _params ++ _fields)
          unify t_expr (app _name t_params)
          let env = Map.fromList (zip idents t_fields)
          t_rhs <- local binds (Map.union env) (infer _rhs)
          unify t_res t_rhs
      return t_res

check :: Type Closed -> Expr SourcePos -> TI s ()
check t_want expr = do
  t_expr <- infer expr
  unify (open t_want) t_expr

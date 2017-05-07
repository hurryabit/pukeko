{-# LANGUAGE RankNTypes, TemplateHaskell #-}
module Pukeko.Language.TypeChecker
  ( checkModule
  )
where

import Control.Monad
import Control.Monad.Fail
import Control.Monad.RWS hiding (asks, local, gets, modify)
import Control.Monad.State hiding (gets, modify)
import Control.Monad.ST
import Data.Label (mkLabels)
import Data.Label.Monadic
import Data.Map (Map)
import Data.STRef
import Text.Parsec (SourcePos)
import qualified Data.Map as Map

import Pukeko.Error
import Pukeko.Pretty
import Pukeko.Language.Syntax
import Pukeko.Language.Type hiding (Type)
import qualified Pukeko.Language.Ident as Ident
import qualified Pukeko.Language.Type as Type

type Type a = Type.Type (ADT Ident.Con) a

data Environment s = MkEnvironment
  { _locals :: Map Ident.Var (Type (Open s))
  , _level  :: Int
  }

data GlobalStatus = Imported | Declared | Defined

data TCState = MkTCState
  { _globals :: Map Ident.Var (Type Closed, GlobalStatus)
  , _fresh   :: [Ident.Var]
  }
mkLabels [''Environment, ''TCState]

newtype TC s a = TC {unTC :: RWST (Environment s) () TCState (ExceptT String (ST s)) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader (Environment s)
           , MonadState TCState
           )

instance MonadFail (TC s) where
  fail = throwError

evalTC :: MonadError String m => (forall s. TC s a) -> m a
evalTC tc =
  let env = MkEnvironment
        { _locals = Map.empty
        , _level  = 0
        }
      st = MkTCState
        { _globals = Map.empty
        , _fresh   = []
        }
  in  fst <$> runST (runExceptT (evalRWST (unTC tc) env st))

liftST :: ST s a -> TC s a
liftST = TC . lift . lift

checkModule :: MonadError String m => Module StageTR SourcePos -> m ()
checkModule module_ = evalTC (mapM_ checkTopLevel module_)

checkTopLevel :: TopLevel StageTR SourcePos -> TC s ()
checkTopLevel top = case top of
  Type{} -> bug "type checker" "type definition" Nothing
  Val{ _annot, _ident, _type } -> do
    look <- modifyAndGet globals $
      Map.insertLookupWithKey (\_ -> const) _ident (_type, Declared)
    case look of
      Nothing -> return ()
      Just _ -> throwAt _annot "duplicate declaration of function" _ident
  Def{ _annot, _isrec, _defns } -> do
    resetFresh
    env <- inferLet _isrec _defns
    forM_ env $ \(ident, t_infer) -> do
      look <- Map.lookup ident <$> gets globals
      case look of
        Just (t_decl, Declared) -> do
          t_infer <- instantiate t_infer
          unify (open t_decl) t_infer `catchError` throwErrorAt _annot
          modify globals $ Map.insert ident (t_decl, Defined)
        Just _  -> throwAt _annot "duplicate definition of function" ident
        Nothing -> throwAt _annot "undeclared function" ident
  Asm{ _annot, _ident } -> do
    look <- Map.lookup _ident <$> gets globals
    case look of
      Just (t_decl, Declared) -> modify globals $ Map.insert _ident (t_decl, Defined)
      Just _  -> throwAt _annot "duplicate definition of function" _ident
      Nothing -> throwAt _annot "undeclared function" _ident

resetFresh :: TC s ()
resetFresh = do
  let vars = "$":[ xs ++ [x] | xs <- vars, x <- ['a'..'z'] ]
  puts fresh (map Ident.variable vars)

freshVar :: TC s (Type (Open s))
freshVar = do
  _level <- asks level
  _ident <- modifyAndGet fresh (\(x:xs) -> (x,xs))
  var <- liftST $ newSTRef $ Free { _ident, _level }
  return (TVar var)

lookupType :: SourcePos -> Ident.Var -> TC s (Type (Open s))
lookupType annot ident = do
  t_local <- Map.lookup ident <$> asks locals
  case t_local of
    Just t -> return t
    Nothing -> do
      t_global <- Map.lookup ident <$> gets globals
      case t_global of
        Just (t, Imported) -> return (open t)
        Just (t, Defined)  -> return (open t)
        Just (_, Declared) -> throwAt annot "undefined function" ident
        Nothing            -> throwAt annot "undeclared function" ident

occursCheck :: (STRef s (TypeVar (TypeConOf StageTR) s)) -> Type (Open s) -> TC s ()
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
unwind :: Type (Open s) -> TC s (Type (Open s))
unwind t =
  case t of
    TVar tvr -> do
      tv <- liftST $ readSTRef tvr
      case tv of
        Link{ _type } -> unwind _type
        Free{} -> return t
    _ -> return t

unify :: Pretty (TypeConOf StageTR) => Type (Open s) -> Type (Open s) -> TC s ()
unify t1 t2 = do
  t1 <- unwind t1
  t2 <- unwind t2
  case (t1, t2) of
    (TVar tvr1, TVar tvr2) | tvr1 == tvr2 -> return ()
    (TVar tvr1, _) -> do
      Free{ _ident } <- liftST $ readSTRef tvr1
      occursCheck tvr1 t2 `catchError` \_ -> do
        p2 <- liftST $ prettyType prettyNormal 0 t2
        throwDoc $ quotes (pretty _ident) <+> "occurs in" <+> p2
      liftST $ writeSTRef tvr1 $ Link { _type = t2 }
    (_, TVar _) -> unify t2 t1
    (QVar name1, QVar name2)
      | name1 == name2 -> return ()
    (TFun tx1 ty1, TFun tx2 ty2) -> unify tx1 tx2 >> unify ty1 ty2
    -- TODO: Make ADT comparable itself.
    (TApp MkADT{_name = c1} ts1, TApp MkADT{_name = c2} ts2)
      | c1 == c2 -> zipWithM_ unify ts1 ts2
    _ -> do
      p1 <- liftST $ prettyType prettyNormal 0 t1
      p2 <- liftST $ prettyType prettyNormal 0 t2
      throwDoc $ "mismatching types" <+> p1 <+> "and" <+> p2

generalize :: Type (Open s) -> TC s (Type (Open s))
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

instantiateMany :: [Type (Open s)] -> TC s [Type (Open s)]
instantiateMany ts = evalStateT (mapM inst ts) Map.empty
  where
    inst :: Type (Open s)
         -> StateT (Map Ident.Var (Type (Open s))) (TC s) (Type (Open s))
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

instantiate :: Type (Open s) -> TC s (Type (Open s))
instantiate t = head <$> instantiateMany [t]

instantiateBind :: BindGen _ StageTR SourcePos -> TC s (Type (Open s))
instantiateBind MkBind{ _annot, _type } = case _type of
  Nothing -> freshVar
  Just t  -> instantiate (open t)

inferLet :: Bool -> [Defn StageTR SourcePos] -> TC s [(Ident.Var, Type (Open s))]
inferLet isrec defns = do
  let (lhss, rhss) = unzipDefns defns
      idents = map (_ident :: Bind _ _ -> _) lhss
  t_rhss <- local level succ $ do
    t_lhss <- mapM instantiateBind lhss
    let env | isrec     = Map.fromList $ zip idents t_lhss
            | otherwise = Map.empty
    t_rhss <- local locals (Map.union env) (mapM infer rhss)
    zipWithM_ unify t_lhss t_rhss
    return t_rhss
  t_idents <- mapM generalize t_rhss
  return $ zip idents t_idents

infer :: Expr StageTR SourcePos -> TC s (Type (Open s))
infer expr = do
  let unifyHere t1 t2 = unify t1 t2 `catchError` throwErrorAt (annot expr)
  case expr of
    Var{ _annot, _var } -> do
      t <- lookupType _annot _var
      instantiate t
    Con{ _annot, _con } -> do
      let t = open (typeOf _con)
      instantiate t
    Num{} -> return (open typeInt)
    Ap{ _fun, _args } -> do
      t_fun <- infer _fun
      t_args <- mapM infer _args
      t_res <- freshVar
      unifyHere t_fun (t_args *~> t_res)
      return t_res
    Lam{ _binds, _body } -> do
      let params = map (_ident :: Bind0 _ _ -> _) _binds
      t_params <- mapM instantiateBind _binds
      let env = Map.fromList $ zipMaybe params t_params
      t_body <- local locals (Map.union env) (infer _body)
      return $ t_params *~> t_body
    Let{ _annot, _isrec, _defns, _body } -> do
      env <- Map.fromList <$> inferLet _isrec _defns `catchError` throwErrorAt _annot
      local locals (Map.union env) (infer _body)
    If{} -> infer (desugarIf expr)
    Match{ _expr, _altns } -> do
      t_expr <- infer _expr
      t_res <- freshVar
      forM_ _altns $ \MkAltn{ _annot, _con = MkConstructor{_name, _adt = adt@MkADT{_params}, _fields}, _binds, _rhs } -> do
        when (length _binds /= length _fields) $
          throwDocAt _annot $ "term cons" <+> quotes (pretty _name) <+>
          "expects" <+> int (length _fields) <+> "arguments"
        let idents = map (_ident :: Bind0 _ _ -> _) _binds
        (t_params, t_fields) <-
          splitAt (length _params) <$> instantiateMany (map open $ map var _params ++ _fields)
        unifyHere t_expr (app adt t_params)
        let env = Map.fromList $ zipMaybe idents t_fields
        t_rhs <- local locals (Map.union env) (infer _rhs)
        unifyHere t_res t_rhs
      return t_res

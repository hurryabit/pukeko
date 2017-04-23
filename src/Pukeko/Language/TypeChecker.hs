{-# LANGUAGE TemplateHaskell #-}
module Pukeko.Language.TypeChecker
  ( checkModule
  )
  where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Reader hiding (asks, local)
import Control.Monad.State  hiding (State, gets, modify)
import Control.Monad.ST
import Data.Label (mkLabels)
import Data.Label.Monadic
import Data.Map (Map)
import Data.STRef
import Text.Parsec (SourcePos)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Pukeko.Pretty hiding (int)
import Pukeko.Language.ADT
import Pukeko.Language.Syntax
import Pukeko.Language.Type

data Environment s = MkEnvironment
  { _locals :: Map Ident (Type (Open s))
  , _level  :: Int
  }

data GlobalStatus = Imported | Declared | Defined

data State = MkState
  { _globals :: Map Ident (Type Closed, GlobalStatus)
  , _types   :: Map Ident ADT
  , _constrs :: Map Ident Constructor
  , _fresh   :: [Ident]
  }
mkLabels [''Environment, ''State]

newtype TI s a =
  TI { unTI :: ExceptT String (ReaderT (Environment s) (StateT State (ST s))) a }
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader (Environment s)
           , MonadState State
           )

instance MonadFail (TI s) where
  fail = throwError

liftST :: ST s a -> TI s a
liftST = TI . lift . lift . lift

checkModule :: MonadError String m => Module SourcePos -> m (Map Ident Constructor)
checkModule tops = do
  let env = MkEnvironment
        { _locals = Map.empty
        , _level  = 0
        }
      st = MkState
        { _globals = Map.empty
        , _types   = Map.empty
        , _constrs = Map.empty
        , _fresh   = []
        }
      (res,st') = runST (runStateT (runReaderT (runExceptT (unTI (mapM_ checkTopLevel tops))) env) st)
  case res of
    Left error -> throwError error
    Right ()   -> return $ _constrs st'

checkTopLevel :: TopLevel SourcePos -> TI s ()
checkTopLevel top = case top of
  Type{ _annot, _adts } -> do
    forM_ _adts (addType _annot)
    forM_ _adts $ \MkADT{ _constructors } ->
      forM_ _constructors (addConstructor _annot)
  Val{ _annot, _ident, _type } -> do
    checkKinds _annot _type
    look <- modifyAndGet globals $
      Map.insertLookupWithKey (\_ -> const) _ident (_type, Declared)
    let throw verb = throwHere _annot $
          "the function " ++ show _ident ++ " cannot be declared, it " ++ verb
    case look of
      Nothing -> return ()
      Just (_, Imported) -> throw "has been imported"
      Just (_, Declared) -> throw "has already been declared"
      Just (_, Defined)  -> throw "has already beed defined"
  Def{ _annot, _isrec, _defns } -> do
    resetFresh
    env <- inferLet _isrec _defns
    forM_ env $ \(ident, t_infer) -> do
      look <- Map.lookup ident <$> gets globals
      let throw verb = throwHere _annot $
            "the function " ++ show ident ++ " cannot be defined, it " ++ verb
      case look of
        Nothing            -> throw "has not been declared"
        Just (_, Imported) -> throw "has been imported"
        Just (_, Defined)  -> throw "has already been defined"
        Just (t_decl, Declared) -> do
          t_infer <- instantiate t_infer
          unify (open t_decl) t_infer `catchError` throwHere _annot
          modify globals $ Map.insert ident (t_decl, Defined)
  Asm{ _annot, _ident } -> do
    look <- Map.lookup _ident <$> gets globals
    let throw verb = throwHere _annot $
          "the function " ++ show _ident ++
          " cannot be defined as assembly, it " ++ verb
    case look of
      Nothing            -> throw "has not been declared"
      Just (_, Imported) -> throw "has been imported"
      Just (_, Defined)  -> throw "has already been defined"
      Just (t_decl, Declared) ->
        modify globals $ Map.insert _ident (t_decl, Defined)


addType :: SourcePos -> ADT -> TI s ()
addType _annot adt@MkADT{ _name } = do
  _types <- gets types
  when (_name `Map.member` _types) $
    throwHere _annot $ "Type " ++ show _name ++ " has already been defined"
  modify types (Map.insert _name adt)

findType :: SourcePos -> Ident -> TI s ADT
findType annot ident = do
  _types <- gets types
  case Map.lookup ident _types of
    Nothing -> throwHere annot $ "Type constructor " ++ show ident ++ " unknown"
    Just t -> return t

addConstructor :: SourcePos -> Constructor -> TI s ()
addConstructor annot constr@MkConstructor{ _adt = MkADT{ _params }, _name, _fields } = do
  _constrs <- gets constrs
  when (_name `Map.member` _constrs) $
    throwHere annot $ "Constructor " ++ show _name ++ " has already been defined"
  let unbound = Set.unions (map qvars _fields) `Set.difference` Set.fromList _params
  unless (Set.null unbound) $
    throwHere annot $ "Unbound type variables in constructor " ++ show _name ++ ": "
      ++ unwords (map show $ Set.toList unbound)
  mapM_ (checkKinds annot) _fields
  modify constrs (Map.insert _name constr)

findConstructor :: SourcePos -> Ident -> TI s Constructor
findConstructor annot ident = do
  _constrs <- gets constrs
  case Map.lookup ident _constrs of
    Nothing -> throwHere annot $ "Constructor " ++ show ident ++ " unknown"
    Just constr -> return constr


resetFresh :: TI s ()
resetFresh = do
  let vars = "$":[ xs ++ [x] | xs <- vars, x <- ['a'..'z'] ]
  puts fresh (map MkIdent vars)

freshVar :: TI s (Type (Open s))
freshVar = do
  _level <- asks level
  _ident <- modifyAndGet fresh (\(x:xs) -> (x,xs))
  var <- liftST $ newSTRef $ Free { _ident, _level }
  return (TVar var)

lookupType :: SourcePos -> Ident -> TI s (Type (Open s))
lookupType annot ident
  | isVariable ident = do
      t_local <- Map.lookup ident <$> asks locals
      case t_local of
        Just t -> return t
        Nothing -> do
          t_global <- Map.lookup ident <$> gets globals
          case t_global of
            Just (t, Imported) -> return (open t)
            Just (t, Defined)  -> return (open t)
            Just (_, Declared) ->
              throwHere annot $ "function " ++ show ident ++ " has not been defined yet"
            Nothing ->
              throwHere annot $ "function " ++ show ident ++ " is unknown"
  | isConstructor ident = do
      constr <- findConstructor annot ident
      return $ open (typeOf constr)
  | otherwise = throwHere annot $ "invalid symbol: " ++ show ident

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
    (QVar name1, QVar name2)
      | name1 == name2 -> return ()
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

instantiateAnnots :: [Bind SourcePos] -> TI s [(Ident, Type (Open s))]
instantiateAnnots binds = forM binds $ \MkBind{ _annot, _ident, _type } -> do
  t <- case _type of
    Nothing -> freshVar
    Just t  -> do
      checkKinds _annot t
      instantiate (open t)
  return (_ident, t)

throwHere :: SourcePos -> String -> TI s a
throwHere annot msg = throwError $ show annot ++ ": " ++ msg

inferLet :: Bool -> [Defn SourcePos] -> TI s [(Ident, Type (Open s))]
inferLet isrec defns = do
  let (binds, rhss) = unzipDefns defns
  t_rhss <- local level succ $ do
    inst_binds <- instantiateAnnots binds
    let env | isrec     = Map.fromList inst_binds
            | otherwise = Map.empty
    t_rhss <- local locals (Map.union env) (mapM infer rhss)
    zipWithM_ unify (map snd inst_binds) t_rhss
    return t_rhss
  t_idents <- mapM generalize t_rhss
  return $ zipWith (\MkBind{ _ident } t -> (_ident, t)) binds t_idents

infer :: Expr SourcePos -> TI s (Type (Open s))
infer expr = do
  let unifyHere t1 t2 = unify t1 t2 `catchError` throwHere (annot expr)
  case expr of
    Var{ _annot, _ident } -> do
      t <- lookupType _annot _ident
      instantiate t
    Num{} -> return (open int)
    Ap{ _fun, _args } -> do
      t_fun <- infer _fun
      t_args <- mapM infer _args
      t_res <- freshVar
      unifyHere t_fun (t_args *~> t_res)
      return t_res
    ApOp{} -> infer (desugarApOp expr)
    Lam{ _binds, _body } -> do
      inst_binds <- instantiateAnnots _binds
      let env = Map.fromList inst_binds
      t_body <- local locals (Map.union env) (infer _body)
      return $ map snd inst_binds *~> t_body
    Let{ _annot, _isrec, _defns, _body } -> do
      env <- Map.fromList <$> inferLet _isrec _defns `catchError` throwHere _annot
      local locals (Map.union env) (infer _body)
    If{} -> infer (desugarIf expr)
    Match{ _expr, _altns } -> do
      t_expr <- infer _expr
      t_res <- freshVar
      forM_ _altns $ \MkAltn{ _annot, _cons, _binds, _rhs } -> do
        MkConstructor{ _adt = MkADT{ _name, _params }, _fields } <- findConstructor _annot _cons
        if length _binds /= length _fields
          then throwHere _annot $
               "wrong number of arguments for constructor " ++ show _cons
          else do
          let (idents, _) = unzipBinds _binds
          (t_params, t_fields) <-
            splitAt (length _params) <$>
            instantiateMany (map open $ map var _params ++ _fields)
          unifyHere t_expr (app _name t_params)
          let env = Map.fromList (zip idents t_fields)
          t_rhs <- local locals (Map.union env) (infer _rhs)
          unifyHere t_res t_rhs
      return t_res

checkKinds :: SourcePos -> Type _ -> TI s ()
checkKinds annot t = case t of
  QVar _ -> return ()
  TVar _ -> return ()
  TFun t_arg t_res -> do
    checkKinds annot t_arg
    checkKinds annot t_res
  TApp ident t_params -> do
    MkADT{ _params } <- findType annot ident
    let arity = length _params
    when (length t_params /= arity) $
      throwHere annot $ "Type constructor " ++ show ident ++ " expects " ++ show arity ++ " parameters"

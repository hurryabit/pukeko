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

import Pukeko.Pretty hiding (int)
import Pukeko.Language.Builtins
import Pukeko.Language.Syntax
import Pukeko.Language.Type

import qualified Pukeko.Language.Builtins as Builtins

data Environment s = MkEnvironment
  { _locals :: Map Ident (Type (Open s))
  , _level  :: Int
  }

data GlobalStatus = Imported | Declared | Defined
data State = MkState
  { _globals :: Map Ident (Type Closed, GlobalStatus)
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

checkModule :: MonadError String m => Module SourcePos -> m ()
checkModule tops = do
  let env = MkEnvironment
        { _locals = Map.empty
        , _level  = 0
        }
      st = MkState
        { _globals = Map.fromList [ (_ident, (_type, Imported))
                                  | (_ident, _type) <- Builtins.everything
                                  ]
        , _fresh   = []
        }
      (res, _) = runST (runStateT (runReaderT (runExceptT (unTI (mapM_ checkTopLevel tops))) env) st)
  case res of
    Left error -> throwError error
    Right ()   -> return ()

checkTopLevel :: TopLevel SourcePos -> TI s ()
checkTopLevel top = case top of
  Val{ _annot, _ident, _type } -> do
    look <- modifyAndGet globals $
      Map.insertLookupWithKey (\_ -> const) _ident (_type, Declared)
    let throw verb = throwHere _annot $
          "the symbol " ++ show _ident ++ " cannot be declared, it " ++ verb
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
            "the symbol " ++ show ident ++ " cannot be defined, it " ++ verb
      case look of
        Nothing            -> throw "has not been declared"
        Just (_, Imported) -> throw "has been imported"
        Just (_, Defined)  -> throw "has already been defined"
        Just (t_decl, Declared) -> do
          t_infer <- instantiate t_infer
          unify (open t_decl) t_infer `catchError` throwHere _annot
          modify globals $ Map.insert ident (t_decl, Defined)


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
lookupType annot ident = do
  t_local <- Map.lookup ident <$> asks locals
  case t_local of
    Just t -> return t
    Nothing -> do
      t_global <- Map.lookup ident <$> gets globals
      case t_global of
        Just (t, Imported) -> return (open t)
        Just (t, Defined)  -> return (open t)
        Just (_, Declared) ->
          throwHere annot $ "symbol " ++ show ident ++ " has not been defined yet"
        Nothing ->
          throwHere annot $ "symbol " ++ show ident ++ " is unknown"

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

instantiateAnnots :: [Maybe (Type Closed)] -> TI s [Type (Open s)]
instantiateAnnots = mapM $ \t_opt ->
  case t_opt of
    Nothing -> freshVar
    Just t  -> instantiate (open t)

throwHere :: SourcePos -> String -> TI s a
throwHere annot msg = throwError $ show annot ++ ": " ++ msg

inferLet :: Bool -> [Defn SourcePos] -> TI s [(Ident, Type (Open s))]
inferLet isrec defns = do
  let (idents, t_annots, rhss) = unzipDefns3 defns
  t_rhss <- local level succ $ do
    t_lhss <- instantiateAnnots t_annots
    let env | isrec     = Map.fromList (zip idents t_lhss)
            | otherwise = Map.empty
    t_rhss <- local locals (Map.union env) (mapM infer rhss)
    zipWithM_ unify t_lhss t_rhss
    return t_rhss
  t_idents <- mapM generalize t_rhss
  return $ zip idents t_idents

infer :: Expr SourcePos -> TI s (Type (Open s))
infer expr = do
  let unifyHere t1 t2 = unify t1 t2 `catchError` throwHere (annot expr)
  case expr of
    Var{ _annot, _ident } -> do
      t <- lookupType _annot _ident
      instantiate t
    Num{} -> return int
    Pack{} -> pthrow $
      text "Pack expressions should only be introduced after type checking!"
    Ap{ _fun, _args } -> do
      t_fun <- infer _fun
      t_args <- mapM infer _args
      t_res <- freshVar
      unifyHere t_fun (t_args *~> t_res)
      return t_res
    ApOp{} -> infer (desugarApOp expr)
    Lam{ _patns, _body } -> do
      let (idents, t_annots) = unzipPatns _patns
      t_idents <- instantiateAnnots t_annots
      let env = Map.fromList (zip idents t_idents)
      t_body <- local locals (Map.union env) (infer _body)
      return $ t_idents *~> t_body
    Let{ _annot, _isrec, _defns, _body } -> do
      env <- Map.fromList <$> inferLet _isrec _defns `catchError` throwHere _annot
      local locals (Map.union env) (infer _body)
    If{} -> infer (desugarIf expr)
    Match{ _expr, _altns } -> do
      t_expr <- infer _expr
      t_res <- freshVar
      forM_ _altns $ \MkAltn{ _annot, _cons, _patns, _rhs } -> do
        (MkConstructor{ _fields }, MkADT{ _name, _params }) <-
          findConstructor _cons
        if length _patns /= length _fields
          then throwHere _annot $
               "wrong number of arguments for constructor " ++ show _cons
          else do
          let (idents, _) = unzipPatns _patns
          (t_params, t_fields) <-
            splitAt (length _params) <$>
            instantiateMany (map open $ _params ++ _fields)
          unifyHere t_expr (app _name t_params)
          let env = Map.fromList (zip idents t_fields)
          t_rhs <- local locals (Map.union env) (infer _rhs)
          unifyHere t_res t_rhs
      return t_res

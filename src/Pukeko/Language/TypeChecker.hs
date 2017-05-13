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
import Data.Foldable
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
  { _locals :: Map Ident.EVar (Type (Open s))
  , _level  :: Int
  }

data GlobalStatus = Imported | Declared | Defined

data TCState = MkTCState
  { _globals :: Map Ident.EVar (Type Closed, GlobalStatus)
  , _fresh   :: [Ident.TVar]
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

resetFresh :: TC s ()
resetFresh = puts fresh Ident.freshTVars

freshUVar :: TC s (Type (Open s))
freshUVar = do
  _level <- asks level
  _ident <- modifyAndGet fresh (\(x:xs) -> (x,xs))
  uref <- liftST $ newSTRef $ Free{_ident, _level}
  return (UVar uref)

lookupType :: SourcePos -> Ident.EVar -> TC s (Type (Open s))
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

occursCheck :: (STRef s (UVar (TypeConOf StageTR) s)) -> Type (Open s) -> TC s ()
occursCheck uref1 t2 =
  case t2 of
    UVar uref2
      | uref1 == uref2 -> throwError "occurs check"
      | otherwise    -> do
          uvar2 <- liftST $ readSTRef uref2
          case uvar2 of
            Free{_ident, _level = level2} -> do
              Free{_level = level1} <- liftST $ readSTRef uref1
              liftST $ writeSTRef uref2 $
                Free {_ident, _level = min level1 level2}
            Link t2' -> occursCheck uref1 t2'
    TVar _ -> return ()
    TFun tx ty -> occursCheck uref1 tx >> occursCheck uref1 ty
    TApp _ ts -> mapM_ (occursCheck uref1) ts

-- TODO: link compression
unwind :: Type (Open s) -> TC s (Type (Open s))
unwind t =
  case t of
    UVar uref -> do
      uvar <- liftST $ readSTRef uref
      case uvar of
        Link{_type} -> unwind _type
        Free{} -> return t
    _ -> return t

unify :: Pretty (TypeConOf StageTR) => SourcePos -> Type (Open s) -> Type (Open s) -> TC s ()
unify pos t1 t2 = do
  t1 <- unwind t1
  t2 <- unwind t2
  case (t1, t2) of
    (UVar uref1, UVar uref2) | uref1 == uref2 -> return ()
    (UVar uref1, _) -> do
      Free{_ident} <- liftST $ readSTRef uref1
      occursCheck uref1 t2 `catchError` \_ -> do
        p2 <- liftST $ prettyType prettyNormal 0 t2
        throwDocAt pos $ quotes (pretty _ident) <+> "occurs in" <+> p2
      liftST $ writeSTRef uref1 $ Link{_type = t2}
    (_, UVar _) -> unify pos t2 t1
    (TVar name1, TVar name2)
      | name1 == name2 -> return ()
    (TFun tx1 ty1, TFun tx2 ty2) -> unify pos tx1 tx2 >> unify pos ty1 ty2
    -- TODO: Make ADT comparable itself.
    (TApp MkADT{_name = c1} ts1, TApp MkADT{_name = c2} ts2)
      | c1 == c2 -> zipWithM_ (unify pos) ts1 ts2
    _ -> do
      p1 <- liftST $ prettyType prettyNormal 0 t1
      p2 <- liftST $ prettyType prettyNormal 0 t2
      throwDocAt pos $ "mismatching types" <+> p1 <+> "and" <+> p2

generalize :: Type (Open s) -> TC s (Type (Open s))
generalize t =
  case t of
    UVar uref -> do
      uvar <- liftST $ readSTRef uref
      case uvar of
        Free{_ident, _level = tv_level} -> do
          cur_level <- asks level
          if tv_level > cur_level
            then return $ TVar _ident
            else return t
        Link{ _type } -> generalize _type
    TVar _ -> return t
    TFun tx ty -> TFun <$> generalize tx <*> generalize ty
    TApp c  ts -> TApp c <$> mapM generalize ts

instantiateMany :: [Type (Open s)] -> TC s [Type (Open s)]
instantiateMany ts = evalStateT (mapM inst ts) Map.empty
  where
    inst :: Type (Open s)
         -> StateT (Map Ident.TVar (Type (Open s))) (TC s) (Type (Open s))
    inst t =
      case t of
        UVar uref -> do
          uvar <- lift $ liftST $ readSTRef uref
          case uvar of
            Free{} -> return t
            Link{ _type } -> inst _type
        TVar name -> do
          subst <- get
          case Map.lookup name subst of
            Just t' -> return t'
            Nothing -> do
              t' <- lift freshUVar
              put $ Map.insert name t' subst
              return t'
        TFun tx ty -> TFun <$> inst tx <*> inst ty
        TApp c  ts -> TApp c <$> mapM inst ts

instantiate :: Type (Open s) -> TC s (Type (Open s))
instantiate t = head <$> instantiateMany [t]

inferLet :: Bool -> [Defn StageTR SourcePos] -> TC s [(Ident.EVar, Type (Open s))]
inferLet isrec defns = do
  let (lhss, rhss) = unzipDefns defns
      idents = map (_ident :: Bind _ _ -> _) lhss
  t_rhss <- local level succ $ do
    t_lhss <- mapM (const freshUVar) lhss
    let env | isrec     = Map.fromList $ zip idents t_lhss
            | otherwise = Map.empty
    t_rhss <- local locals (Map.union env) (mapM infer rhss)
    for_ (zip3 lhss t_lhss t_rhss) $ \(MkBind{_annot}, t_lhs, t_rhs) ->
      unify _annot t_lhs t_rhs
    return t_rhss
  -- TODO: Add test case which makes sure this generalization is not moved into
  -- the scope of the @local@ above.
  t_idents <- mapM generalize t_rhss
  return $ zip idents t_idents

infer :: Expr StageTR SourcePos -> TC s (Type (Open s))
infer expr = do
  let unifyHere t1 t2 = unify (annot expr) t1 t2
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
      t_res <- freshUVar
      unifyHere t_fun (t_args *~> t_res)
      return t_res
    Lam{ _binds, _body } -> do
      let params = map (_ident :: Bind0 _ _ -> _) _binds
      t_params <- mapM (const freshUVar) _binds
      let env = Map.fromList $ zipMaybe params t_params
      t_body <- local locals (Map.union env) (infer _body)
      return $ t_params *~> t_body
    Let{ _annot, _isrec, _defns, _body } -> do
      env <- Map.fromList <$> inferLet _isrec _defns `catchError` throwErrorAt _annot
      local locals (Map.union env) (infer _body)
    If{} -> infer (desugarIf expr)
    Match{ _expr, _altns } -> do
      t_expr <- infer _expr
      t_res <- freshUVar
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
          unify _annot (open t_decl) t_infer
          modify globals $ Map.insert ident (t_decl, Defined)
        Just _  -> throwAt _annot "duplicate definition of function" ident
        Nothing -> throwAt _annot "undeclared function" ident
  Asm{ _annot, _ident } -> do
    look <- Map.lookup _ident <$> gets globals
    case look of
      Just (t_decl, Declared) -> modify globals $ Map.insert _ident (t_decl, Defined)
      Just _  -> throwAt _annot "duplicate definition of function" _ident
      Nothing -> throwAt _annot "undeclared function" _ident

checkModule :: MonadError String m => Module StageTR SourcePos -> m ()
checkModule module_ = evalTC (mapM_ checkTopLevel module_)

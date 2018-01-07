{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Pukeko.SystemF where

import           Control.Lens
import           Control.Monad         (ap)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Finite
import           Data.Foldable
import           Data.Forget
import qualified Data.List.NonEmpty    as NE
import qualified Data.Map              as Map
import           Data.Proxy
import           Data.Tagged
import           Data.Type.Equality
import           Data.Void
import           Data.Vector.Sized     as Vec
import           GHC.TypeLits

data Scope i v
  = Bound i (Forget String)
  | Free  v
  deriving (Eq, Functor)

type OneScope = Scope ()

type FinScope n = Scope (Finite n)

dist :: Applicative f => Scope i (f v) -> f (Scope i v)
dist = \case
  Bound i h -> pure (Bound i h)
  Free  t   -> fmap Free t

(>>>=) :: Monad f => f (Scope i v1) -> (v1 -> f v2) -> f (Scope i v2)
t >>>= f = t >>= dist . fmap f

(//) :: Monad f => f (Scope i tv) -> (i -> f tv) -> f tv
(//) t f = t >>= \case
  Bound i _ -> f i
  Free  x   -> pure x

class (Eq i, Functor (EnvLevel i)) => VarLevel i where
  type EnvLevel i :: * -> *
  lookupEnvLevel :: i -> EnvLevel i a -> a

class (Eq v, Functor (Env v)) => Var v where
  data Env v :: * -> *
  lookupEnv :: v -> Env v a -> a

instance Var Void where
  data Env Void a = VoidEnv
  lookupEnv = const . absurd

instance (VarLevel i, Var v) => Var (Scope i v) where
  data Env (Scope i v) a = ScopeEnv (EnvLevel i a) (Env v a)
  lookupEnv x (ScopeEnv bnd fre) = case x of
    Bound i _ -> lookupEnvLevel i bnd
    Free  y   -> lookupEnv y fre

extendEnv :: forall v i a. EnvLevel i a -> Env v a -> Env (Scope i v) a
extendEnv = ScopeEnv

data Kind = KStar

data DataIdTag
type DataId = Tagged DataIdTag String

data ConsIdTag
type ConsId = Tagged ConsIdTag String

data Some f = forall (n :: Nat). Some (f n)

data DataDecl n = DataDecl DataId (Vector n TId) (Some (DataCons n))

data DataCons n m = DataCons (DataDecl n) ConsId (Vector m (Type (FinScope n Void)))

data TIdTag
type TId = Tagged TIdTag String

data Type tv
  = TVar tv
  | TFun (Type tv) (Type tv)
  | TForall (Forget TId) (Type (OneScope tv))
  | TInt
  | forall n. KnownNat n => TDat (DataDecl n) (Vector n (Type tv))

data EIdTag
type EId = Tagged EIdTag String

data Expr tv ev
  = EVar    ev
  | EApp    (Expr tv ev) (Expr tv ev)
  | EAbs    (Forget EId) (Type tv) (Expr tv (OneScope ev))
  | ETyApp  (Expr tv ev) (Type tv)
  | ETyAbs  (Forget TId) (Expr (OneScope tv) ev)
  | forall n. (KnownNat n) =>
    ELet    (Vector n (Defn tv ev)) (Expr tv (FinScope n ev))
  | forall n. (KnownNat n) =>
    ELetRec (Vector n (Defn tv (FinScope n ev))) (Expr tv (FinScope n ev))
  | EInt    Int
  | EPlus   (Expr tv ev) (Expr tv ev)
  | EIf0    (Expr tv ev) (Expr tv ev) (Expr tv ev)
  | forall n m. (KnownNat n, KnownNat m) =>
    EData   (DataCons n m) (Vector n (Type tv)) (Vector m (Expr tv ev))
  | EMatch  (Expr tv ev) (NE.NonEmpty (Altn tv ev))

data Defn tv ev = Defn (Forget EId) (Type tv) (Expr tv ev)

data Patn
  = PBind EId
  | forall n m. (KnownNat n, KnownNat m) =>
    PData (DataCons n m) (Vector m Patn)

data Altn tv ev = Altn Patn (Expr tv (Scope EId ev))

data TCEnv tv ev = TCEnv
  { _kindEnv :: Env tv Kind
  , _typeEnv :: Env ev (Type tv)
  }

newtype TC tv ev a = TC{unTC :: ReaderT (TCEnv tv ev) (Except String) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader (TCEnv tv ev)
           )

makeLenses ''TCEnv

lookupType :: (Var ev) => ev -> TC tv ev (Type tv)
lookupType x = lookupEnv x <$> view typeEnv

extendTypeEnv ::
  forall tv ev i a.
  EnvLevel i (Type tv) -> TC tv (Scope i ev) a -> TC tv ev a
extendTypeEnv ts = TC . withReaderT (typeEnv %~ extendEnv @ev ts) . unTC

extendKindEnv :: forall tv ev a. (Var ev) => Kind -> TC (OneScope tv) ev a -> TC tv ev a
extendKindEnv k = TC . withReaderT f . unTC
  where
    f (TCEnv kenv tenv) =
      TCEnv (extendEnv @tv (Identity k) kenv) ((fmap . fmap) Free tenv)

typeOf :: forall tv ev. (Var tv, Var ev) => Expr tv ev -> TC tv ev (Type tv)
typeOf = \case
  EVar x -> lookupType x
  EApp e1 e2 -> do
    t1 <- typeOf e1
    case t1 of
      TFun tx ty -> do
        check e2 tx "argument type does not match"
        pure ty
      _ -> throwError "trying to apply non-function"
  EAbs _ t1 e1 -> TFun t1 <$> extendTypeEnv (Identity t1) (typeOf e1)
  ETyApp e1 t2 -> do
    t1 <- typeOf e1
    case t1 of
      TForall _ tq -> pure (tq // (\() -> t2))
      _            -> throwError "trying to type apply non-quantification"
  ETyAbs h e1 -> TForall h <$> extendKindEnv KStar (typeOf e1)
  ELet ds e2 -> do
    for_ ds $ \(Defn _ t1 e1) -> check e1 t1 "let type does not match"
    extendTypeEnv (fmap (\(Defn _ t1 _) -> t1) ds) (typeOf e2)
  ELetRec ds e2 -> do
    extendTypeEnv (fmap (\(Defn _ t1 _) -> t1) ds) $ do
      for_ ds $ \(Defn _ t1 e1) -> check e1 t1 "let rec type does not match"
      typeOf e2
  EInt _ -> pure TInt
  EPlus e1 e2 -> do
    check e1 TInt "1st argument to + is not an int"
    check e2 TInt "2nd argument to + is not an int"
    pure TInt
  EIf0 e1 e2 e3 -> do
    check e1 TInt "scrutinee of if0 is not an int"
    t <- typeOf e2
    check e3 t "branches of if0 have different types"
    pure t
  EData (DataCons decl _ tfs0) tas efs -> do
    let tfs = fmap (\tf0 -> (fmap . fmap) absurd tf0 // (tas Vec.!)) tfs0
    Vec.zipWithM_ (\ef tf -> check ef tf "field types do not match") efs tfs
    pure (TDat decl tas)
  EMatch e0 (a1 NE.:| as) -> do
    t0 <- typeOf e0
    t1 <- typeOfAltn t0 a1
    for_ as $ \a2 -> do
      t2 <- typeOfAltn t0 a2
      unless (t1 == t2) (throwError "alternatives have different types")
    pure t1

check :: (Var tv, Var ev) => Expr tv ev -> Type tv -> String -> TC tv ev ()
check e t m = do
  t' <- typeOf e
  unless (t == t') (throwError m)

patnEnvLevel :: Patn -> Type tv -> TC tv ev (EnvLevel EId (Type tv))
patnEnvLevel p t = case p of
  PBind x -> pure (Map.singleton x t)
  PData (DataCons decl1 _ tfs0) ps ->
    case t of
      TDat decl2 tas ->
        case sameNat (natProxy decl1) (natProxy decl2) of
          Just Refl
            | decl1 == decl2 -> do
                let tfs = fmap (\tf0 -> (fmap . fmap) absurd tf0 // (tas Vec.!)) tfs0
                envs <- Vec.zipWithM patnEnvLevel ps tfs
                pure (fold envs)
          _ -> throwError "pattern data type does not match scrutinee data type"
      _ -> throwError "pattern data type does not match scrutinee data type"

typeOfAltn :: (Var tv, Var ev) => Type tv -> Altn tv ev -> TC tv ev (Type tv)
typeOfAltn t0 (Altn p e) = do
  env <- patnEnvLevel p t0
  extendTypeEnv env (typeOf e)

natProxy :: forall (n :: Nat) p. p n -> Proxy n
natProxy _ = Proxy

instance Applicative (Scope i) where
  pure = return
  (<*>) = ap

instance Monad (Scope i) where
  return = Free
  s >>= f = case s of
    Bound i h -> Bound i h
    Free  x   -> f x

instance VarLevel () where
  type EnvLevel () = Identity
  lookupEnvLevel () = runIdentity

instance VarLevel (Finite n) where
  type EnvLevel (Finite n) = Vector n
  lookupEnvLevel i env = env Vec.! i

instance VarLevel EId where
  type EnvLevel EId = Map.Map EId
  lookupEnvLevel i env = env Map.! i

deriving instance Functor (Env Void)

deriving instance (Functor (EnvLevel i), Functor (Env v)) => Functor (Env (Scope i v))

instance Eq (DataDecl n) where
  DataDecl d1 _ _ == DataDecl d2 _ _ = d1 == d2

instance Eq (DataCons n m) where
  DataCons d1 c1 _ == DataCons d2 c2 _ = d1 == d2 && c1 == c2

deriving instance Functor Type

instance (Eq tv) => Eq (Type tv) where
  t1 == t2 = case (t1, t2) of
    (TVar x1      , TVar x2      ) -> x1 == x2
    (TFun tx1 ty1 , TFun tx2 ty2 ) -> tx1 == tx2 && ty1 == ty2
    (TForall _ tq1, TForall _ tq2) -> tq1 == tq2
    (TInt         , TInt         ) -> True
    (TDat d1 ts1  , TDat d2 ts2  ) ->
      case sameNat (natProxy d1) (natProxy d2) of
        Nothing   -> False
        Just Refl -> d1 == d2 && ts1 == ts2
      where
    (TVar{}   , _) -> False
    (TFun{}   , _) -> False
    (TForall{}, _) -> False
    (TInt     , _) -> False
    (TDat{}   , _) -> False

instance Applicative Type where
  pure = return
  (<*>) = ap

instance Monad Type where
  return = TVar
  t >>= f = case t of
    TVar x       -> f x
    TInt         -> TInt
    TDat d  ts   -> TDat d (fmap (>>= f) ts)
    TFun tx ty   -> TFun (tx >>= f) (ty >>= f)
    TForall h tq -> TForall h (tq >>>= f)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Pukeko.Language.PatternMatcher
  ( Module
  , compileModule
  )
where

import           Control.Lens     (Traversal, ifoldMap)
import           Control.Monad.State
import           Data.Bifunctor   (second)
import           Data.Either      (partitionEithers)
import           Data.Foldable    (foldlM, toList)
import           Data.Forget      (Forget (..))
import qualified Data.List.Sized  as LS
import qualified Data.Map         as Map
import           Data.Traversable (for)

import           Pukeko.Error
import           Pukeko.Language.Base.AST
import           Pukeko.Language.PatternMatcher.AST
import qualified Pukeko.Language.TypeChecker.AST    as In
import qualified Pukeko.Language.Type               as Ty
import qualified Pukeko.Language.Ident              as Id

newtype PM a = PM{unPM :: StateT [Id.EVar] (Except String) a}
  deriving ( Functor, Applicative, Monad
           , MonadState [Id.EVar]
           , MonadError String
           )

evalPM :: MonadError String m => PM a -> m a
evalPM pm = runExcept $ evalStateT (unPM pm) []

freshEVar :: PM Id.EVar
freshEVar = state (\(x:xs) -> (x, xs))

name :: Ty.Constructor _ -> Id.Con
name Ty.MkConstructor{_name} = _name

pmExpr :: In.Expr v -> PM (Expr v)
pmExpr = \case
  In.Var w x          -> pure $ Var w x
  In.Con w c          -> pure $ Con w c
  In.Num w n          -> pure $ Num w n
  In.App w t  us      -> App w <$> pmExpr t <*> traverse pmExpr us
  -- In.If  w t  u  v -> If  w <$> pmExpr t <*> pmExpr u <*> pmExpr v
  In.Lam w bs t       -> Lam w bs <$> pmExpr t
  In.Let w ds t       -> Let w <$> (traverse . rhs2) pmExpr ds <*> pmExpr t
  In.Rec w ds t       -> Rec w <$> (traverse . rhs2) pmExpr ds <*> pmExpr t
  In.Mat w t0 as0     -> LS.withList as0 $ \case
    LS.Nil -> bug "pattern matcher" "no alternatives" Nothing
    as1@LS.Cons{} -> do
      t1 <- pmExpr t0
      pmMatch w (mkRowMatch1 t1 as1)

pmTopLevel :: In.TopLevel -> PM TopLevel
pmTopLevel = \case
  Def w f t -> do
    put $ Id.freshEVars "pm" f
    Def w f <$> pmExpr t
  asm@Asm{} -> pure asm

compileModule :: MonadError String m => In.Module -> m Module
compileModule = evalPM . traverse pmTopLevel

pmMatch ::
  forall m m' n v. m ~ 'LS.Succ m' =>
  Pos -> RowMatch m n v -> PM (Expr v)
pmMatch w rowMatch0 = do
  let colMatch1 = rowToCol rowMatch0
  elimBindCols w colMatch1 $ \case
    MkColMatch LS.Nil (LS.Cons u2 us2)
      | LS.Nil    <- us2 -> pmExpr $ fmap (strengthen "pattern matcher") u2
      | LS.Cons{} <- us2 -> throwErrorAt w "overlapping patterns"
    colMatch2@(MkColMatch LS.Cons{} _) -> do
      (destCol, colMatch3) <- findDestCol w colMatch2
      let rowMatch4 = colToRow colMatch3
      grpMatch <- groupDests w destCol rowMatch4
      grpMatchExpr w grpMatch

type RhsExpr v = In.Expr (Scope Id.EVar v)

data Row n v = MkRow (LS.List n In.Patn) (RhsExpr v)

data RowMatch m n v = MkRowMatch (LS.List n (Expr v)) (LS.List m (Row n v))

mkRow1 :: In.Altn v -> Row LS.One v
mkRow1 (MkAltn _ p t) = MkRow (LS.Singleton p) t

mkRowMatch1 :: Expr v -> LS.List m (In.Altn v) -> RowMatch m LS.One v
mkRowMatch1 t as = MkRowMatch (LS.Singleton t) (LS.map mkRow1 as)

data Col m v a = MkCol (Expr v) (LS.List m a)

data ColMatch m n v = MkColMatch (LS.List n (Col m v In.Patn)) (LS.List m (RhsExpr v))

colPatn :: Traversal (Col m v a) (Col m v b) a b
colPatn f (MkCol t ps) = MkCol t <$> traverse f ps

rowToCol :: RowMatch m n v -> ColMatch m n v
rowToCol (MkRowMatch ts rs) =
  let (pss, us) = LS.unzipWith (\(MkRow ps u) -> (ps, u)) rs
      cs = LS.zipWith MkCol ts (LS.transpose ts pss)
  in  MkColMatch cs us

colToRow :: ColMatch m n v -> RowMatch m n v
colToRow (MkColMatch cs us) =
  let (ts, pss) = LS.unzipWith (\(MkCol t ps) -> (t, ps)) cs
      rs = LS.zipWith MkRow (LS.transpose us pss) us
  in  MkRowMatch ts rs

elimBindCols :: Pos -> ColMatch m n v -> (forall n'. ColMatch m n' v -> PM a) -> PM a
elimBindCols w (MkColMatch cs0 us0) k = do
  let (cs1, bcs) = partitionEithers (map isBindCol (toList cs0))
  us1 <- foldlM (applyBindCol w) us0 bcs
  LS.withList cs1 $ \cs2 -> k (MkColMatch cs2 us1)
  where
    isBindCol :: Col n v In.Patn -> Either (Col n v In.Patn) (Col n v Bind)
    isBindCol (MkCol t ps) =
      case traverse patnToBind ps of
        Just bs -> Right (MkCol t bs)
        Nothing -> Left  (MkCol t ps)
    applyBindCol ::
      Pos -> LS.List m (RhsExpr v) -> Col m v Bind -> PM (LS.List m (RhsExpr v))
    applyBindCol w rhss (MkCol t bs) = case t of
      In.Var _ x ->
        let replacePatnWithX rhs = \case
              Wild _   -> rhs
              Name _ y ->
                let replaceYwithX = \case
                      Bound z _ | y == z -> Free x
                      b                  -> b
                in  fmap replaceYwithX rhs
        in  pure $ LS.zipWith replacePatnWithX rhss bs
      _ -> throwErrorAt w "pattern match too simple, use a let binding instead"

data Dest = MkDest In.ExprCon [In.Patn]

patnToDest :: In.Patn -> Maybe Dest
patnToDest = \case
  Bind   _    -> Nothing
  Dest _ c ps -> Just (MkDest c ps)

findDestCol ::
  Pos -> ColMatch m ('LS.Succ n) v -> PM (Col m v Dest, ColMatch m n v)
findDestCol w (MkColMatch cs0 us) =
  case find (colPatn patnToDest) cs0 of
    Nothing       -> throwErrorAt w "cannot apply constructor rule"
    Just (c, cs1) -> pure (c, MkColMatch cs1 us)
  where
    find :: (a -> Maybe b) -> LS.List ('LS.Succ n) a -> Maybe (b, LS.List n a)
    find f = \case
      LS.Cons (f -> Just y) xs           -> Just (y, xs)
      LS.Cons _             LS.Nil       -> Nothing
      LS.Cons x             xs@LS.Cons{} -> fmap (second (LS.Cons x)) (find f xs)

data GrpMatchItem v =
  forall m m' n. m ~ 'LS.Succ m' =>
  MkGrpMatchItem In.ExprCon [Bind] (RowMatch m n (Scope Int v))

data GrpMatch v = MkGrpMatch (Expr v) [GrpMatchItem v]

groupDests ::
  forall m m' n v. m ~ 'LS.Succ m' =>
  Pos -> Col m v Dest -> RowMatch m n v -> PM (GrpMatch v)
groupDests w (MkCol t ds@(LS.Cons (MkDest con0 _) _)) (MkRowMatch ts rs) = do
  let drs = toList (LS.zip ds rs)
  grps <- for (Ty._constructors (Ty._adt con0)) $ \con1 -> do
    let drs1 = filter (\(MkDest con2 _, _)-> name con1 == name con2) drs
    LS.withList drs1 $ \case
      LS.Nil -> throwAt w "unmatched constructor" (name con1)
      LS.Cons (MkDest con (traverse patnToBind -> Just bs), MkRow qs u) LS.Nil -> do
        let mp = ifoldMap (\i -> maybe mempty (\x -> Map.singleton x i) . bindName) bs
        let row = MkRow qs (fmap (abstract1 (`Map.lookup` mp)) u)
        let ts1 = LS.map (fmap weaken) ts
        pure $ MkGrpMatchItem con bs (MkRowMatch ts1 (LS.Singleton row))
      drs2@(LS.Cons (MkDest con ps0, _) _) -> do
        xs <- traverse (const freshEVar) ps0
        LS.withList (zip [0..] xs)$ \ixs -> do
          grpRows <- for drs2 $ \(MkDest _ ps, MkRow qs u) ->
            case LS.match ixs ps of
              Nothing  -> bug "pattern matcher" "wrong number of patterns" Nothing
              Just ps1 -> pure $ MkRow (ps1 LS.++ qs) (fmap weaken1 u)
          let ts1 = LS.map (Var w . uncurry bound ) ixs LS.++ LS.map (fmap weaken) ts
          pure $ MkGrpMatchItem con (map (Name w) xs) (MkRowMatch ts1 grpRows)
  pure $ MkGrpMatch t grps

grpMatchExpr :: Pos -> GrpMatch v -> PM (Expr v)
grpMatchExpr w (MkGrpMatch t is) =
  Mat w t <$> traverse (grpMatchItemAltn w) is

grpMatchItemAltn :: Pos -> GrpMatchItem v -> PM (Altn v)
grpMatchItemAltn w (MkGrpMatchItem con bs rm) =
  MkAltn w (Simp w con bs) <$> fmap (fmap f) (pmMatch w rm)
  where
    f = \case
      Bound _ (Forget x) -> Bound x (Forget x)
      Free x             -> Free x

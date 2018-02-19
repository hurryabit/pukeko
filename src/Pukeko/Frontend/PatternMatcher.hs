{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Pukeko.FrontEnd.PatternMatcher
  ( compileModule
  )
where

import Pukeko.Prelude

import           Control.Monad.Freer.Supply
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.Sized  as LS
import qualified Data.Set         as Set

import           Pukeko.FrontEnd.Info
import           Pukeko.AST.Type
import           Pukeko.AST.SystemF
import           Pukeko.AST.Language
import           Pukeko.AST.Name
import           Pukeko.AST.ConDecl

type In  = Typed
type Out = Unnested

type GlobalEffs effs = Members [Reader ModuleInfo, NameSource, Error Failure] effs

type CanPM effs = (Members [Supply Int, Reader SourcePos] effs, GlobalEffs effs)

freshEVar :: CanPM effs => Eff effs (Name EVar)
freshEVar = do
  n <- fresh @Int
  mkName (Lctd noPos (Tagged ("pm$" ++ show n)))

pmExpr :: CanPM effs => Expr In tv ev -> Eff effs (Expr Out tv ev)
pmExpr = \case
  ELoc le         -> here le $ ELoc <$> lctd pmExpr le
  EVar x          -> pure (EVar x)
  EAtm a          -> pure (EAtm a)
  EApp e  a       -> EApp <$> pmExpr e <*> pmExpr a
  ELam b  e       -> ELam b <$> pmExpr e
  ELet ds t       -> ELet <$> traverse (defn2expr pmExpr) ds <*> pmExpr t
  ERec ds t       -> ERec <$> traverse (defn2expr pmExpr) ds <*> pmExpr t
  EMat t0 as0     -> LS.withNonEmpty as0 $ \as1 -> do
      t1 <- pmExpr t0
      pmMatch (mkRowMatch1 t1 as1)
  ETyCoe c e0  -> ETyCoe c <$> pmExpr e0
  ETyAbs xs e0 -> ETyAbs xs <$> pmExpr e0
  ETyApp e0 t  -> ETyApp <$> pmExpr e0 <*> pure t
  ETyAnn t  e  -> ETyAnn t <$> pmExpr e

pmFuncDecl :: GlobalEffs effs => FuncDecl In tv -> Eff effs (FuncDecl Out tv)
pmFuncDecl decl@(MkFuncDecl name typ_ body) =
  MkFuncDecl name typ_ <$> pmExpr body
  & evalSupply @Int [1 ..]
  & runReader (getPos decl)

pmDecl :: GlobalEffs effs => Decl In -> Eff effs (Decl Out)
pmDecl = \case
  DType ds -> pure (DType ds)
  DFunc func -> DFunc <$> pmFuncDecl func
  DExtn (MkExtnDecl name typ_ extn) -> pure (DExtn (MkExtnDecl name typ_ extn))
  DClss c -> pure (DClss c)
  DInst i -> DInst <$> (inst2methods . traverse) pmFuncDecl i

compileModule :: Members [NameSource, Error Failure] effs =>
  Module In -> Eff effs (Module Out)
compileModule m0 = module2decls (traverse pmDecl) m0 & runInfo m0


pmMatch ::
  forall m m' n tv ev effs. (CanPM effs, m ~ 'LS.Succ m') =>
  RowMatch m n tv ev -> Eff effs (Expr Out tv ev)
pmMatch rowMatch0 = do
  let colMatch1 = rowToCol rowMatch0
  elimBPatnCols colMatch1 $ \case
    MkColMatch LS.Nil (LS.Cons u2 us2)
      | LS.Nil    <- us2 -> pmExpr (fmap unsafeStrengthenScope u2)
      | LS.Cons{} <- us2 -> throwHere "overlapping patterns"
    colMatch2@(MkColMatch LS.Cons{} _) -> do
      (conCol, colMatch3) <- findCPatnCol colMatch2
      let rowMatch4 = colToRow colMatch3
      grpMatch <- groupCPatns conCol rowMatch4
      grpMatchExpr grpMatch

-- | A binding pattern, i.e., not a constructor pattern.
data BPatn
  = BWild
  | BName (Name EVar)

bindName :: BPatn -> Maybe (Name EVar)
bindName = \case
  BWild   -> Nothing
  BName x -> Just x

patnToBPatn :: Patn In tv -> Maybe BPatn
patnToBPatn = \case
  PWld   -> Just (BWild)
  PVar x -> Just (BName x)
  PCon{} -> Nothing

-- | The expression on the RHS of a pattern match alternative.
type RhsExpr tv ev = Expr In tv (EScope (Name EVar) ev)

-- | A row of a pattern match, i.e., an alternative.
data Row n tv ev = MkRow (LS.List n (Patn In tv)) (RhsExpr tv ev)

-- | A pattern match in it's row-major representation.
data RowMatch m n tv ev =
  MkRowMatch (LS.List n (Expr Out tv ev)) (LS.List m (Row n tv ev))

-- | Translate an alternative of a pattern match with one column into a row.
mkRow1 :: Altn In tv ev -> Row LS.One tv ev
mkRow1 (MkAltn p t) = MkRow (LS.Singleton p) t

-- | Tanslate a pattern match with one column into its row-major representation.
mkRowMatch1 :: Expr Out tv ev -> LS.List m (Altn In tv ev) -> RowMatch m LS.One tv ev
mkRowMatch1 t as = MkRowMatch (LS.Singleton t) (LS.map mkRow1 as)

-- | A column of a pattern match.
data Col m tv ev a = MkCol (Expr Out tv ev) (LS.List m a)

-- | A pattern match in it's column-major representation.
data ColMatch m n tv ev =
  MkColMatch (LS.List n (Col m tv ev (Patn In tv))) (LS.List m (RhsExpr tv ev))

-- | Traversal of a 'Col'.
colPatn :: Traversal (Col m tv ev a) (Col m tv ev b) a b
colPatn f (MkCol t ps) = MkCol t <$> traverse f ps

-- | Turn the row-major representation of a pattern match into its column-major
-- representation.
rowToCol :: RowMatch m n tv ev -> ColMatch m n tv ev
rowToCol (MkRowMatch ts rs) =
  let (pss, us) = LS.unzipWith (\(MkRow ps u) -> (ps, u)) rs
      cs = LS.zipWith MkCol ts (LS.transpose ts pss)
  in  MkColMatch cs us

-- | Turn the column-major representation of a pattern match into its row-major
-- representation.
colToRow :: ColMatch m n tv ev -> RowMatch m n tv ev
colToRow (MkColMatch cs us) =
  let (ts, pss) = LS.unzipWith (\(MkCol t ps) -> (t, ps)) cs
      rs = LS.zipWith MkRow (LS.transpose us pss) us
  in  MkRowMatch ts rs

-- | Eliminate all columns which contain only binding patterns by "inlining"
-- them into the RHS. This works under the assumption that all scrutinees
-- consist of just single bound variable. For instance, let @p@, @q@ be bound
-- variables, @x@, @y@ be binders and @e@, @f@, @g@ be arbitrary expressions.
-- Then,
--
-- > match p, q with
-- > | x, LT -> e
-- > | y, EQ -> f
-- > | _, GT -> g
--
-- is transformed into
--
-- > match q with
-- > | LT -> e[p/x]
-- > | EQ -> f[p/y]
-- > | GT -> g
elimBPatnCols :: CanPM effs =>
  ColMatch m n tv ev -> (forall n'. ColMatch m n' tv ev -> Eff effs a) -> Eff effs a
elimBPatnCols (MkColMatch cs0 us0) k = do
  let (cs1, bcs) = partitionEithers (map isBPatnCol (toList cs0))
  us1 <- foldlM applyBPatnCol us0 bcs
  LS.withList cs1 $ \cs2 -> k (MkColMatch cs2 us1)
 where
    isBPatnCol ::
      Col n tv ev (Patn In tv) -> Either (Col n tv ev (Patn In tv)) (Col n tv ev BPatn)
    isBPatnCol (MkCol t ps) =
      case traverse patnToBPatn ps of
        Just bs -> Right (MkCol t bs)
        Nothing -> Left  (MkCol t ps)
    applyBPatnCol :: CanPM effs =>
      LS.List m (RhsExpr tv ev) ->
      Col m tv ev BPatn ->
      Eff effs (LS.List m (RhsExpr tv ev))
    applyBPatnCol rhss (MkCol t bs) = case t of
      EVar x ->
        let replacePatnWithX rhs = \case
              BWild   -> rhs
              BName y ->
                let replaceYwithX = \case
                      Bound z _ | y == z -> Free x
                      b                  -> b
                in  fmap replaceYwithX rhs
        in  pure $ LS.zipWith replacePatnWithX rhss bs
      _ -> throwHere "pattern match too simple, use a let binding instead"

-- | A constructor pattern, i.e., neither a wildcard nor a variable pattern.
data CPatn tv = MkCPatn (Name DCon) [Type tv] [Patn In tv]

patnToCPatn :: forall tv. Patn In tv -> Maybe (CPatn tv)
patnToCPatn = \case
  PWld{}      -> Nothing
  PVar{}      -> Nothing
  PCon c ts ps -> Just (MkCPatn c ts ps)

-- | Find the first column of a pattern match which consists entirely of
-- constructor patterns.
findCPatnCol :: CanPM effs =>
  ColMatch m ('LS.Succ n) tv ev -> Eff effs (Col m tv ev (CPatn tv), ColMatch m n tv ev)
findCPatnCol (MkColMatch cs0 us) =
  case find (colPatn patnToCPatn) cs0 of
    Nothing       -> throwHere "cannot apply constructor rule"
    Just (c, cs1) -> pure (c, MkColMatch cs1 us)
  where
    find :: (a -> Maybe b) -> LS.List ('LS.Succ n) a -> Maybe (b, LS.List n a)
    find f = \case
      LS.Cons (f -> Just y) xs           -> Just (y, xs)
      LS.Cons _             LS.Nil       -> Nothing
      LS.Cons x             xs@LS.Cons{} -> fmap (second (LS.Cons x)) (find f xs)

-- | A single group of a grouped pattern match.
data GrpMatchItem tv ev =
  forall m m' n. m ~ 'LS.Succ m' =>
  MkGrpMatchItem (Name DCon) [Type tv] [BPatn] (RowMatch m n tv (EScope (Name EVar) ev))

-- | A grouped pattern match. They result from the transformation in
-- 'groupCPatn'.
data GrpMatch tv ev = MkGrpMatch (Expr Out tv ev) (NonEmpty (GrpMatchItem tv ev))

-- | Group a pattern match by its first column. For instance,
--
-- > match p, q with
-- > | Cons True  xs, x -> e
-- > | Cons False ys, y -> f
-- > | Nil          , z -> g
--
-- is transformed into
--
-- > match p with
-- > | Cons _1 _2 -> match _1, _2, q with
-- >                 | True , xs, x -> e
-- >                 | False, ys, y -> f
-- > | Nil -> match q with
-- >          | z -> g
--
-- where @_1@ and @_2@ are fresh binders. The 'GrpMatch' typed is used to
-- represent the "constructor groups", i.e., the outer @Cons@ and @Nil@
-- alternatives together with the pattern matches on their RHSs.
--
-- Fresh binders are only introduced when necessary, i.e., when a certain
-- constructor appears mutliple times in the first column.
groupCPatns ::
  forall m m' n tv ev effs. (CanPM effs, m ~ 'LS.Succ m') =>
  Col m tv ev (CPatn tv) -> RowMatch m n tv ev -> Eff effs (GrpMatch tv ev)
groupCPatns (MkCol t ds@(LS.Cons (MkCPatn dcon0 _ts _) _)) (MkRowMatch es rs) = do
  let drs = toList (LS.zip ds rs)
  (MkTConDecl _ _params dcons0, _dconDecl) <- findInfo info2dcons dcon0
  dcons1 <- case dcons0 of
    Right (d:ds) -> pure (d :| ds)
    _ -> impossible  -- the type checker guarantees that we pattern match only
                     -- on ADTs and only on those with at least one constructur
  grps <- for dcons1 $ \dcon1 -> do
    let drs1 = filter (\(MkCPatn dcon2 _ts _, _) -> nameOf dcon1 == dcon2) drs
    LS.withList drs1 $ \case
      LS.Nil -> throwHere ("unmatched constructor:" <+> pretty (nameOf dcon1))

      -- NOTE: This is the case where a constructor appears exactly once and no
      -- new binders are introduced.
      LS.Cons (MkCPatn con ts (traverse patnToBPatn -> Just bs), MkRow qs u) LS.Nil -> do
        let bound = Set.fromList (mapMaybe bindName bs)
        let row = MkRow qs (fmap (abstract1 (\x -> guard (x `Set.member` bound) $> x)) u)
        let es1 = LS.map weakenE es
        pure $ MkGrpMatchItem con ts bs (MkRowMatch es1 (LS.Singleton row))

      drs2@(LS.Cons (MkCPatn con ts ps0, _) _) -> do
        -- TODO: This is overly complicated.
        ixs0 <- traverse (const freshEVar) ps0
        LS.withList (toList ixs0) $ \ixs -> do
          grpRows <- for drs2 $ \(MkCPatn _ _ts ps, MkRow qs u) ->
            case LS.match ixs ps of
              Nothing  -> impossible  -- the type checker guarantees the correct arity
              Just ps1 -> pure $ MkRow (ps1 LS.++ qs) (fmap (over _Free weakenScope) u)
          let es1 = LS.map (\x -> EVar (mkBound x x)) ixs LS.++ LS.map weakenE es
          pure (MkGrpMatchItem con ts (fmap BName ixs0) (MkRowMatch es1 grpRows))
  pure $ MkGrpMatch t grps

grpMatchExpr :: CanPM effs => GrpMatch tv ev -> Eff effs (Expr Out tv ev)
grpMatchExpr (MkGrpMatch t is) = EMat t <$> traverse grpMatchItemAltn is

grpMatchItemAltn :: CanPM effs => GrpMatchItem tv ev -> Eff effs (Altn Out tv ev)
grpMatchItemAltn (MkGrpMatchItem con ts bs rm) = do
  MkAltn (PSimple con ts (fmap bindName bs)) <$> pmMatch rm

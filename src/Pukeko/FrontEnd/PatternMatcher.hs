{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
module Pukeko.FrontEnd.PatternMatcher
  ( compileModule
  )
where

import Pukeko.Prelude

import           Control.Monad.Freer.Supply
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.Sized as LS
import qualified Data.Map.Extended as Map

import Pukeko.AST.ConDecl
import Pukeko.AST.Language
import Pukeko.AST.Name
import Pukeko.AST.SystemF
import Pukeko.AST.Type
import Pukeko.FrontEnd.Info

type In  = Typed
type Out = Unnested

type GlobalEffs effs = Members [Reader ModuleInfo, NameSource, Error Failure] effs

type CanPM effs = (Members [Supply Int, Reader SourcePos] effs, GlobalEffs effs)

freshTmVar :: CanPM effs => Eff effs TmVar
freshTmVar = do
  n <- fresh @Int
  mkName (Lctd noPos (Tagged ("pm$" ++ show n)))

pmExpr :: CanPM effs => Expr In -> Eff effs (Expr Out)
pmExpr = \case
  ELoc le         -> here le $ ELoc <$> lctd pmExpr le
  EVar x          -> pure (EVar x)
  EAtm a          -> pure (EAtm a)
  EApp e (TmArg a) -> ETmApp <$> pmExpr e <*> pmExpr a
  EApp e (TyArg t) -> ETyApp <$> pmExpr e <*> pure t
  EApp e (DxArg c) -> EDxApp <$> pmExpr e <*> pure c
  EAbs (TmPar x) e -> ETmAbs x <$> pmExpr e
  EAbs (TyPar v) e -> ETyAbs v <$> pmExpr e
  EAbs (DxPar c) e -> EDxAbs c <$> pmExpr e
  ELet b e -> ELet <$> bind2expr pmExpr b <*> pmExpr e
  EMat t0 e0 as0     -> LS.withNonEmpty as0 $ \as1 -> do
      e1 <- pmExpr e0
      pmMatch (mkRowMatch1 e1 t0 as1)
  ECast coe e0 -> ECast coe <$> pmExpr e0
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
  DInst (MkInstDecl name clss typ prms ctxt super mthds) ->
    DInst . MkInstDecl name clss typ prms ctxt super <$> traverse pmFuncDecl mthds

compileModule :: Members [NameSource, Error Failure] effs =>
  Module In -> Eff effs (Module Out)
compileModule m0 = module2decls (traverse pmDecl) m0 & runInfo m0


pmMatch :: forall m m' n effs. (CanPM effs, m ~ 'LS.Succ m') =>
  RowMatch m n -> Eff effs (Expr Out)
pmMatch rowMatch0 = do
  let colMatch1 = rowToCol rowMatch0
  elimBPatnCols colMatch1 $ \case
    MkColMatch LS.Nil (LS.Cons u2 us2)
      | LS.Nil    <- us2 -> pmExpr u2
      | LS.Cons{} <- us2 -> throwHere "overlapping patterns"
    colMatch2@(MkColMatch LS.Cons{} _) -> do
      (conCol, colMatch3) <- findCPatnCol colMatch2
      let rowMatch4 = colToRow colMatch3
      grpMatch <- groupCPatns conCol rowMatch4
      grpMatchExpr grpMatch

-- | A binding pattern, i.e., not a constructor pattern.
data BPatn
  = BWild
  | BName (TmBinder Type)

bindName :: BPatn -> Maybe (TmBinder Type)
bindName = \case
  BWild   -> Nothing
  BName x -> Just x

patnToBPatn :: Patn In -> Maybe BPatn
patnToBPatn = \case
  PWld   -> Just BWild
  PVar x -> Just (BName x)
  PCon{} -> Nothing

-- | The expression on the RHS of a pattern match alternative.
type RhsExpr = Expr In

-- | A row of a pattern match, i.e., an alternative.
data Row n = MkRow (LS.List n (Patn In)) RhsExpr

-- | A pattern match in it's row-major representation.
data RowMatch m n = MkRowMatch (LS.List n (Expr Out, Type)) (LS.List m (Row n))

-- | Translate an alternative of a pattern match with one column into a row.
mkRow1 :: Altn In -> Row LS.One
mkRow1 (MkAltn p t) = MkRow (LS.Singleton p) t

-- | Tanslate a pattern match with one column into its row-major representation.
mkRowMatch1 :: Expr Out -> Type -> LS.List m (Altn In) -> RowMatch m LS.One
mkRowMatch1 e t as = MkRowMatch (LS.Singleton (e, t)) (LS.map mkRow1 as)

-- | A column of a pattern match.
data Col m a = MkCol (Expr Out, Type) (LS.List m a)

-- | A pattern match in it's column-major representation.
data ColMatch m n = MkColMatch (LS.List n (Col m (Patn In))) (LS.List m RhsExpr)

-- | Traversal of a 'Col'.
colPatn :: Traversal (Col m a) (Col m b) a b
colPatn f (MkCol t ps) = MkCol t <$> traverse f ps

-- | Turn the row-major representation of a pattern match into its column-major
-- representation.
rowToCol :: RowMatch m n -> ColMatch m n
rowToCol (MkRowMatch ts rs) =
  let (pss, us) = LS.unzipWith (\(MkRow ps u) -> (ps, u)) rs
      cs = LS.zipWith MkCol ts (LS.transpose ts pss)
  in  MkColMatch cs us

-- | Turn the column-major representation of a pattern match into its row-major
-- representation.
colToRow :: ColMatch m n -> RowMatch m n
colToRow (MkColMatch cs us) =
  let (ts, pss) = LS.unzipWith (\(MkCol t ps) -> (t, ps)) cs
      rs = LS.zipWith MkRow (LS.transpose us pss) us
  in  MkRowMatch ts rs

-- | Eliminate all columns which contain only binding patterns by "inlining"
-- them into the RHS. This works under the assumption that all scrutinees
-- consist of just a single bound variable. For instance, let @p@, @q@ be bound
-- variables, @x@, @y@ binders and @e@, @f@, @g@ arbitrary expressions. Then,
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
  ColMatch m n -> (forall n'. ColMatch m n' -> Eff effs a) -> Eff effs a
elimBPatnCols (MkColMatch cs0 us0) k = do
  let (cs1, bcs) = partitionEithers (map isBPatnCol (toList cs0))
  us1 <- foldlM applyBPatnCol us0 bcs
  LS.withList cs1 $ \cs2 -> k (MkColMatch cs2 us1)
 where
    isBPatnCol :: Col n (Patn In) -> Either (Col n (Patn In)) (Col n BPatn)
    isBPatnCol (MkCol et ps) =
      case traverse patnToBPatn ps of
        Just bs -> Right (MkCol et bs)
        Nothing -> Left  (MkCol et ps)
    applyBPatnCol :: CanPM effs =>
      LS.List m RhsExpr -> Col m BPatn -> Eff effs (LS.List m RhsExpr)
    applyBPatnCol rhss (MkCol (e, _) bs) = case e of
      EVar x ->
        let replacePatnWithX rhs = \case
              BWild        -> rhs
              BName (y, _) ->
                let replaceYwithX z
                      | z == y    = x
                      | otherwise = z
                in  over freeTmVar replaceYwithX rhs
        in  pure $ LS.zipWith replacePatnWithX rhss bs
      _ -> throwHere "pattern match too simple, use a let binding instead"

-- | A constructor pattern, i.e., neither a wildcard nor a variable pattern.
data CPatn = MkCPatn TmCon [Patn In]

patnToCPatn :: Patn In -> Maybe CPatn
patnToCPatn = \case
  PWld{}    -> Nothing
  PVar{}    -> Nothing
  PCon c ps -> Just (MkCPatn c ps)

-- | Find the first column of a pattern match which consists entirely of
-- constructor patterns.
findCPatnCol :: CanPM effs =>
  ColMatch m ('LS.Succ n) -> Eff effs (Col m CPatn, ColMatch m n)
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
data GrpMatchItem =
  forall m m' n. m ~ 'LS.Succ m' => MkGrpMatchItem TmCon [BPatn] (RowMatch m n)

-- | A grouped pattern match. They result from the transformation in
-- 'groupCPatn'.
data GrpMatch = MkGrpMatch (Expr Out, Type) (NonEmpty GrpMatchItem)

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
groupCPatns :: forall m m' n effs. (CanPM effs, m ~ 'LS.Succ m') =>
  Col m CPatn -> RowMatch m n -> Eff effs GrpMatch
groupCPatns (MkCol (e, t) ds) (MkRowMatch es rs) = do
  let (tcon, targs) = case unwindl _TApp t of
        (TCon c, ts) -> (c, ts)
        _            -> impossible  -- the type checker guarantees this
  MkTyConDecl _ tpars dcons0 <- findInfo info2tycons tcon
  let tsubst = Map.fromList (zipExact tpars targs)
  let drs = toList (LS.zip ds rs)
  let dcons1 = case dcons0 of
        Right (d:ds) -> d :| ds
        _            -> impossible  -- the type checker guarantees this
  grps <- for dcons1 $ \(MkTmConDecl _ dcon1 _ fields0) -> do
    let fields1 = map (>>= (tsubst Map.!)) fields0
    let drs1 = filter (\(MkCPatn dcon2 _, _) -> dcon1 == dcon2) drs
    LS.withList drs1 $ \case
      LS.Nil -> throwHere ("unmatched constructor:" <+> pretty dcon1)

      -- NOTE: This is the case where a constructor appears exactly once and no
      -- new binders are introduced.
      LS.Cons (MkCPatn con (traverse patnToBPatn -> Just bs), row) LS.Nil ->
        pure $ MkGrpMatchItem con bs (MkRowMatch es (LS.Singleton row))

      drs2@LS.Cons{} -> do
        binders0 <- traverse (\t -> (, t) <$> freshTmVar) fields1
        LS.withList binders0 $ \binders -> do
          grpRows <- for drs2 $ \(MkCPatn _ ps, MkRow qs u) ->
            case LS.match binders ps of
              Nothing  -> impossible  -- the type checker guarantees the correct arity
              Just ps1 -> pure $ MkRow (ps1 LS.++ qs) u
          let es1 = LS.map (first EVar) binders LS.++ es
          pure (MkGrpMatchItem dcon1 (fmap BName binders0) (MkRowMatch es1 grpRows))
  pure $ MkGrpMatch (e, t) grps

grpMatchExpr :: CanPM effs => GrpMatch -> Eff effs (Expr Out)
grpMatchExpr (MkGrpMatch (e, t) is) = EMat t  e <$> traverse grpMatchItemAltn is

grpMatchItemAltn :: CanPM effs => GrpMatchItem -> Eff effs (Altn Out)
grpMatchItemAltn (MkGrpMatchItem con bs rm) =
  MkAltn (PSimple con (fmap bindName bs)) <$> pmMatch rm

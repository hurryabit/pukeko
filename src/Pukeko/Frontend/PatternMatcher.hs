{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Pukeko.FrontEnd.PatternMatcher
  ( compileModule
  )
where

import Pukeko.Prelude

-- import           Control.Lens
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.Sized  as LS
import qualified Data.Map         as Map
import qualified Data.Vector.Sized as Vec

import           Pukeko.FrontEnd.Info
import           Pukeko.AST.Type
import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Identifier as Id

type In  = St.Inferencer Type
type Out = St.PatternMatcher

type PM = InfoT (StateT [Id.EVar] (HereT (Except Doc)))

evalPM :: PM a -> Module In -> Either Doc a
evalPM pm m0 = runExcept (runHereT (evalStateT (runInfoT pm m0) []))

freshEVar :: PM Id.EVar
freshEVar = state (\(x:xs) -> (x, xs))

pmExpr :: Expr In tv ev -> PM (Expr Out tv ev)
pmExpr = \case
  ELoc l          -> ELoc <$> traverseHere pmExpr l
  EVar x          -> pure (EVar x)
  EVal z          -> pure (EVal z)
  ECon c          -> pure (ECon c)
  ENum n          -> pure (ENum n)
  EApp t  us      -> EApp <$> pmExpr t <*> traverse pmExpr us
  ELam bs e t     -> ELam bs <$> pmExpr e <*> pure t
  ELet ds t       -> ELet <$> traverseHeres (defn2exprSt pmExpr) ds <*> pmExpr t
  ERec ds t       -> ERec <$> traverseHeres (defn2exprSt pmExpr) ds <*> pmExpr t
  EMat t0 as0     -> LS.withNonEmpty as0 $ \as1 -> do
      t1 <- pmExpr t0
      pmMatch (mkRowMatch1 t1 as1)
  ETyAbs xs e0 -> ETyAbs xs <$> pmExpr e0
  ETyApp e0 t  -> ETyApp <$> pmExpr e0 <*> pure t

pmDefn :: Defn In tv ev -> PM (Defn Out tv ev)
pmDefn (MkDefn b e) = do
    put (Id.freshEVars "pm" (b^.bind2evar))
    MkDefn b <$> pmExpr e

pmDecl :: Decl In -> PM (Decl Out)
pmDecl = \case
  DType ds -> pure (DType ds)
  DClss c -> pure (DClss c)
  DInst i -> DInst <$> inst2defn pmDefn i
  DDefn d -> DDefn <$> pmDefn d
  DPrim p -> pure (DPrim p)

compileModule :: Module In -> Either Doc (Module Out)
compileModule m0 = evalPM (module2decls (traverseHeres pmDecl) m0) m0

pmMatch ::
  forall m m' n tv ev. m ~ 'LS.Succ m' =>
  RowMatch m n tv ev -> PM (Expr Out tv ev)
pmMatch rowMatch0 = do
  let colMatch1 = rowToCol rowMatch0
  elimBPatnCols colMatch1 $ \case
    MkColMatch LS.Nil (LS.Cons u2 us2)
      | LS.Nil    <- us2 -> pmExpr (fmap unsafeStrengthen u2)
      | LS.Cons{} <- us2 -> throwHere "overlapping patterns"
    colMatch2@(MkColMatch LS.Cons{} _) -> do
      (conCol, colMatch3) <- findCPatnCol colMatch2
      let rowMatch4 = colToRow colMatch3
      grpMatch <- groupCPatns conCol rowMatch4
      grpMatchExpr grpMatch

data BPatn
  = BWild
  | BName Id.EVar

bindName :: BPatn -> Maybe Id.EVar
bindName = \case
  BWild   -> Nothing
  BName x -> Just x

patnToBPatn :: Patn st tv -> Maybe BPatn
patnToBPatn = \case
  PWld   -> Just (BWild)
  PVar x -> Just (BName x)
  PCon{} -> Nothing

type RhsExpr tv ev = Expr In tv (EScope Id.EVar ev)

data Row n tv ev = MkRow (LS.List n (Patn Type tv)) (RhsExpr tv ev)

data RowMatch m n tv ev =
  MkRowMatch (LS.List n (Expr Out tv ev)) (LS.List m (Row n tv ev))

mkRow1 :: Altn In tv ev -> Row LS.One tv ev
mkRow1 (MkAltn p t) = MkRow (LS.Singleton p) t

mkRowMatch1 :: Expr Out tv ev -> LS.List m (Altn In tv ev) -> RowMatch m LS.One tv ev
mkRowMatch1 t as = MkRowMatch (LS.Singleton t) (LS.map mkRow1 as)

data Col m tv ev a = MkCol (Expr Out tv ev) (LS.List m a)

data ColMatch m n tv ev =
  MkColMatch (LS.List n (Col m tv ev (Patn Type tv))) (LS.List m (RhsExpr tv ev))

colPatn :: Traversal (Col m tv ev a) (Col m tv ev b) a b
colPatn f (MkCol t ps) = MkCol t <$> traverse f ps

rowToCol :: RowMatch m n tv ev -> ColMatch m n tv ev
rowToCol (MkRowMatch ts rs) =
  let (pss, us) = LS.unzipWith (\(MkRow ps u) -> (ps, u)) rs
      cs = LS.zipWith MkCol ts (LS.transpose ts pss)
  in  MkColMatch cs us

colToRow :: ColMatch m n tv ev -> RowMatch m n tv ev
colToRow (MkColMatch cs us) =
  let (ts, pss) = LS.unzipWith (\(MkCol t ps) -> (t, ps)) cs
      rs = LS.zipWith MkRow (LS.transpose us pss) us
  in  MkRowMatch ts rs

elimBPatnCols :: ColMatch m n tv ev -> (forall n'. ColMatch m n' tv ev -> PM a) -> PM a
elimBPatnCols (MkColMatch cs0 us0) k = do
  let (cs1, bcs) = partitionEithers (map isBPatnCol (toList cs0))
  us1 <- foldlM applyBPatnCol us0 bcs
  LS.withList cs1 $ \cs2 -> k (MkColMatch cs2 us1)
 where
    isBPatnCol ::
      Col n tv ev (Patn Type tv) -> Either (Col n tv ev (Patn Type tv)) (Col n tv ev BPatn)
    isBPatnCol (MkCol t ps) =
      case traverse patnToBPatn ps of
        Just bs -> Right (MkCol t bs)
        Nothing -> Left  (MkCol t ps)
    applyBPatnCol ::
      LS.List m (RhsExpr tv ev) -> Col m tv ev BPatn -> PM (LS.List m (RhsExpr tv ev))
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

data CPatn tv = MkCPatn Id.DCon [Type tv] [Patn Type tv]

patnToCPatn :: forall tv. Patn Type tv -> Maybe (CPatn tv)
patnToCPatn = \case
  PWld{}      -> Nothing
  PVar{}      -> Nothing
  PCon c ts ps -> Just (MkCPatn c ts ps)

findCPatnCol ::
  ColMatch m ('LS.Succ n) tv ev -> PM (Col m tv ev (CPatn tv), ColMatch m n tv ev)
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

data GrpMatchItem tv ev =
  forall m m' n k. m ~ 'LS.Succ m' =>
  MkGrpMatchItem Id.DCon [Type tv] (Vector k BPatn) (RowMatch m n tv (EFinScope k ev))

data GrpMatch tv ev = MkGrpMatch (Expr Out tv ev) (NonEmpty (GrpMatchItem tv ev))

groupCPatns ::
  forall m m' n tv ev. m ~ 'LS.Succ m' =>
  Col m tv ev (CPatn tv) -> RowMatch m n tv ev -> PM (GrpMatch tv ev)
groupCPatns (MkCol t ds@(LS.Cons (MkCPatn dcon0 _ts _) _)) (MkRowMatch es rs) = do
  let drs = toList (LS.zip ds rs)
  Some1 (Pair1 (MkTConDecl tcon _params dcons0) _dconDecl) <- findInfo info2dcons dcon0
  dcons1 <- case dcons0 of
    []   -> bugWith "pattern match on type without data constructors" tcon
    d:ds -> pure (d :| ds)
  grps <- for dcons1 $ \(Loc _ MkDConDecl{_dcon2name = dcon1}) -> do
    let drs1 = filter (\(MkCPatn dcon2 _ts _, _)-> dcon1 == dcon2) drs
    LS.withList drs1 $ \case
      LS.Nil -> throwHere ("unmatched constructor:" <+> pretty dcon1)
      LS.Cons (MkCPatn con ts (traverse patnToBPatn -> Just bs0), MkRow qs u) LS.Nil ->
        Vec.withList bs0 $ \bs -> do
          let mp = ifoldMap (\i -> maybe mempty (\x -> Map.singleton x i) . bindName) bs
          let row = MkRow qs (fmap (abstract1 (`Map.lookup` mp)) u)
          let es1 = LS.map (fmap weaken) es
          pure $ MkGrpMatchItem con ts bs (MkRowMatch es1 (LS.Singleton row))
      drs2@(LS.Cons (MkCPatn con ts ps0, _) _) -> Vec.withList ps0 $ \ps -> do
        ixs0 <- itraverse (\i _ -> (,) i <$> freshEVar) ps
        LS.withList (toList ixs0)$ \ixs -> do
          grpRows <- for drs2 $ \(MkCPatn _ _ts ps, MkRow qs u) ->
            case LS.match ixs ps of
              Nothing  -> bug "wrong number of patterns"
              Just ps1 -> pure $ MkRow (ps1 LS.++ qs) (fmap (fmap weaken) u)
          let es1 = LS.map (EVar . uncurry mkBound) ixs LS.++ LS.map (fmap weaken) es
          pure (MkGrpMatchItem con ts (fmap (BName . snd) ixs0) (MkRowMatch es1 grpRows))
  pure $ MkGrpMatch t grps

grpMatchExpr :: GrpMatch tv ev -> PM (Expr Out tv ev)
grpMatchExpr (MkGrpMatch t is) = ECas t <$> traverse grpMatchItemAltn is

grpMatchItemAltn :: GrpMatchItem tv ev -> PM (Case Out tv ev)
grpMatchItemAltn (MkGrpMatchItem con ts bs0 rm) = do
  bs1 <- for bs0 $ \case
    BWild   -> freshEVar
    BName x -> pure x
  MkCase con ts bs1 <$> pmMatch rm

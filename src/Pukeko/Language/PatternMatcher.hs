{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
module Pukeko.Language.PatternMatcher
  ( compileModule
  )
where

import           Control.Lens
import           Control.Monad.State
import           Data.Bifunctor   (second)
import           Data.Either      (partitionEithers)
import           Data.Foldable    (foldlM, toList)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.Sized  as LS
import qualified Data.Map         as Map
import           Data.Traversable (for)
import qualified Data.Vector.Sized as Vec

import           Pukeko.Error
import           Pukeko.Language.Info
import           Pukeko.Language.Type
import           Pukeko.Language.AST.Std            hiding (Bind (..))
import qualified Pukeko.Language.AST.Stage          as St
import qualified Pukeko.Language.AST.ConDecl        as Con
import qualified Pukeko.Language.Ident              as Id

type In  = St.TypeEraser
type Out = St.PatternMatcher

newtype PM a = PM{unPM :: InfoT (ModuleInfo In) (StateT [Id.EVar] (Except String)) a}
  deriving ( Functor, Applicative, Monad
           , MonadInfo (GenModuleInfo 'True 'True)
           , MonadState [Id.EVar]
           , MonadError String
           )

evalPM :: MonadError String m => PM a -> ModuleInfo In -> m a
evalPM pm decls = runExcept $ evalStateT (runInfoT (unPM pm) decls) []

freshEVar :: PM Id.EVar
freshEVar = state (\(x:xs) -> (x, xs))

pmExpr :: Expr In tv ev -> PM (Expr Out tv ev)
pmExpr = \case
  EVar w x          -> pure (EVar w x)
  EVal w z          -> pure (EVal w z)
  ECon w c          -> pure (ECon w c)
  ENum w n          -> pure (ENum w n)
  EApp w t  us      -> EApp w <$> pmExpr t <*> traverse pmExpr us
  ELam w bs t       -> ELam w (fmap retagBind bs) <$> pmExpr t
  ELet w ds t       -> ELet w <$> (traverse . defn2rhs) pmExpr ds <*> pmExpr t
  ERec w ds t       -> ERec w <$> (traverse . defn2rhs) pmExpr ds <*> pmExpr t
  EMat w t0 as0     -> LS.withNonEmpty as0 $ \as1 -> do
      t1 <- pmExpr t0
      pmMatch w (mkRowMatch1 t1 as1)
  -- ETyAbs w xs e0 -> ETyAbs w xs <$> pmExpr e0
  -- ETyApp w e0 t  -> ETyApp w <$> pmExpr e0 <*> pure t

pmTopLevel :: TopLevel In -> PM (TopLevel Out)
pmTopLevel = \case
  TLDef (MkDefn b e) -> do
    put (Id.freshEVars "pm" (b^.bindEVar))
    TLDef <$> MkDefn (retagBind b) <$> pmExpr e
  TLAsm b a -> pure (TLAsm (retagBind b) a)

compileModule :: MonadError String m => Module In -> m (Module Out)
compileModule (MkModule decls tops) =
  MkModule decls <$> evalPM (traverse pmTopLevel tops) decls

pmMatch ::
  forall m m' n tv ev. m ~ 'LS.Succ m' =>
  Pos -> RowMatch m n tv ev -> PM (Expr Out tv ev)
pmMatch w rowMatch0 = do
  let colMatch1 = rowToCol rowMatch0
  elimBindCols w colMatch1 $ \case
    MkColMatch LS.Nil (LS.Cons u2 us2)
      | LS.Nil    <- us2 -> pmExpr (fmap strengthen u2)
      | LS.Cons{} <- us2 -> throwErrorAt w "overlapping patterns"
    colMatch2@(MkColMatch LS.Cons{} _) -> do
      (destCol, colMatch3) <- findDestCol w colMatch2
      let rowMatch4 = colToRow colMatch3
      grpMatch <- groupDests w destCol rowMatch4
      grpMatchExpr w grpMatch

-- TODO: Do this in a more principled way.
data Bind
  = BWild Pos
  | BName Pos Id.EVar

bindName :: Bind -> Maybe Id.EVar
bindName = \case
  BWild _   -> Nothing
  BName _ x -> Just x

patnToBind :: Patn st tv -> Maybe Bind
patnToBind = \case
  PWld w   -> Just (BWild w)
  PVar w x -> Just (BName w x)
  PCon{}   -> Nothing

type RhsExpr tv ev = Expr In tv (EScope Id.EVar ev)

data Row n tv ev = MkRow (LS.List n (Patn In tv)) (RhsExpr tv ev)

data RowMatch m n tv ev =
  MkRowMatch (LS.List n (Expr Out tv ev)) (LS.List m (Row n tv ev))

mkRow1 :: Altn In tv ev -> Row LS.One tv ev
mkRow1 (MkAltn _ p t) = MkRow (LS.Singleton p) t

mkRowMatch1 :: Expr Out tv ev -> LS.List m (Altn In tv ev) -> RowMatch m LS.One tv ev
mkRowMatch1 t as = MkRowMatch (LS.Singleton t) (LS.map mkRow1 as)

data Col m tv ev a = MkCol (Expr Out tv ev) (LS.List m a)

data ColMatch m n tv ev =
  MkColMatch (LS.List n (Col m tv ev (Patn In tv))) (LS.List m (RhsExpr tv ev))

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

elimBindCols ::
  Pos -> ColMatch m n tv ev -> (forall n'. ColMatch m n' tv ev -> PM a) -> PM a
elimBindCols w (MkColMatch cs0 us0) k = do
  let (cs1, bcs) = partitionEithers (map isBindCol (toList cs0))
  us1 <- foldlM (applyBindCol w) us0 bcs
  LS.withList cs1 $ \cs2 -> k (MkColMatch cs2 us1)
  where
    isBindCol ::
      Col n tv ev (Patn In tv) -> Either (Col n tv ev (Patn In tv)) (Col n tv ev Bind)
    isBindCol (MkCol t ps) =
      case traverse patnToBind ps of
        Just bs -> Right (MkCol t bs)
        Nothing -> Left  (MkCol t ps)
    applyBindCol ::
      Pos -> LS.List m (RhsExpr tv ev) -> Col m tv ev Bind -> PM (LS.List m (RhsExpr tv ev))
    applyBindCol w rhss (MkCol t bs) = case t of
      EVar _ x ->
        let replacePatnWithX rhs = \case
              BWild _   -> rhs
              BName _ y ->
                let replaceYwithX = \case
                      Bound z _ | y == z -> Free x
                      b                  -> b
                in  fmap replaceYwithX rhs
        in  pure $ LS.zipWith replacePatnWithX rhss bs
      _ -> throwErrorAt w "pattern match too simple, use a let binding instead"

data Dest tv = MkDest Id.DCon [Patn In tv]

patnToDest :: forall tv. Patn In tv -> Maybe (Dest tv)
patnToDest = \case
  PWld{}      -> Nothing
  PVar{}      -> Nothing
  PCon _ c (_ :: [NoType tv]) ps -> Just (MkDest c ps)

findDestCol ::
  Pos -> ColMatch m ('LS.Succ n) tv ev -> PM (Col m tv ev (Dest tv), ColMatch m n tv ev)
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

data GrpMatchItem tv ev =
  forall m m' n k. m ~ 'LS.Succ m' =>
  MkGrpMatchItem Id.DCon (Vec.Vector k Bind) (RowMatch m n tv (EFinScope k ev))

data GrpMatch tv ev = MkGrpMatch (Expr Out tv ev) (NonEmpty (GrpMatchItem tv ev))

groupDests ::
  forall m m' n tv ev. m ~ 'LS.Succ m' =>
  Pos -> Col m tv ev (Dest tv) -> RowMatch m n tv ev -> PM (GrpMatch tv ev)
groupDests w (MkCol t ds@(LS.Cons (MkDest dcon0 _) _)) (MkRowMatch ts rs) = do
  let drs = toList (LS.zip ds rs)
  Con.MkDConDecl Con.MkDConDeclN{_tcon = tcon} <- findDCon dcon0
  Con.MkTConDecl{_dcons = dcons0} <- findTCon tcon
  dcons1 <- case dcons0 of
    []   -> throwAt w "pattern match on type without data constructors" tcon
    d:ds -> pure (d :| ds)
  grps <- for dcons1 $ \Con.MkDConDeclN{_dname = dcon1} -> do
    let drs1 = filter (\(MkDest dcon2 _, _)-> dcon1 == dcon2) drs
    LS.withList drs1 $ \case
      LS.Nil -> throwAt w "unmatched constructor" dcon1
      LS.Cons (MkDest con (traverse patnToBind -> Just bs0), MkRow qs u) LS.Nil ->
        Vec.withList bs0 $ \bs -> do
          let mp = ifoldMap (\i -> maybe mempty (\x -> Map.singleton x i) . bindName) bs
          let row = MkRow qs (fmap (abstract1 (`Map.lookup` mp)) u)
          let ts1 = LS.map (fmap weaken) ts
          pure $ MkGrpMatchItem con bs (MkRowMatch ts1 (LS.Singleton row))
      drs2@(LS.Cons (MkDest con ps0, _) _) -> Vec.withList ps0 $ \ps -> do
        ixs0 <- itraverse (\i _ -> (,) i <$> freshEVar) ps
        LS.withList (toList ixs0)$ \ixs -> do
          grpRows <- for drs2 $ \(MkDest _ ps, MkRow qs u) ->
            case LS.match ixs ps of
              Nothing  -> bug "wrong number of patterns"
              Just ps1 -> pure $ MkRow (ps1 LS.++ qs) (fmap (fmap weaken) u)
          let ts1 = LS.map (EVar w . uncurry mkBound) ixs LS.++ LS.map (fmap weaken) ts
          pure $ MkGrpMatchItem con (fmap (BName w . snd) ixs0) (MkRowMatch ts1 grpRows)
  pure $ MkGrpMatch t grps

grpMatchExpr :: Pos -> GrpMatch tv ev -> PM (Expr Out tv ev)
grpMatchExpr w (MkGrpMatch t is) =
  ECas w t <$> traverse (grpMatchItemAltn w) is

grpMatchItemAltn :: Pos -> GrpMatchItem tv ev -> PM (Case Out tv ev)
grpMatchItemAltn w (MkGrpMatchItem con bs rm) =
  MkCase w con [] (fmap bindName bs) <$> pmMatch w rm

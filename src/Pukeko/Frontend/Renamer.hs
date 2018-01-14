-- | Transform AST to use type safe de Bruijn indices.
module Pukeko.FrontEnd.Renamer
  ( renameModule
  ) where

import Pukeko.Prelude

import           Control.Lens
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Data.Vector.Sized as Vec

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import qualified Pukeko.AST.ConDecl    as Con
import qualified Pukeko.AST.Surface    as Ps
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type       (NoType (..), toPrenex)

type Out = St.Renamer

renameModule :: MonadError String m => Ps.Module -> m (Module Out)
renameModule tops = runRn $ MkModule <$> concat <$> traverse rnTopLevel tops

type RnEnv ev = Map Id.EVar ev
type RnState = Set Id.EVar

newtype Rn ev a = Rn{unRn :: ReaderT (RnEnv ev) (StateT RnState (Except String)) a}
  deriving ( Functor, Applicative, Monad
           , MonadReader (RnEnv ev)
           , MonadState  RnState
           , MonadError String
           )

runRn :: MonadError String m => Rn Void a -> m a
runRn ix = runExcept (evalStateT (runReaderT (unRn ix) Map.empty) Set.empty)

localize :: Map Id.EVar i -> Rn (EScope i ev) a -> Rn ev a
localize bs = Rn . withReaderT upd . unRn
  where
    upd env = Map.mapWithKey (flip mkBound) bs `Map.union` Map.map Free env

localizeDefns :: Vector n (Ps.Defn _) -> Rn (EFinScope n ev) a -> Rn ev a
localizeDefns =
  localize . ifoldMap (\i (Ps.MkDefn (Ps.MkBind _ x) _) -> Map.singleton x i)

rnTopLevel :: Ps.TopLevel -> Rn Void [TopLevel Out]
rnTopLevel top = case top of
  Ps.TLTyp w tcs ->
    (:[]) . TLTyp w <$> traverse rnTConDecl tcs `catchError` throwErrorAt w
  Ps.TLVal w x t -> pure [TLVal w x (toPrenex t)]
  Ps.TLLet _ ds0 -> do
    ds1 <- traverse rnDefn ds0
    let xs = map (\(Ps.MkDefn (Ps.MkBind _ x) _) -> x) ds0
    id <>= Set.fromList xs
    pure (map TLDef ds1)
  Ps.TLRec _ ds0 -> do
    let xs = map (\(Ps.MkDefn (Ps.MkBind _ x) _) -> x) ds0
    id <>= Set.fromList xs
    ds1 <- traverse rnDefn ds0
    pure (map TLDef ds1)
  Ps.TLAsm w x a -> do
    id <>= Set.singleton x
    pure [TLAsm (MkBind w x NoType) a]


rnTConDecl :: Ps.TConDecl -> Rn ev (Some1 Con.TConDecl)
rnTConDecl (Ps.MkTConDecl tcon ps0 dcs0) = Vec.withList ps0 $ \ps1 -> do
  let env = ifoldMap (flip Map.singleton) ps1
  Some1 <$> Con.MkTConDecl tcon ps1 <$> zipWithM (rnDConDecl tcon env) [0..] dcs0

rnDConDecl ::
  forall n ev.
  Id.TCon -> Map Id.TVar (Finite n) -> Int -> Ps.DConDecl -> Rn ev (Con.DConDecl n)
rnDConDecl tcon env tag (Ps.MkDConDecl dcon ts) =
  Con.MkDConDecl tcon dcon tag <$> (traverse . traverse) rnTVar ts
  where
    rnTVar :: Id.TVar -> Rn ev (TFinScope n Void)
    rnTVar x = case x `Map.lookup` env of
      Just i  -> pure (mkBound i x)
      Nothing -> throw "unknown type variable" x

rnDefn :: Ps.Defn Id.EVar -> Rn ev (Defn Out Void ev)
rnDefn (Ps.MkDefn b e) = MkDefn (rnBind b) <$> rnExpr e

rnExpr :: Ps.Expr Id.EVar -> Rn ev (Expr Out Void ev)
rnExpr = \case
  Ps.EVar w x -> do
    y_mb <- view (at x)
    case y_mb of
      Just y -> pure (EVar w y)
      Nothing -> do
        global <- use (contains x)
        if global then pure (EVal w x) else throwAt w "unknown variable" x
  Ps.ECon w c -> pure (ECon w c)
  Ps.ENum w n -> pure (ENum w n)
  Ps.EApp w e0  es -> EApp w <$> rnExpr e0 <*> traverse rnExpr es
  Ps.EMat w e0  as0 ->
    case as0 of
      []   -> throwErrorAt w "pattern match without alternatives"
      a:as -> EMat w <$> rnExpr e0 <*> traverse rnAltn (a :| as)
  Ps.ELam w bs0 e0 -> Vec.withList (map rnBind bs0) $ \bs1 -> do
    let bs2 = ifoldMap (\i (MkBind _ x NoType) -> Map.singleton x i) bs1
    ELam w bs1 <$> localize bs2 (rnExpr e0) <*> pure NoType
  Ps.ELet w ds0 e0 -> Vec.withList ds0 $ \ds1 -> do
    ELet w <$> traverse rnDefn ds1 <*> localizeDefns ds1 (rnExpr e0)
  Ps.ERec w ds0 e0 -> Vec.withList ds0 $ \ds1 -> do
    localizeDefns ds1 $ ERec w <$> traverse rnDefn ds1 <*> rnExpr e0

rnBind :: Ps.Bind -> Bind Out Void
rnBind (Ps.MkBind w x) = MkBind w x NoType

rnAltn :: Ps.Altn Id.EVar -> Rn ev (Altn Out Void ev)
rnAltn (Ps.MkAltn w p0 e) = do
  let p1 = rnPatn p0
  let bs = Map.fromSet id (setOf patn2evar p1)
  MkAltn w p1 <$> localize bs (rnExpr e)

rnPatn :: Ps.Patn -> Patn Out Void
rnPatn = \case
  Ps.PWld w      -> PWld w
  Ps.PVar w x    -> PVar w x
  Ps.PCon w c ps -> PCon w c [] (map rnPatn ps)

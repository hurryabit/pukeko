-- | Transform AST to use type safe de Bruijn indices.
module Pukeko.Language.Renamer
  ( renameModule
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Finite       (Finite)
import qualified Data.Map          as Map
import qualified Data.Set.Lens     as Set
import qualified Data.Vector.Sized as Vec

import           Pukeko.Error
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.AST.Stage      as St
import qualified Pukeko.Language.AST.ConDecl    as Con
import qualified Pukeko.Language.AST.ModuleInfo as MI
import qualified Pukeko.Language.Parser.AST     as Ps
import qualified Pukeko.Language.Ident          as Id
import           Pukeko.Language.Type           (NoType (..), toPrenex)

type Out = St.Renamer

renameModule :: MonadError String m => Ps.Module -> m (Module Out)
renameModule tops = runRn $ MkModule info <$> traverse rnTopLevel tops
  where
    info = MI.MkModuleInfo MI.Absent MI.Absent MI.Absent

data Env ev = MkEnv (Map.Map Id.EVar ev) (Id.EVar -> ev)

newtype Rn ev a = Rn{unRn :: ReaderT (Env ev) (Except String) a}
  deriving ( Functor, Applicative, Monad
           , MonadReader (Env ev)
           , MonadError String
           )

runRn :: MonadError String m => Rn Id.EVar a -> m a
runRn ix = runExcept (runReaderT (unRn ix) (MkEnv mempty id))

localize :: Map.Map Id.EVar i -> Rn (EScope i ev) a -> Rn ev a
localize bs = Rn . withReaderT upd . unRn
  where
    upd (MkEnv bound0 mkFree) =
      let bound1 = Map.mapWithKey (flip mkBound) bs `Map.union` Map.map Free bound0
      in  MkEnv bound1 (Free . mkFree)

localizeDefns :: Vec.Vector n (Ps.Defn _) -> Rn (EFinScope n ev) a -> Rn ev a
localizeDefns =
  localize . ifoldMap (\i (Ps.MkDefn (Ps.MkBind _ x) _) -> Map.singleton x i)

rnTopLevel :: Ps.TopLevel -> Rn Id.EVar (TopLevel Out)
rnTopLevel top = case top of
  Ps.TLTyp w tcs -> TLTyp w <$> traverse rnTConDecl tcs `catchError` throwErrorAt w
  Ps.TLVal w x t -> pure (TLVal w x (toPrenex t))
  Ps.TLLet w ds0 -> Vec.withList ds0 $ \ds1 ->
    TLLet w <$> traverse rnDefn ds1
  Ps.TLRec w ds0 -> Vec.withList ds0 $ \ds1 ->
    localizeDefns ds1 $ TLRec w <$> traverse rnDefn ds1
  Ps.TLAsm w x a -> pure (TLAsm (MkBind w x NoType) a)

rnTConDecl :: Ps.TConDecl -> Rn ev Con.TConDecl
rnTConDecl (Ps.MkTConDecl tcon ps0 dcs0) = Vec.withList ps0 $ \ps1 -> do
  let env = ifoldMap (flip Map.singleton) ps1
  Con.MkTConDecl tcon ps1 <$> zipWithM (rnDConDecl tcon env) [0..] dcs0

rnDConDecl ::
  forall n ev.
  Id.TCon -> Map.Map Id.TVar (Finite n) -> Int -> Ps.DConDecl -> Rn ev (Con.DConDeclN n)
rnDConDecl tcon env tag (Ps.MkDConDecl dcon ts) =
  Con.MkDConDeclN tcon dcon tag <$> (traverse . traverse) rnTVar ts
  where
    rnTVar :: Id.TVar -> Rn ev (TFinScope n Void)
    rnTVar x = case x `Map.lookup` env of
      Just i  -> pure (mkBound i x)
      Nothing -> throw "unknown type variable" x

rnDefn :: Ps.Defn Id.EVar -> Rn ev (Defn Out Void ev)
rnDefn (Ps.MkDefn b e) = MkDefn (rnBind b) <$> rnExpr e

rnExpr :: Ps.Expr Id.EVar -> Rn ev (Expr Out Void ev)
rnExpr = \case
  Ps.EVar w x ->
    asks $ \(MkEnv bound mkFree) -> EVar w (Map.findWithDefault (mkFree x) x bound)
  Ps.ECon w c -> pure (ECon w c)
  Ps.ENum w n -> pure (ENum w n)
  Ps.EApp w e0  es -> EApp w <$> rnExpr e0 <*> traverse rnExpr es
  Ps.EMat w e0  as -> EMat w <$> rnExpr e0 <*> traverse rnAltn as
  Ps.ELam w bs0 e0 -> Vec.withList (map rnBind bs0) $ \bs1 -> do
    let bs2 = ifoldMap (\i (MkBind _ x NoType) -> Map.singleton x i) bs1
    ELam w bs1 <$> localize bs2 (rnExpr e0)
  Ps.ELet w ds0 e0 -> Vec.withList ds0 $ \ds1 -> do
    ELet w <$> traverse rnDefn ds1 <*> localizeDefns ds1 (rnExpr e0)
  Ps.ERec w ds0 e0 -> Vec.withList ds0 $ \ds1 -> do
    localizeDefns ds1 $ ERec w <$> traverse rnDefn ds1 <*> rnExpr e0

rnBind :: Ps.Bind -> Bind Out Void
rnBind (Ps.MkBind w x) = MkBind w x NoType

rnAltn :: Ps.Altn Id.EVar -> Rn ev (Altn Out Void ev)
rnAltn (Ps.MkAltn w p0 e) = do
  let p1 = rnPatn p0
  let bs = Map.fromSet id (Set.setOf patn2evar p1)
  MkAltn w p1 <$> localize bs (rnExpr e)

rnPatn :: Ps.Patn -> Patn Out Void
rnPatn = \case
  Ps.PWld w      -> PWld w
  Ps.PVar w x    -> PVar w x
  Ps.PCon w c ps -> PCon w c [] (map rnPatn ps)

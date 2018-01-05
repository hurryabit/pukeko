{-# LANGUAGE TupleSections #-}
-- | Transform AST to use type safe de Bruijn indices.
module Pukeko.Language.Renamer
  ( renameModule
  ) where

import           Control.Lens
import           Control.Monad.Reader
import qualified Data.Map          as Map
import qualified Data.Set.Lens     as Set
import qualified Data.Vector.Sized as Vec

import           Pukeko.Language.AST.Classes
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.AST.Stage      as St
import qualified Pukeko.Language.AST.ModuleInfo as MI
import qualified Pukeko.Language.Parser.AST     as Ps
import qualified Pukeko.Language.Ident          as Id

type Out = St.Renamer

renameModule :: Ps.Module -> Module Out
renameModule = MkModule info . runRn . traverse rnTopLevel
  where
    info = MI.MkModuleInfo MI.Absent MI.Absent MI.Absent

data Env tv = MkEnv (Map.Map Id.EVar tv) (Id.EVar -> tv)

newtype Rn tv a = Rn{unRn :: Reader (Env tv) a}
  deriving ( Functor, Applicative, Monad
           , MonadReader (Env tv)
           )

runRn :: Rn Id.EVar a -> a
runRn ix = runReader (unRn ix) (MkEnv mempty id)

localize :: Map.Map Id.EVar i -> Rn (Scope i tv) a -> Rn tv a
localize bs = Rn . withReader upd . unRn
  where
    upd (MkEnv bound0 mkFree) =
      let bound1 = Map.mapWithKey (flip mkBound) bs `Map.union` Map.map Free bound0
      in  MkEnv bound1 (Free . mkFree)

localizeDefns :: Vec.Vector n (GenDefn _ _) -> Rn (FinScope n tv) a -> Rn tv a
localizeDefns = localize . ifoldMap (\i d -> Map.singleton (d^.lhs) i)

rnTopLevel :: Ps.TopLevel -> Rn Id.EVar (TopLevel Out)
rnTopLevel top = case top of
  Ps.TLTyp w ts  -> pure (TLTyp w ts)
  Ps.TLVal w x t -> pure (TLVal w x t)
  Ps.TLLet w ds0 -> Vec.withList ds0 $ \ds1 ->
    TLLet w <$> traverse rnDefn ds1
  Ps.TLRec w ds0 -> Vec.withList ds0 $ \ds1 ->
    localizeDefns ds1 $ TLRec w <$> traverse rnDefn ds1
  Ps.TLAsm w x a -> pure (TLAsm w x a)

rnDefn :: Ps.Defn Id.EVar -> Rn tv (Defn Out tv)
rnDefn = rhs2 rnExpr

rnExpr :: Ps.Expr Id.EVar -> Rn tv (Expr Out tv)
rnExpr = \case
  Ps.EVar w x ->
    asks $ \(MkEnv bound mkFree) -> EVar w (Map.findWithDefault (mkFree x) x bound)
  Ps.ECon w c -> pure (ECon w c)
  Ps.ENum w n -> pure (ENum w n)
  Ps.EApp w e0  es -> EApp w <$> rnExpr e0 <*> traverse rnExpr es
  Ps.EMat w e0  as -> EMat w <$> rnExpr e0 <*> traverse rnAltn as
  Ps.ELam w bs0 e0 -> Vec.withList bs0 $ \bs1 -> do
    let bs2 = ifoldMapOf (itraversed . _BName) (\i (_w, x) -> Map.singleton x i) bs1
    ELam w bs1 <$> localize bs2 (rnExpr e0)
  Ps.ELet w ds0 e0 -> Vec.withList ds0 $ \ds1 -> do
    ELet w <$> traverse rnDefn ds1 <*> localizeDefns ds1 (rnExpr e0)
  Ps.ERec w ds0 e0 -> Vec.withList ds0 $ \ds1 -> do
    localizeDefns ds1 $ ERec w <$> traverse rnDefn ds1 <*> rnExpr e0

rnAltn :: Ps.Altn Id.EVar -> Rn tv (Altn Out tv)
rnAltn (Ps.MkAltn w p e) = do
  let bs = Map.fromSet id (Set.setOf (patn2bind . bind2evar) p)
  MkAltn w p <$> localize bs (rnExpr e)

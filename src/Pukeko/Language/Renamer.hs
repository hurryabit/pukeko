{-# LANGUAGE TupleSections #-}
-- | Transform AST to use type safe de Bruijn indices.
module Pukeko.Language.Renamer
  ( Rn.Module
  , renameModule
  )
  where

import           Control.Lens
import           Control.Monad.Reader
import qualified Data.Map          as Map
import qualified Data.Set.Lens     as Set
import qualified Data.Vector.Sized as Vec

import           Pukeko.Language.AST.Classes
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.Parser.AST     as Ps
import qualified Pukeko.Language.Renamer.AST    as Rn
import qualified Pukeko.Language.Ident          as Id

renameModule :: Ps.Module -> Rn.Module
renameModule = runRn . traverse rnTopLevel

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

rnTopLevel :: Ps.TopLevel -> Rn Id.EVar Rn.TopLevel
rnTopLevel top = case top of
  Ps.TypDef w ts  -> pure (Rn.TypDef w ts)
  Ps.Val    w x t -> pure (Rn.Val    w x t)
  Ps.TopLet w ds0 -> Vec.withList ds0 $ \ds1 ->
    Rn.TopLet w <$> traverse rnDefn ds1
  Ps.TopRec w ds0 -> Vec.withList ds0 $ \ds1 ->
    localizeDefns ds1 $ Rn.TopRec w <$> traverse rnDefn ds1
  Ps.Asm    w x a -> pure (Rn.Asm    w x a)

rnDefn :: Ps.Defn Id.EVar -> Rn tv (Rn.Defn tv)
rnDefn = rhs2 rnExpr

rnExpr :: Ps.Expr Id.EVar -> Rn tv (Rn.Expr tv)
rnExpr = \case
  Ps.Var w x ->
    asks $ \(MkEnv bound mkFree) -> Var w (Map.findWithDefault (mkFree x) x bound)
  Ps.Con w c -> pure (Con w c)
  Ps.Num w n -> pure (Num w n)
  Ps.App w e0  es -> App w <$> rnExpr e0 <*> traverse rnExpr es
  Ps.Mat w e0  as -> Mat w <$> rnExpr e0 <*> traverse rnAltn as
  Ps.Lam w bs0 e0 -> Vec.withList bs0 $ \bs1 -> do
    let bs2 = ifoldMapOf (itraversed . _Name) (\i (_w, x) -> Map.singleton x i) bs1
    Lam w bs1 <$> localize bs2 (rnExpr e0)
  Ps.Let w ds0 e0 -> Vec.withList ds0 $ \ds1 -> do
    Let w <$> traverse rnDefn ds1 <*> localizeDefns ds1 (rnExpr e0)
  Ps.Rec w ds0 e0 -> Vec.withList ds0 $ \ds1 -> do
    localizeDefns ds1 $ Rec w <$> traverse rnDefn ds1 <*> rnExpr e0

rnAltn :: Ps.Altn Id.EVar -> Rn tv (Rn.Altn tv)
rnAltn (Ps.MkAltn w p e) = do
  let bs = Map.fromSet id (Set.setOf (patn2bind . bind2evar) p)
  MkAltn w p <$> localize bs (rnExpr e)

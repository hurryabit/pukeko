{-# LANGUAGE TypeOperators #-}
module Pukeko.Language.LambdaLifter
  ( Out
  , liftModule
  )
where

import           Control.Lens
import           Control.Monad.RWS
import           Data.Either       (partitionEithers)
import qualified Data.Finite       as Fin
import           Data.Foldable     (traverse_)
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Data.Set.Lens     as Set
import qualified Data.Vector.Sized as Vec

import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.AST.Stage   as St
import qualified Pukeko.Language.Ident       as Id
import           Pukeko.Language.Type        (NoType (..))

type In  = St.TypeEraser
type Out = St.LambdaLifter

type State = [Id.EVar]

-- TODO: Use @SupplyT@.
newtype LL a = LL{unLL :: RWS () [TopLevel Out] State a}
  deriving ( Functor, Applicative, Monad
           , MonadWriter [TopLevel Out]
           , MonadState State
           )

execLL :: LL () -> [TopLevel Out]
execLL ll =
  let state = []
      ((), defns) = evalRWS (unLL ll) () state
  in  defns

freshIdent :: LL Id.EVar
freshIdent = state $ \(ident:idents) -> (ident, idents)

llExpr :: (BaseEVar v, Ord v) => Expr In Void v -> LL (Expr Out Void v)
llExpr = \case
  EVar w x -> pure (EVar w x)
  EVal w z -> pure (EVal w z)
  ECon w c -> pure (ECon w c)
  ENum w n -> pure (ENum w n)
  EApp w t  us -> EApp w <$> llExpr t <*> traverse llExpr us
  ECas w t  cs -> ECas w <$> llExpr t <*> traverse llCase cs
  ELet w ds t -> ELet w <$> (traverse . defn2rhs) llExpr ds <*> llExpr t
  ERec w ds t -> ERec w <$> (traverse . defn2rhs) llExpr ds <*> llExpr t
  ELam w oldBinds rhs -> do
    lhs <- freshIdent
    rhs <- llExpr rhs
    let unscope = scope' (\i b -> Right (i, b)) Left
    let (capturedL, others) =
          partitionEithers (map unscope (Set.toList (Set.setOf traverse rhs)))
    Vec.withList capturedL $ \(capturedV :: Vec.Vector m v) -> do
      let newBinds = fmap (\x -> MkBind w (baseEVar x) NoType) capturedV Vec.++ oldBinds
      let renameOther (i, b) = (mkBound i b, mkBound (Fin.shift i) b)
      let renameCaptured i v =
            Map.singleton (Free v) (mkBound (Fin.weaken i) (baseEVar v))
      let rename =
            Map.fromList (map renameOther others) <> ifoldMap renameCaptured capturedV
      tell [TLSup (MkBind w lhs NoType) (fmap retagBind newBinds) (fmap (rename Map.!) rhs)]
      pure (mkEApp w (EVal w lhs) (map (EVar w) capturedL))

llCase :: (BaseEVar v, Ord v) => Case In Void v -> LL (Case Out Void v)
llCase (MkCase w c ts bs e) = MkCase w c ts bs <$> llExpr e

llTopLevel :: TopLevel In -> LL ()
llTopLevel = \case
  TLDef (MkDefn (MkBind w lhs _) rhs) -> do
    put $ Id.freshEVars "ll" lhs
    case rhs of
      ELam{} -> do
        -- NOTE: Make sure this lambda gets lifted to the name it is defining.
        modify (lhs:)
        void $ llExpr rhs
      _ -> do
        rhs <- llExpr rhs
        tell [TLSup (MkBind w lhs NoType) Vec.empty (fmap absurd rhs)]
  TLAsm b asm -> tell [TLAsm (retagBind b) asm]

liftModule :: Module In -> Module Out
liftModule = over module2tops (execLL . traverse_ llTopLevel)

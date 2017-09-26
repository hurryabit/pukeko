module Pukeko.Language.LambdaLifter
  ( Module
  , liftModule
  )
where

import           Control.Lens
import           Control.Lens.Extras (is)
import           Control.Monad.RWS
import qualified Data.Finite       as Fin
import           Data.Foldable     (traverse_)
import           Data.List         (sortOn)
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Data.Set.Lens     as Set
import qualified Data.Vector.Sized as Vec
-- import           GHC.TypeLits

import           Pukeko.Language.Base.AST
import           Pukeko.Language.LambdaLifter.AST
import qualified Pukeko.Language.DeadCode.AST     as In
import qualified Pukeko.Language.Ident            as Id

type State = [Id.EVar]

newtype LL a = LL{unLL :: RWS () [TopLevel] State a}
  deriving ( Functor, Applicative, Monad
           , MonadWriter [TopLevel]
           , MonadState State
           )

execLL :: LL () -> Module
execLL ll =
  let state = []
      ((), defns) = evalRWS (unLL ll) () state
  in  defns

freshIdent :: LL Id.EVar
freshIdent = state $ \(ident:idents) -> (ident, idents)

llExpr :: forall v. (IsVar v) => In.Expr v -> LL (Expr v)
llExpr = \case
  In.Var w x -> pure $ Var w x
  In.Con w c -> pure $ Con w c
  In.Num w n -> pure $ Num w n
  In.App w t  us -> App w <$> llExpr t <*> traverse llExpr us
  -- In.If  w t  u  v -> If w <$> llExpr t <*> llExpr u <*> llExpr v
  In.Mat w t  as -> Mat w <$> llExpr t <*> traverse (altnRhs llExpr) as
  In.Let w ds t -> Let w <$> (traverse . rhs2) llExpr ds <*> llExpr t
  In.Rec w ds t -> Rec w <$> (traverse . rhs2) llExpr ds <*> llExpr t
  In.Lam w oldBinds rhs -> do
    lhs <- freshIdent
    rhs <- llExpr rhs
    let isCaptured v = is _Free v && not (isTotallyFree v)
    let (capturedS, others) = Set.partition isCaptured $ Set.setOf traverse rhs
    -- TODO: Arrange 'captured' in a clever way. The @sortOn varName@ is just to
    -- not break the tests right now.
    let capturedL = sortOn varName $ Set.toList capturedS
    Vec.withList capturedL $ \capturedV -> do
      let newBinds = fmap (Name w . varName) capturedV Vec.++ oldBinds
      let renameOther = \case
            Bound i x -> Bound (Fin.shift i) x
            Free  v   -> Free  (varName v)
      let renameCaptured i v = Map.singleton v (bound (Fin.weaken i) (varName v))
      let rename = Map.fromSet renameOther others <> ifoldMap renameCaptured capturedV
      tell [Def w lhs newBinds (fmap (rename Map.!) rhs)]
      let unfree = \case
            Bound{} -> undefined -- NOTE: Everyhing in @capturedL@ starts with 'Free'.
            Free v  -> v
      let fun = Var w (mkTotallyFree lhs)
      case capturedL of
        []  -> return fun
        _:_ -> return $ App w fun (map (Var w . unfree) capturedL)

llTopLevel :: In.TopLevel -> LL ()
llTopLevel = \case
  In.Def w lhs rhs -> do
    put $ Id.freshEVars "ll" lhs
    case rhs of
      In.Lam{} -> do
        -- NOTE: Make sure this lambda gets lifted to the name it is defining.
        modify (lhs:)
        void $ llExpr rhs
      _ -> do
        rhs <- llExpr rhs
        tell [Caf w lhs rhs]
  In.Asm w lhs asm -> tell [Asm w lhs asm]

liftModule :: In.Module -> Module
liftModule = execLL . traverse_ llTopLevel

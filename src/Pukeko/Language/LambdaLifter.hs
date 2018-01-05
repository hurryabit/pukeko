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

import           Pukeko.Language.AST.Classes
import           Pukeko.Language.AST.Std     hiding (Module)
import qualified Pukeko.Language.AST.Std     as Std
import qualified Pukeko.Language.AST.Stage   as St
import qualified Pukeko.Language.Ident       as Id

type In  = St.DeadCode
type Out = St.LambdaLifter

type Module = Std.Module Out

type State = [Id.EVar]

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

llExpr :: forall v. (IsVar v) => Expr In v -> LL (Expr Out v)
llExpr = \case
  Var w x -> pure $ Var w x
  Con w c -> pure $ Con w c
  Num w n -> pure $ Num w n
  App w t  us -> App w <$> llExpr t <*> traverse llExpr us
  Cas w t  cs -> Cas w <$> llExpr t <*> traverse (case2rhs llExpr) cs
  Let w ds t -> Let w <$> (traverse . rhs2) llExpr ds <*> llExpr t
  Rec w ds t -> Rec w <$> (traverse . rhs2) llExpr ds <*> llExpr t
  Lam w oldBinds rhs -> do
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
      let renameCaptured i v = Map.singleton v (mkBound (Fin.weaken i) (varName v))
      let rename = Map.fromSet renameOther others <> ifoldMap renameCaptured capturedV
      tell [SupCom w lhs newBinds (fmap (rename Map.!) rhs)]
      let unfree = \case
            Bound{} -> undefined -- NOTE: Everyhing in @capturedL@ starts with 'Free'.
            Free v  -> v
      let fun = Var w (mkTotallyFree lhs)
      case capturedL of
        []  -> return fun
        _:_ -> return $ App w fun (map (Var w . unfree) capturedL)

llTopLevel :: TopLevel In -> LL ()
llTopLevel = \case
  Def w lhs rhs -> do
    put $ Id.freshEVars "ll" lhs
    case rhs of
      Lam{} -> do
        -- NOTE: Make sure this lambda gets lifted to the name it is defining.
        modify (lhs:)
        void $ llExpr rhs
      _ -> do
        rhs <- llExpr rhs
        tell [Caf w lhs rhs]
  Asm w lhs asm -> tell [Asm w lhs asm]

liftModule :: Std.Module In -> Std.Module Out
liftModule = over module2tops (execLL . traverse_ llTopLevel)

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
  EVar w x -> pure (EVar w x)
  ECon w c -> pure (ECon w c)
  ENum w n -> pure (ENum w n)
  EApp w t  us -> EApp w <$> llExpr t <*> traverse llExpr us
  ECas w t  cs -> ECas w <$> llExpr t <*> traverse (case2rhs llExpr) cs
  ELet w ds t -> ELet w <$> (traverse . rhs2) llExpr ds <*> llExpr t
  ERec w ds t -> ERec w <$> (traverse . rhs2) llExpr ds <*> llExpr t
  ELam w oldBinds rhs -> do
    lhs <- freshIdent
    rhs <- llExpr rhs
    let isCaptured v = is _Free v && not (isTotallyFree v)
    let (capturedS, others) = Set.partition isCaptured $ Set.setOf traverse rhs
    -- TODO: Arrange 'captured' in a clever way. The @sortOn varName@ is just to
    -- not break the tests right now.
    let capturedL = sortOn varName $ Set.toList capturedS
    Vec.withList capturedL $ \capturedV -> do
      let newBinds = fmap (BName w . varName) capturedV Vec.++ oldBinds
      let renameOther = \case
            Bound i x -> Bound (Fin.shift i) x
            Free  v   -> Free  (varName v)
      let renameCaptured i v = Map.singleton v (mkBound (Fin.weaken i) (varName v))
      let rename = Map.fromSet renameOther others <> ifoldMap renameCaptured capturedV
      tell [TLSup w lhs newBinds (fmap (rename Map.!) rhs)]
      let unfree = \case
            Bound{} -> undefined -- NOTE: Everyhing in @capturedL@ starts with 'Free'.
            Free v  -> v
      let fun = EVar w (mkTotallyFree lhs)
      case capturedL of
        []  -> return fun
        _:_ -> return $ EApp w fun (map (EVar w . unfree) capturedL)

llTopLevel :: TopLevel In -> LL ()
llTopLevel = \case
  TLDef w lhs rhs -> do
    put $ Id.freshEVars "ll" lhs
    case rhs of
      ELam{} -> do
        -- NOTE: Make sure this lambda gets lifted to the name it is defining.
        modify (lhs:)
        void $ llExpr rhs
      _ -> do
        rhs <- llExpr rhs
        tell [TLCaf w lhs rhs]
  TLAsm w lhs asm -> tell [TLAsm w lhs asm]

liftModule :: Std.Module In -> Std.Module Out
liftModule = over module2tops (execLL . traverse_ llTopLevel)

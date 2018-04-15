module Pukeko.FrontEnd.Inferencer.Constraints where

import Pukeko.Prelude

import qualified Data.Map.Extended as Map
import qualified Data.Sequence as Seq
import           Data.STRef

import Pukeko.AST.Dict
import Pukeko.AST.Name
import Pukeko.AST.SystemF
import Pukeko.FrontEnd.Inferencer.UType
import Pukeko.FrontEnd.Inferencer.Gamma
import Pukeko.FrontEnd.Info
import Pukeko.Pretty

type CanSolve s effs =
  ( CanThrowHere effs
  , CanGamma s effs
  , Members [NameSource, Reader ModuleInfo] effs
  , MemberST s effs
  )

type MDictRef s = STRef s (MDict s)

data MDict s
  = MDVar DxVar
  | MDDer DxVar [UType s] [MDict s]
  | MMeta (MDictRef s)
  | MTodo

type UnsolvedCstrs s = Seq (UTypeCstr s, MDictRef s)

data SplitCstrs s = MkSplitCstrs
  { _retained :: Seq ((Class, TyVar), MDictRef s)
  , _deferred :: UnsolvedCstrs s
  }

freshConstraint :: CanSolve s effs => UTypeCstr s -> Eff effs (MDict s, UnsolvedCstrs s)
freshConstraint cstr = do
  dref <- sendM (newSTRef MTodo)
  pure (MMeta dref, Seq.singleton (cstr, dref))

freezeDict :: MDict s -> ST s Dict
freezeDict = \case
  MDVar x -> pure (DVar x)
  MDDer z ts ds -> DDer z <$> traverse freezeType ts <*> traverse freezeDict ds
  MTodo -> impossible
  MMeta dref -> readSTRef dref >>= freezeDict

throwUnsolvable :: CanSolve s effs => Maybe [Class] -> UTypeCstr s -> Eff effs a
throwUnsolvable mbCtxt (clss, typ) = do
  dTyp <- sendM (prettyUType 3 typ)
  let dCtxt = case mbCtxt of
        Nothing -> mempty
        Just ctxt ->
          "in context"
          <+> parens (hsep (punctuate "," (map (\c -> pretty c <+> dTyp) ctxt)))
  throwHere ("cannot solve constraint" <+> pretty clss <+> dTyp <+> dCtxt)

preSolveConstraint
  :: forall s effs
  .  CanSolve s effs
  => UTypeCstr s
  -> Eff (Writer (SplitCstrs s) : effs) (MDict s)
preSolveConstraint cstr@(clss, t0) = do
  (t1, targs) <- sendM (unwindUTApp t0)
  case t1 of
    UVar uref -> do
      cur_level <- getLevel @s
      sendM (readSTRef uref) >>= \case
        UFree v l
          | l > cur_level ->
              throwHere
                ("ambiguous type variable in constraint" <+> pretty clss <+> pretty v)
          | otherwise -> do
              dref <- sendM (newSTRef MTodo)
              tell (MkSplitCstrs mempty (Seq.singleton (cstr, dref)))
              pure (MMeta dref)
        ULink{} -> impossible  -- we've unwound 'UTApp's
    UTVar tvar ->
      lookupContext @s tvar >>= \case
        Nothing -> do
          dref <- sendM (newSTRef MTodo)
          tell (MkSplitCstrs (Seq.singleton ((clss, tvar), dref)) mempty)
          pure (MMeta dref)
        Just dvars ->
          case Map.lookup clss dvars of
            Nothing -> throwUnsolvable (Just (Map.keys dvars)) cstr
            Just dvar -> pure (MDVar dvar)
    UTAtm atom -> do
      lookupInfo info2insts (clss, atom) >>= \case
        Nothing -> throwUnsolvable Nothing cstr
        Just (SomeInstDecl (MkInstDecl z _ _ prms ctxt _)) -> do
          -- The kind checker guarantees the number of parameters/arguments to match.
          let inst = open1 . fmap (Map.fromList (zipExact prms targs) Map.!)
          MDDer z targs <$> traverse (preSolveConstraint . second inst . snd) ctxt
    UTApp{} -> impossible  -- we've unwound 'UTApp's
    UTUni{} -> impossible  -- we have only rank-1 types
    UTCtx{} -> impossible

solveConstraints
  :: forall s effs
  .  CanSolve s effs
  => UnsolvedCstrs s
  -> Eff effs ([DxBinder (UType s)], UnsolvedCstrs s)
solveConstraints ucstrs = do
  ((), MkSplitCstrs rets0 defs) <- runWriter $
    for_ ucstrs $ \(cstr, dref) -> do
      dict <- preSolveConstraint @s @effs cstr
      sendM (writeSTRef dref dict)
  let rets1 = Map.fromMultiList (toList rets0)
  rets2 <- for (Map.toList rets1) $ \((clss, tvar), drefs) -> do
    dvar <- mkDxVar clss tvar
    let dict = MDVar dvar
    for_ drefs $ \dref -> sendM (writeSTRef dref dict)
    pure (dvar, (clss, UTVar tvar))
  pure (rets2, defs)


instance Semigroup (SplitCstrs s) where
  MkSplitCstrs rets1 defs1 <> MkSplitCstrs rets2 defs2 =
    MkSplitCstrs (rets1 <> rets2) (defs1 <> defs2)

instance Monoid (SplitCstrs s) where
  mempty = MkSplitCstrs mempty mempty

module Pukeko.FrontEnd.Inferencer.Constraints where

import Pukeko.Prelude

import qualified Data.Map.Extended as Map
import qualified Data.Sequence as Seq
import           Data.STRef

import Pukeko.AST.Dict
import Pukeko.AST.Name
import Pukeko.AST.SystemF
import Pukeko.FrontEnd.Inferencer.Gamma
import Pukeko.FrontEnd.Inferencer.UType
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
  | MDSub DxVar Class (UType s) (MDict s)
  | MMeta (MDictRef s)
  | MTodo

type UnsolvedCstrs s = Seq (UTypeCstr s, MDictRef s)

data SplitCstrs s = MkSplitCstrs
  { _retained :: Seq ((Class, TyVar), MDictRef s)
  , _deferred :: UnsolvedCstrs s
  }

nullSplitCstrs :: SplitCstrs s -> Bool
nullSplitCstrs (MkSplitCstrs rets defs) = null rets && null defs

freshConstraint :: CanSolve s effs => UTypeCstr s -> Eff effs (MDict s, UnsolvedCstrs s)
freshConstraint cstr = do
  dref <- sendM (newSTRef MTodo)
  pure (MMeta dref, Seq.singleton (cstr, dref))

freezeDict :: MDict s -> ST s Dict
freezeDict = \case
  MDVar x -> pure (DVar x)
  MDDer z ts ds -> DDer z <$> traverse freezeType ts <*> traverse freezeDict ds
  MDSub z c t d -> DSub z c <$> freezeType t <*> freezeDict d
  MTodo -> impossible
  MMeta dref -> readSTRef dref >>= freezeDict

throwUnsolvable :: CanSolve s effs => Maybe [UTypeCstr s] -> UTypeCstr s -> Eff effs a
throwUnsolvable mbCtxt cstr = do
  dCstr <- sendM (prettyUTypeCstr cstr)
  dCtxt <- case mbCtxt of
    Nothing -> pure mempty
    Just ctxt -> do
      dCstrs <- sendM (traverse prettyUTypeCstr ctxt)
      pure ("in context" <+> parens (hsep (punctuate "," dCstrs)))
  throwHere ("cannot solve constraint" <+> dCstr <+> dCtxt)
  where
    prettyUTypeCstr (clss, typ) = (<+>) (pretty clss) <$> prettyUType 3 typ


solveRigid
  :: forall s effs. CanSolve s effs
  => UTypeCstr s -> Map Class DxVar -> Eff effs (MDict s)
solveRigid cstr0@(clss0, type0) ctxt0 = go (fmap MDVar ctxt0)
  where
    -- 'go' searches for the /shortest/ derivation of the constraint.
    go :: Map Class (MDict s) -> Eff effs (MDict s)
    go ctxt1
      | null ctxt1 = throwUnsolvable (Just (zip (Map.keys ctxt0) (repeat type0))) cstr0
      | Just dict <- ctxt1 Map.!? clss0 = pure dict
      | otherwise = do
          -- TODO: Simplify code.
          ctxt2 <- for (Map.toList ctxt1) $ \(clss, dict) -> do
            clssDecl <- findInfo info2classes clss
            pure $ case clssDecl ^. class2super of
              Just (z, super) -> Just (super, MDSub z clss type0 dict)
              Nothing         -> Nothing
          go (Map.fromList (catMaybes ctxt2))

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
        Just ctxt -> solveRigid cstr ctxt
    UTAtm atom ->
      lookupInfo info2insts (clss, atom) >>= \case
        Nothing -> throwUnsolvable Nothing cstr
        Just (SomeInstDecl (MkInstDecl z _ _ prms ctxt _ _)) -> do
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

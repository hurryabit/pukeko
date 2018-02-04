{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Pukeko.FrontEnd.Info
  ( ModuleInfo
  , SomeInstDecl (..)
  , info2tcons
  , info2dcons
  , info2signs
  , info2clsss
  , info2mthds
  , info2insts
  , runInfo
  , runInfoSC
  , lookupInfo
  , findInfo
  , typeOfFunc
  , typeOfAtom
  , tconDeclInfo
  ) where

import Pukeko.Prelude

import           Control.Lens                (foldMapOf, _Right)
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set

import           Pukeko.AST.SystemF
import           Pukeko.AST.SuperCore
import           Pukeko.AST.Language
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type


data ModuleInfo = MkModuleInfo
  { _info2tcons :: Map Id.TCon TConDecl
  , _info2dcons :: Map Id.DCon (TConDecl, DConDecl)
  , _info2signs :: Map Id.EVar (SignDecl Void)
  , _info2clsss :: Map Id.Clss ClssDecl
  , _info2mthds :: Map Id.EVar (ClssDecl, SignDecl (TScope Int Void))
  , _info2insts :: Map (Id.Clss, Id.TCon) SomeInstDecl
  }

data SomeInstDecl = forall st. SomeInstDecl (InstDecl st)

makeLenses ''ModuleInfo
makePrisms ''ModuleInfo

runInfo ::
  (IsType (TypeOf st)) =>
  Module st -> Eff (Reader ModuleInfo : effs) a -> Eff effs a
runInfo = runReader . collectInfo

runInfoSC :: ModuleSC -> Eff (Reader ModuleInfo : effs) a -> Eff effs a
runInfoSC = runReader . collectInfoSC

lookupInfo ::
  (Member (Reader ModuleInfo) effs, Ord k) =>
  Lens' ModuleInfo (Map k v) -> k -> Eff effs (Maybe v)
lookupInfo l k = Map.lookup k <$> view l

findInfo ::
  (HasCallStack, Member (Reader ModuleInfo) effs, Ord k, Show k) =>
  Lens' ModuleInfo (Map k v) -> k -> Eff effs v
findInfo l k = Map.findWithDefault (bugWith "findInfo" k) k <$> view l

typeOfFunc ::
  (HasCallStack, Member (Reader ModuleInfo) effs) =>
  Id.EVar -> Eff effs (Type tv)
typeOfFunc func = fmap absurd . _sign2type <$> findInfo info2signs func

typeOfAtom :: (HasCallStack, Member (Reader ModuleInfo) effs) =>
  Atom -> Eff effs (Type tv)
typeOfAtom = \case
  AVal z -> typeOfFunc z
  ACon c -> fmap absurd <$> uncurry typeOfDCon <$> findInfo info2dcons c
  ANum _ -> pure typeInt

itemInfo :: (Ord k) => Lens' ModuleInfo (Map k v) -> k -> v -> ModuleInfo
itemInfo l k v = set l (Map.singleton k v) mempty

tconDeclInfo :: TConDecl -> ModuleInfo
tconDeclInfo tcon =
  let dis = foldMapOf
        (tcon2dcons . _Right . traverse)
        (\dcon -> itemInfo info2dcons (dcon^.dcon2name.lctd) (tcon, dcon))
        tcon
  in  itemInfo info2tcons (tcon^.tcon2name.lctd) tcon <> dis

-- FIXME: Detect multiple definitions.
signDeclInfo :: SignDecl Void -> ModuleInfo
signDeclInfo s = itemInfo info2signs (unlctd (s^.sign2func)) s

supcDeclInfo :: SupCDecl -> ModuleInfo
supcDeclInfo (MkSupCDecl z xs t _ _) = signDeclInfo (MkSignDecl z (mkTUni xs t))

bindInfo :: IsType ty => Bind ty Void -> ModuleInfo
bindInfo (MkBind z t) = maybe mempty (signDeclInfo . MkSignDecl z) (isType t)

extnDeclInfo :: IsType ty => ExtnDecl ty -> ModuleInfo
extnDeclInfo (MkExtnDecl b _) = bindInfo b

collectInfo :: (IsType (TypeOf st)) => Module st -> ModuleInfo
collectInfo (MkModule decls) = foldFor decls $ \case
  DType tcons -> foldMap tconDeclInfo tcons
  DSign s -> signDeclInfo s
  DClss clss@(MkClssDecl (unlctd -> c) v ms) ->
    let mthds_info = foldFor ms $ \mthd@(MkSignDecl z t0) ->
          let t1 = mkTUni [MkQVar (Set.singleton c) v] t0
          in  signDeclInfo (MkSignDecl z t1)
              <> itemInfo info2mthds (z^.lctd) (clss, mthd)
    in  itemInfo info2clsss c clss <> mthds_info
  DInst inst@(MkInstDecl (unlctd -> c) t _ _) ->
    itemInfo info2insts (c, t) (SomeInstDecl inst)
  DDefn (MkDefn b _) -> bindInfo b
  DSupC supc -> supcDeclInfo supc
  DExtn extn -> extnDeclInfo extn
  where
    foldFor :: (Foldable t, Monoid m) => t a -> (a -> m) -> m
    foldFor = flip foldMap


collectInfoSC :: ModuleSC -> ModuleInfo
collectInfoSC (MkModuleSC types extns supcs) = fold
  [ foldMap tconDeclInfo types
  , foldMap extnDeclInfo extns
  , foldMap supcDeclInfo supcs
  ]

instance Semigroup ModuleInfo where
  MkModuleInfo a1 b1 c1 d1 e1 f1 <> MkModuleInfo a2 b2 c2 d2 e2 f2 =
    MkModuleInfo (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2)

instance Monoid ModuleInfo where
  mempty = MkModuleInfo mempty mempty mempty mempty mempty mempty
  mappend = (<>)

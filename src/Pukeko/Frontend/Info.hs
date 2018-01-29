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
  , lookupInfo
  , findInfo
  , typeOfDCon
  , typeOfFunc
  , tconDeclInfo
  ) where

import Pukeko.Prelude

import           Control.Lens                (foldMapOf, _Right)
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import qualified Data.Vector.Sized           as Vec

import           Pukeko.AST.SystemF
import           Pukeko.AST.Stage
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type


data ModuleInfo = MkModuleInfo
  { _info2tcons :: Map Id.TCon (Some1 TConDecl)
  , _info2dcons :: Map Id.DCon (Some1 (Pair1 TConDecl DConDecl))
  , _info2signs :: Map Id.EVar (SignDecl Void)
  , _info2clsss :: Map Id.Clss ClssDecl
  , _info2mthds :: Map Id.EVar (ClssDecl, SignDecl (TFinScope 1 Void))
  , _info2insts :: Map (Id.Clss, Id.TCon) SomeInstDecl
  }

data SomeInstDecl = forall st. SomeInstDecl (InstDecl st)

makeLenses ''ModuleInfo
makePrisms ''ModuleInfo

runInfo ::
  (IsType (StageType st)) =>
  Module st -> Eff (Reader ModuleInfo : effs) a -> Eff effs a
runInfo = runReader . collectInfo

lookupInfo ::
  (Member (Reader ModuleInfo) effs, Ord k) =>
  Lens' ModuleInfo (Map k v) -> k -> Eff effs (Maybe v)
lookupInfo l k = Map.lookup k <$> view l

findInfo ::
  (HasCallStack, Member (Reader ModuleInfo) effs, Ord k, Show k) =>
  Lens' ModuleInfo (Map k v) -> k -> Eff effs v
findInfo l k = Map.findWithDefault (bugWith "findInfo" k) k <$> view l

typeOfDCon ::
  (HasCallStack, Member (Reader ModuleInfo) effs) => Id.DCon -> Eff effs (Type tv)
typeOfDCon dcon = do
  Some1 (Pair1 tconDecl dconDecl) <- findInfo info2dcons dcon
  pure (fmap absurd (typeOfDConDecl tconDecl dconDecl))

typeOfFunc ::
  (HasCallStack, Member (Reader ModuleInfo) effs) =>
  Id.EVar -> Eff effs (Type tv)
typeOfFunc func = fmap absurd . _sign2type <$> findInfo info2signs func

itemInfo :: (Ord k) => Lens' ModuleInfo (Map k v) -> k -> v -> ModuleInfo
itemInfo l k v = set l (Map.singleton k v) mempty

tconDeclInfo :: Some1 TConDecl -> ModuleInfo
tconDeclInfo (Some1 tcon) =
  let dis = foldMapOf
        (tcon2dcons . _Right . traverse . unWhere lctd)
        (\dcon -> itemInfo info2dcons (dcon^.dcon2name) (Some1 (Pair1 tcon dcon)))
        tcon
  in  itemInfo info2tcons (tcon^.tcon2name) (Some1 tcon) <> dis

collectInfo :: forall st. (IsType (StageType st)) => Module st -> ModuleInfo
collectInfo (MkModule decls) = foldFor decls $ \decl -> case unlctd decl of
  DType tcons -> foldMap (tconDeclInfo . unlctd) tcons
  DSign s -> sign s
  DClss clss@(MkClssDecl c v ms) ->
    let mthds_info = foldFor ms $ \mthd@(MkSignDecl z t0) ->
          let t1 = mkTUni (Vec.singleton (MkQVar (Set.singleton c) v)) t0
          in  sign (MkSignDecl z t1) <> itemInfo info2mthds z (clss, mthd)
    in  itemInfo info2clsss c clss <> mthds_info
  DInst inst@(MkInstDecl c t _ _) -> itemInfo info2insts (c, t) (SomeInstDecl inst)
  DDefn (MkDefn b _) -> signBind b
  DSupC (MkSupCDecl z xs t _ _) -> sign (MkSignDecl z (mkTUni xs t))
  DPrim (MkPrimDecl b _) -> signBind b
  where
    foldFor :: (Foldable t, Monoid m) => t a -> (a -> m) -> m
    foldFor = flip foldMap
    -- FIXME: Detect multiple definitions.
    sign :: SignDecl Void -> ModuleInfo
    sign s = itemInfo info2signs (s^.sign2func) s
    signBind :: IsType ty => Bind ty Void -> ModuleInfo
    signBind (MkBind z t) = maybe mempty (sign . MkSignDecl z) (isType t)

instance Semigroup ModuleInfo where
  MkModuleInfo a1 b1 c1 d1 e1 f1 <> MkModuleInfo a2 b2 c2 d2 e2 f2 =
    MkModuleInfo (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2)

instance Monoid ModuleInfo where
  mempty = MkModuleInfo mempty mempty mempty mempty mempty mempty
  mappend = (<>)

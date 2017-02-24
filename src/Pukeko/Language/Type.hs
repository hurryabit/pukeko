module Pukeko.Language.Type
  ( Type (..)
  , var
  , (~>)
  , app
  , record
  , int
  , unit
  , bool
  , pair
  , list
  , io
  , unify
  , unifyMany
  , module Pukeko.Language.Ident
  , module Pukeko.Language.Term
  )
  where

import Control.Arrow (second)
import Control.Monad.Except
import Data.Char (isLower, isUpper)
import Data.Ratio () -- for precedences in pretty printer

import qualified Data.Set as Set

import Pukeko.Language.Ident
import Pukeko.Language.Term hiding ((<>))
import Pukeko.Pretty hiding (int)

infixr 1 ~>

data Type
  = Var (Var Type)
  | Fun Type Type
  | App Ident [Type]
  | Rec [(Ident, Type)]
  deriving (Eq)

instance Term Type where
  newtype Var Type = MkVar String
    deriving (Eq, Ord)
  supply = map MkVar (tail vars)
    where
      vars = "_":[ xs ++ [x] | xs <- vars, x <- ['A'..'Z'] ]
  promote = Var
  freeVars t =
    case t of
      Var v     -> Set.singleton v
      Fun tx ty -> Set.union (freeVars tx) (freeVars ty)
      App _  ts -> Set.unions (map freeVars ts)
      Rec fs    -> Set.unions (map (freeVars . snd) fs)
  subst phi t =
    case t of
      Var v     -> substVar phi v
      Fun tx ty -> Fun (subst phi tx) (subst phi ty)
      App c  ts -> App c $ map (subst phi) ts
      Rec fs    -> Rec $ map (second $ subst phi) fs

var :: String -> Type
var name@(start:_)
  | isUpper start = Var (MkVar name)
var name          = perror $ text name <+> text "is not a valid variable name"

(~>) :: Type -> Type -> Type
(~>) = Fun

app :: Ident -> [Type] -> Type
app name@(MkIdent (start:_)) ts
  | isLower start = App name ts
app name _        = perror $ pPrint name <+> text "is not a valid type constructor name"

record :: [(Ident, Type)] -> Type
record = Rec

int, unit, bool :: Type
int  = app (MkIdent "int")  []
unit = app (MkIdent "unit") []
bool = app (MkIdent "bool") []

pair :: Type -> Type -> Type
pair t1 t2 = app (MkIdent "pair") [t1, t2]

list :: Type -> Type
list t = app (MkIdent "list") [t]

io :: Type -> Type
io t = app (MkIdent "io") [t]

extend :: MonadError String m => Subst Type -> Var Type -> Type -> m (Subst Type)
extend phi v t =
  case t of
    Var u
      | u == v                    -> return phi
    _
      | Set.member v (freeVars t) -> pthrow (text "cyclic type variable" <+> pretty v)
      | otherwise                 -> return (assign v t <> phi)

-- | @unify phi v t@ finds an mgu @psi@ of @phi v == phi t@ and returns @psi <> phi@.
unifyVar :: MonadError String m => Subst Type -> Var Type -> Type -> m (Subst Type)
unifyVar phi v t =
  case substVar phi v of
    Var u
      | u == v -> extend phi v phi_t
    phi_v      -> unify phi phi_v phi_t
    where
      phi_t = subst phi t

-- | @unify phi s t@ finds an mgu @psi@ of @phi s = phi t@ and returns @psi <> phi@.
unify :: MonadError String m => Subst Type -> Type -> Type -> m (Subst Type)
unify phi t1 t2 =
  case (t1, t2) of
    (Var v1     , _          ) -> unifyVar phi v1 t2
    (_          , Var v2     ) -> unifyVar phi v2 t1
    (Fun tx1 ty1, Fun tx2 ty2) -> unifyMany phi [(tx1, tx2), (ty1, ty2)]
    (App c1  ts1, App c2  ts2)
      | c1 == c2               -> unifyMany phi (zip ts1 ts2)
    (Rec ds1    , Rec ds2    ) -> do
      let checkLabels [] [] acc = return acc
          checkLabels ((i1, _):_) [] _ =
            pthrow $ hsep [text "field", pretty i1, text "not present in record", pretty t2]
          checkLabels [] ((i2, _):_) _ =
            pthrow $ hsep [text "field", pretty i2, text "not present in record", pretty t1]
          checkLabels ((i1, t_i1):ds1') ((i2, t_i2):ds2') acc
            | i1 == i2  = checkLabels ds1' ds2' ((t_i1, t_i2):acc)
            | otherwise =
                pthrow $ hsep [text "record fields", pretty i1, text "and", pretty i2, text "do not match"]
      eqs <- checkLabels ds1 ds2 []
      unifyMany phi eqs
    _ -> pthrow $ hsep [text "mismatching types", pretty t1, text "and", pretty t2]

-- | @unifyMany phi eqs@ finds an mgu @psi@ of @{ phi s1 = phi t1, ..., phi sn = phi tn }@,
-- where @eqs = [(s1,t1), ..., (sn,tn)]@, and returns @psi <> phi@.
unifyMany :: MonadError String m => Subst Type -> [(Type, Type)] -> m (Subst Type)
unifyMany = foldM (uncurry . unify)


instance Pretty (Var Type) where
  pPrint (MkVar a) = text a

instance Show (Var Type) where
  show = prettyShow

instance Pretty Type where
  pPrintPrec lvl prec t =
    case t of
      Var v    -> pretty v
      Fun s t  ->
        maybeParens (prec > 1) $ pPrintPrec lvl 2 s <+> text "->" <+> pPrintPrec lvl 1 t
      App c [] -> pretty c
      App c ts ->
        maybeParens (prec > 2) $ pretty c <+> hsep (map (pPrintPrec lvl 3) ts)
      Rec ds   -> braces $ hsep $ punctuate comma (map field ds)
    where
      field (l, t) = pPrint l <> colon <+> pPrint t

instance Show Type where
  show = prettyShow

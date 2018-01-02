module Pukeko.Language.TypeChecker.Unify
  ( TU
  , unify
  )
where

import           Control.Monad.Except
import           Control.Monad.ST
import           Data.STRef

import           Pukeko.Error
import           Pukeko.Pos
import           Pukeko.Pretty
import           Pukeko.Language.Type
import           Pukeko.Language.TypeChecker.AST (TCon)

type TypeOpen s = Type TCon (Open s)

type TU s = ExceptT String (ST s)

-- TODO: link compression
unwind :: TypeOpen s -> TU s (TypeOpen s)
unwind t = case t of
  UVar uref -> do
    uvar <- lift $ readSTRef uref
    case uvar of
      Link{_type} -> unwind _type
      Free{} -> return t
  _ -> return t

occursCheck :: STRef s (UVar TCon s) -> TypeOpen s -> TU s ()
occursCheck uref1 t2 = case t2 of
  UVar uref2
    | uref1 == uref2 -> throwError "occurs check"
    | otherwise      -> do
        uvar2 <- lift $ readSTRef uref2
        case uvar2 of
          Free{_ident, _level = level2} -> do
            uvar1 <- lift $ readSTRef uref1
            case uvar1 of
              Free{_level = level1} ->
                lift $ writeSTRef uref2 Free{_ident, _level = min level1 level2}
              _ -> bug "type unifier" "bad case in occurs check" Nothing
          Link t2' -> occursCheck uref1 t2'
  Var _ -> pure ()
  Con _ -> pure ()
  Arr   -> pure ()
  App tf tp -> occursCheck uref1 tf *> occursCheck uref1 tp

unify :: Pos -> TypeOpen s -> TypeOpen s -> TU s ()
unify pos t1 t2 = do
  t1 <- unwind t1
  t2 <- unwind t2
  case (t1, t2) of
    (UVar uref1, UVar uref2) | uref1 == uref2 -> return ()
    (UVar uref1, _) -> do
      uvar1 <- lift $ readSTRef uref1
      case uvar1 of
        Free{_ident} -> do
          occursCheck uref1 t2 `catchError` \_ -> do
            p2 <- lift $ prettyType prettyNormal 0 t2
            throwDocAt pos $ quotes (pretty _ident) <+> "occurs in" <+> p2
          lift $ writeSTRef uref1 $ Link{_type = t2}
        _ -> bug "type unifier" "bad pattern in unifier" Nothing
    (_, UVar _) -> unify pos t2 t1
    (Var name1, Var name2)
      | name1 == name2 -> pure ()
    (Arr, Arr) -> pure ()
    -- TODO: Make ADT comparable itself.
    (Con MkADT{_name = c1}, Con MkADT{_name = c2})
      | c1 == c2 -> pure ()
    -- NOTE: The kind checker has ensured that @length ts == length us@.
    (App tf1 tp1, App tf2 tp2) ->
      unify pos tf1 tf2 *> unify pos tp1 tp2
    _ -> do
      p1 <- lift $ prettyType prettyNormal 0 t1
      p2 <- lift $ prettyType prettyNormal 0 t2
      throwDocAt pos $ "mismatching types" <+> p1 <+> "and" <+> p2

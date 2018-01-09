module Pukeko.Language.Inferencer.Unify
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
import           Pukeko.Language.Inferencer.UType

type TU s = ExceptT String (ST s)

-- TODO: link compression
unwind :: UType s tv -> TU s (UType s tv)
unwind t0 = case t0 of
  UVar uref -> do
    uvar <- lift $ readSTRef uref
    case uvar of
      ULink t1 -> unwind t1
      UFree _ _ -> return t0
  _ -> return t0

occursCheck :: STRef s (UVar s tv) -> UType s tv -> TU s ()
occursCheck uref1 t2 = case t2 of
  UVar uref2
    | uref1 == uref2 -> throwError "occurs check"
    | otherwise      -> do
        uvar2 <- lift $ readSTRef uref2
        case uvar2 of
          UFree x2 l2 -> do
            uvar1 <- lift $ readSTRef uref1
            case uvar1 of
              UFree _ l1 ->
                lift $ writeSTRef uref2 (UFree x2 (min l1 l2))
              _ -> bug "bad case in occurs check"
          ULink t2' -> occursCheck uref1 t2'
  UTVar _ -> pure ()
  UTCon _ -> pure ()
  UTArr   -> pure ()
  UTUni{} -> bug "universal quantification in occurs check"
  UTApp tf tp -> occursCheck uref1 tf *> occursCheck uref1 tp

unify :: Pos -> UType s tv -> UType s tv -> TU s ()
unify pos t1 t2 = do
  t1 <- unwind t1
  t2 <- unwind t2
  case (t1, t2) of
    (UVar uref1, UVar uref2) | uref1 == uref2 -> return ()
    (UVar uref1, _) -> do
      uvar1 <- lift $ readSTRef uref1
      case uvar1 of
        UFree x1 _ -> do
          occursCheck uref1 t2 `catchError` \_ -> do
            p2 <- lift $ prettyUType prettyNormal 0 t2
            throwDocAt pos $ quotes (pretty x1) <+> "occurs in" <+> p2
          lift $ writeSTRef uref1 (ULink t2)
        _ -> bug "bad pattern in unify"
    (_, UVar _) -> unify pos t2 t1
    (UTVar x1, UTVar x2)
      | x1 == x2 -> pure ()
    (UTArr, UTArr) -> pure ()
    (UTCon tcon1, UTCon tcon2)
      | tcon1 == tcon2 -> pure ()
    (UTApp tf1 tp1, UTApp tf2 tp2) ->
      unify pos tf1 tf2 *> unify pos tp1 tp2
    _ -> do
      p1 <- lift $ prettyUType prettyNormal 0 t1
      p2 <- lift $ prettyUType prettyNormal 0 t2
      throwDocAt pos $ "mismatching types" <+> p1 <+> "and" <+> p2

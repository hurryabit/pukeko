module Pukeko.Monomorphic.Subtyping
  ( isSubtypeOf
  , join
  , meet
  )
  where

import Control.Monad hiding (join)

import qualified Data.Map as Map

import Pukeko.Language.Type
import Pukeko.Pretty

infix  4 `isSubtypeOf`, `equiv`
infixl 6 `join`
infixl 7 `meet`

tvError = perror (text "type variables not implemented")

isSubtypeOf, equiv :: Type -> Type -> Bool
t1 `equiv` t2 = t1 `isSubtypeOf` t2 && t2 `isSubtypeOf` t1
t1 `isSubtypeOf` t2 =
  case (t1, t2) of
    (Var _        , _            ) -> tvError
    (_            , Var _        ) -> tvError
    (Fun t1_x t1_y, Fun t2_x t2_y) -> t2_x `isSubtypeOf` t1_x && t1_y `isSubtypeOf` t2_y
    (App c1 ts1   , App c2 ts2   ) -> c1 == c2 && and (zipWith equiv ts1 ts2)
    (Rec lts1     , Rec lts2     ) ->
      -- TODO: Use Map.mergeWithKey
      all (\(l, t2) -> maybe False (`isSubtypeOf` t2) (lookup l lts1)) lts2
    (_            , _            ) -> False

join :: Type -> Type -> Maybe Type
join = combine join meet joinRecords
meet = combine meet join meetRecords

joinRecords mp1 mp2 = Just $ Map.mapMaybe id $ Map.intersectionWith join mp1 mp2
meetRecords mp1 mp2 = sequence $
  Map.mergeWithKey (\_ t1 t2 -> Just (t1 `meet` t2)) (Map.map Just) (Map.map Just) mp1 mp2

combine join meet combineRecords t1 t2 =
  case (t1, t2) of
    (Var _        , _            ) -> tvError
    (_            , Var _        ) -> tvError
    (Fun t1_x t1_y, Fun t2_x t2_y) -> (~>) <$> (t1_x `meet` t2_x) <*> (t1_y `join` t2_y)
    (App _ _      , App _ _      ) -> guard (t1 `equiv` t2) *> pure t1
    (Rec lts1     , Rec lts2     ) -> 
      Rec <$> Map.toList <$> combineRecords (Map.fromList lts1) (Map.fromList lts2)
    (_            , _            ) -> Nothing

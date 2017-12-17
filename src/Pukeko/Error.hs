module Pukeko.Error
  ( throwDoc
  , throwDocAt
  , throwErrorAt
  , throw
  , throwAt
  , bug
  , MonadError (..)
  , Except
  , runExcept
  , ExceptT (..)
  , runExceptT
  )
  where

import           Control.Monad.Except hiding (runExcept, runExceptT)
import qualified Control.Monad.Except as Except

import Pukeko.Pos
import Pukeko.Pretty

throwDoc :: MonadError String m => Doc -> m a
throwDoc = throwError . render

throwDocAt :: MonadError String m => Pos -> Doc -> m a
throwDocAt posn msg = throwDoc $ text (show posn) <> colon <+> msg

throwErrorAt :: MonadError String m => Pos -> String -> m a
throwErrorAt posn = throwDocAt posn . text

throw :: (MonadError String m, Pretty a) => String -> a -> m b
throw thing name = throwDoc $ text thing <+> quotes (pretty name)

throwAt :: (MonadError String m, Pretty a)
        => Pos -> String -> a -> m b
throwAt posn thing name = throwDocAt posn $ text thing <+> quotes (pretty name)

bug :: String -> String -> Maybe String -> a
bug where_ what name_opt = error $
  "BUG! " ++ what ++ maybe "" (\name -> " (" ++ name ++ ")") name_opt ++
  " @ " ++ where_

runExcept :: MonadError e m => Except e a -> m a
runExcept = either throwError return . Except.runExcept

runExceptT :: (MonadError e m, Monad n) => ExceptT e n a -> n (m a)
runExceptT = fmap (either throwError return) . Except.runExceptT

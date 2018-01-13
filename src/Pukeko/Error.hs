module Pukeko.Error
  ( throwDoc
  , throwDocAt
  , throwErrorAt
  , throw
  , throwAt
  , bug
  , bugWith
  , MonadError (..)
  , Except
  , runExcept
  , ExceptT (..)
  , runExceptT
  , here
  )
  where

import           Control.Monad.Except hiding (runExcept, runExceptT)
import qualified Control.Monad.Except as Except
import           Data.CallStack

import Pukeko.AST.Pos
import Pukeko.Pretty

throwDoc :: MonadError String m => Doc -> m a
throwDoc = throwError . render

throwDocAt :: MonadError String m => Pos -> Doc -> m a
throwDocAt posn msg = throwDoc $ pretty posn <> colon <+> msg

throwErrorAt :: MonadError String m => Pos -> String -> m a
throwErrorAt posn = throwDocAt posn . text

throw :: (MonadError String m, Pretty a) => String -> a -> m b
throw thing name = throwDoc $ text thing <+> quotes (pretty name)

throwAt :: (MonadError String m, Pretty a)
        => Pos -> String -> a -> m b
throwAt posn thing name = throwDocAt posn $ text thing <+> quotes (pretty name)

bug :: HasCallStack => String -> a
bug msg = error ("BUG! " ++ msg)

bugWith :: (HasCallStack, Show b) => String -> b -> a
bugWith msg x = bug (msg ++ " (" ++ show x ++ ")")

runExcept :: MonadError e m => Except e a -> m a
runExcept = either throwError return . Except.runExcept

runExceptT :: (MonadError e m, Monad n) => ExceptT e n a -> n (m a)
runExceptT = fmap (either throwError return) . Except.runExceptT

here :: MonadError String m => Pos -> m a -> m a
here pos act = act `catchError` throwErrorAt pos

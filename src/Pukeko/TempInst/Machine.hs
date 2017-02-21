{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Pukeko.TempInst.Machine
  ( Machine
  , runMachine
  , Addr
  , alloc
  , deref
  , update
  , free
  , usedAddrs
  , heapSize
  , heapFree
  , heapUsage
  , stackNull
  , stackSize
  , top
  , pop
  , push
  , mapStack
  , dumpNull
  , dump
  , undump
  , mapDump
  , output
  , debug
  , picture
  , module Control.Monad.Except
  , module Control.Monad.RWS
  )
  where

import Control.Monad.Except
import Control.Monad.RWS

import Data.Array (Array, (!), (//))
import qualified Data.Array as Array
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.Maybe (mapMaybe)

import Text.Printf


-- Data types
data State h i s = State
  { _stack :: [i]
  , _dump  :: [[i]]
  , _heap  :: Array Int (Maybe h)
  , _free  :: IntSet -- free addresses
  , _user  :: s
  }

newtype Machine h i r w s a =
  Machine { getMachine :: ExceptT String (RWST r w (State h i s) IO) a }
  deriving ( Functor, Applicative
           , MonadError String
           , MonadReader r
           , MonadWriter w
           )

runMachine :: (Show h, Show i) => Machine h i r w s a -> Int -> r -> s -> IO (Either String a, String, s, w)
runMachine machine heapSize env userState = do
  let _heap = Array.listArray (1, heapSize) (repeat Nothing)
      state =
        State
          { _stack = []
          , _dump  = []
          , _heap
          , _free  = IntSet.fromAscList (Array.indices _heap)
          , _user  = userState
          }
  (res, state, log) <- 
    runRWST (runExceptT (getMachine machine)) env state
  return (res, show state, _user state, log)

instance Monoid w => Monad (Machine h i r w s) where
  return  = Machine . return
  m >>= f = Machine $ getMachine m >>= getMachine . f
  fail    = Machine . throwError

instance Monoid w => MonadState s (Machine h i r w s) where
  get = Machine $ gets _user
  put _user = Machine $ modify $ \state -> state { _user }


-- Heap operations
newtype Addr = Addr Int

alloc :: Monoid w => h -> Machine h i r w s Addr
alloc item = Machine $ do
  free <- gets _free
  case IntSet.minView free of
    Nothing -> throwError "heap full"
    Just (idx, _free) -> do
      modify $ \state@(State { _heap }) ->
        state
          { _heap = _heap // [(idx, Just item)]
          , _free
          }
      return (Addr idx)

deref :: Monoid w => Addr -> Machine h i r w s h
deref addr@(Addr idx) = Machine $ do
  heap <- gets _heap
  case heap ! idx of
    Nothing   -> throwError (printf "invalid address %s" (show addr))
    Just item -> return item

update :: Monoid w => Addr -> h -> Machine h i r w s ()
update (Addr idx) item = Machine $
  modify $ \state@(State { _heap }) ->
    state { _heap = _heap // [(idx, Just item)] }

free :: Monoid w => Addr -> Machine h i r w s ()
free (Addr idx) = Machine $ 
  modify $ \state@(State { _heap, _free }) ->
    state
      { _heap = _heap // [(idx, Nothing)]
      , _free = IntSet.insert idx _free
      }

usedAddrs :: Monoid w => Machine h i r w s [Addr]
usedAddrs = Machine $ do
  heap <- gets _heap
  return [ Addr idx | (idx, Just _) <- Array.assocs heap ]

heapSize', heapFree', heapUsage' :: State h i s -> Int
heapSize' = Array.rangeSize . Array.bounds . _heap
heapFree' = IntSet.size . _free
heapUsage' = (-) <$> heapSize' <*> heapFree'


heapSize, heapFree, heapUsage :: Monoid w => Machine h i r w s Int
heapSize  = Machine $ gets heapSize'
heapFree  = Machine $ gets heapFree'
heapUsage = Machine $ gets heapUsage'


-- Stack operations
stackNull :: Monoid w => Machine h i r w s Bool
stackNull = Machine $ gets (null . _stack)

stackSize :: Monoid w => Machine h i r w s Int
stackSize = Machine $ gets (length . _stack)

top :: Monoid w => Machine h i r w s i
top = Machine $ do
  stack <- gets _stack
  case stack of
    []     -> throwError "empty stack"
    item:_ -> return item

pop :: Monoid w => Machine h i r w s ()
pop = Machine $ do
  stack <- gets _stack
  case stack of
    []       -> throwError "empty stack"
    _:_stack -> modify $ \state -> state { _stack }

push :: Monoid w => i -> Machine h i r w s ()
push item = Machine $
  modify $ \state@(State { _stack }) -> state { _stack = item:_stack }

mapStack :: Monoid w => (i -> Machine h i r w s i) -> Machine h i r w s ()
mapStack act = Machine $ do
  _stack <- gets _stack 
  _stack <- mapM (getMachine . act) _stack
  modify $ \state -> state { _stack }


-- Dump operations
dumpNull :: Monoid w => Machine h i r w s Bool
dumpNull = Machine $ gets (null . _dump)

dump :: Monoid w => Machine h i r w s ()
dump = Machine $
  modify $ \state@(State { _stack, _dump }) ->
    state { _stack = [], _dump = _stack : _dump }

undump :: Monoid w => Machine h i r w s ()
undump = Machine $ do
  dump <- gets _dump
  case dump of
    []           -> throwError "empty dump"
    _stack:_dump -> modify $ \state -> state { _stack, _dump }

mapDump :: Monoid w => (i -> Machine h i r w s i) -> Machine h i r w s ()
mapDump act = Machine $ do
  _dump <- gets _dump
  _dump <- mapM (mapM (getMachine . act)) _dump
  modify $ \state -> state { _dump }


-- Output operations
output :: Monoid w => String -> Machine h i r w s ()
output = Machine . liftIO . putStrLn


-- Debugging stuff
debug :: Monoid w => String -> Machine h i r w s ()
debug = output

picture :: (Show h, Show i, Monoid w) => Machine h i r w s String
picture = Machine $ gets show


-- Show instances
instance Show Addr where
  show (Addr n) = printf "#%04d" n

instance (Show h, Show i) => Show (State h i s) where
  show (state@State { _stack, _dump, _heap }) =
    List.intercalate "\n" $ concat $
      [ [ printf "Heap Usage = %d / %d" (heapUsage' state) (heapSize' state)
        , "Stack = ["
        ]
      , map (printf "  %s" . show) _stack
      , [ "]"
        , "Dump = ["
        ]
      , map showDumpItem _dump
      , [ "]"
        , "Heap = ["
        ]
      , mapMaybe showHeapItem (Array.assocs _heap)
      , [ "]"
        ]
      ]
    where
      showDumpItem stack = concat
        [ "  ["
        , unwords $ map show stack
        , "]"
        ]
      showHeapItem (_, Nothing) = Nothing
      showHeapItem (idx, Just item) = Just $
        printf "  %s: %s" (show (Addr idx)) (show item)


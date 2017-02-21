module Pukeko.Language.Mark1 where

import Control.Monad.Except
import Control.Monad.RWS

import Data.Array (Array, (!), (//))
import qualified Data.Array as Array
import Data.Maybe (fromJust, isJust, mapMaybe)

import Pukeko.Language.Parser (parse)
import Pukeko.Language.Syntax

type Environment = [(Identifier, Addr)]

newtype Addr = Addr Int

instance Show Addr where
  show (Addr n) = 
    let s = show n
        p = if length s < 4 then replicate (4-length s) '0' else ""
    in  '#' : p ++ s

data Node
  = Application Addr Addr
  | Function    Identifier [Identifier] Expr
  | Number      Integer
  | Indirection Addr
  deriving (Show)

data State = State
  { _stack :: [Addr]
  , _heap  :: Array Int (Maybe Node)
  , _free  :: [Int] -- free addresses
  , _ticks :: Int
  }

emptyState :: State
emptyState = State
  { _stack = []
  , _heap  = Array.listArray (1,1) [Nothing]
  , _free  = [1]
  , _ticks = 0
  }

instance Show State where
  show (State { _stack, _heap, _ticks }) =
    unlines $
      [ "Ticks = " ++ show _ticks
      , "Stack = ["
      ]
      ++
      map showStackItem _stack
      ++
      [ "]"
      , "Heap = ["
      ]
      ++
      mapMaybe showHeapItem (Array.assocs _heap)
      ++
      [ "]"
      , replicate 50 '-'
      ]
    where
      showStackItem addr@(Addr n) = 
        "  " ++ show addr ++ ": " ++ showNode (fromJust (_heap ! n))
      showNode node =
        case node of
          Application addr1 addr2 -> unwords [ "App", show addr1, show addr2 ]
          Function fun _ _        -> unwords [ "Fun", fun ]
          Number n                -> unwords [ "Num", show n ]
          Indirection addr        -> unwords [ "Ind", show addr ]
      showHeapItem (_, Nothing)   = Nothing
      showHeapItem (n, Just node) = Just $
        "  " ++ show (Addr n) ++ ": " ++ showNode node

newtype Mark1 a =
  Mark1 { getMark1 :: ExceptT String (RWS Environment String State) a }
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader Environment
           , MonadWriter String
           , MonadState State
           )

runMark1 :: Mark1 a -> (Either String a, String)
runMark1 mark1 = evalRWS (runExceptT (getMark1 mark1)) [] emptyState

stackSize :: Mark1 Int
stackSize = gets (length . _stack)

isEmpty :: Mark1 Bool
isEmpty = gets (null . _stack)

top :: Mark1 Addr
top = do
  stack <- gets _stack
  case stack of
    []  -> throwError "top: empty stack"
    x:_ -> return x

pop :: Mark1 Addr
pop = do
  stack <- gets _stack
  case stack of
    []   -> throwError "pop: empty stack"
    x:xs -> do
      modify $ \state -> state { _stack = xs }
      return x

push :: Addr -> Mark1 ()
push addr = modify $ \state -> state { _stack = addr : _stack state }

heapSize :: Mark1 Int
heapSize = gets (Array.rangeSize . Array.bounds . _heap)

doubleHeap :: Mark1 ()
doubleHeap =
  modify $ \state@(State { _heap, _free }) ->
    let (l,h) = Array.bounds _heap
        newHeap = Array.listArray (l,2*h) (Array.elems _heap ++ repeat Nothing)
        newFree = _free ++ [h+1 .. 2*h]
    in  state { _heap = newHeap, _free = newFree }

alloc :: Node -> Mark1 Addr
alloc node = do
  free <- gets _free
  case free of
    [] -> doubleHeap >> alloc node
    addr:newFree -> do
      modify $ \state@(State { _heap }) ->
        state
          { _heap = _heap // [(addr, Just node)]
          , _free = newFree
          }
      return (Addr addr)

isValid :: Addr -> Mark1 Bool
isValid (Addr n) = do
  heap <- gets _heap
  return $ Array.inRange (Array.bounds heap) n && isJust (heap ! n)

check :: String -> Addr -> Mark1 ()
check fun addr = do
  valid <- isValid addr
  unless valid $ throwError (fun ++ ": unknown address " ++ show addr)

deref :: Addr -> Mark1 Node
deref addr@(Addr n) = do
  check "deref" addr
  gets (fromJust . (! n) . _heap)

update :: Addr -> Node -> Mark1 ()
update addr@(Addr n) node = do
  check "update" addr
  modify $ \state@(State { _heap }) ->
    state { _heap = _heap // [(n, Just node)] }

free :: Addr -> Mark1 ()
free addr@(Addr n) = do
  valid <- isValid addr
  when valid $ modify $ \state@(State { _heap, _free }) ->
    state
      { _heap = _heap // [(n, Nothing)]
      , _free = n : _free
      }

tick :: Mark1 ()
tick = modify $ \state@(State { _ticks }) -> state { _ticks = _ticks + 1 }

executeFile :: String -> IO ()
executeFile file = readFile file >>= executeIO

executeIO :: String -> IO ()
executeIO code = do
  let (res, log) = execute code
  putStr log
  putStrLn $ case res of
    Left error -> "error: " ++ error
    Right n    -> "main = " ++ show n

execute :: String -> (Either String Integer, String)
execute code = runMark1 $
  case parse "<interactive>" code of
    Left error -> throwError (show error)
    Right program -> do
      globals <- compile program
      local (globals ++) run
      Number n <- top >>= deref
      return n

compile :: Program -> Mark1 Environment
compile program = do
  let definitions = program ++ prelude
  globals <-
    forM definitions $ \(fun, args, body) ->
      (,) fun <$> alloc (Function fun args body)
  case lookup "main" globals of
    Nothing -> throwError "compile: main is not defined"
    Just addr -> push addr
  return globals

isFinal :: Mark1 Bool
isFinal = do
  addr <- pop
  empty <- isEmpty
  push addr
  if empty then do
    node <- deref addr
    case node of
      Number _ -> return True
      _        -> return False
  else
    return False

dump :: Mark1 ()
dump = get >>= tell . show

run :: Mark1 ()
run = do
  over <- isFinal
  if over then dump else step >> run

step :: Mark1 ()
step = do
  dump
  tick
  node <- top >>= deref
  case node of
    Number _ -> throwError "step: number applied as function"
    Application addr _ -> push addr
    Function fun args body -> do
      let rethrowEmptyStack :: String -> Mark1 a
          rethrowEmptyStack _ =
            throwError $
              "step: function " ++ fun ++ " applied to too few arguments"
      bindings <-
        forM args $ \arg -> do
          _ <- pop
          addr <- top `catchError` rethrowEmptyStack
          param <- deref addr
          case param of
            Application _ addr2 -> return (arg, addr2)
            _ -> throwError "step: expected application node"
      _ <- pop `catchError` rethrowEmptyStack
      addr <- local (bindings ++) (instantiate body)
      push addr
    Indirection addr -> do
      _ <- pop
      push addr

instantiate :: Expr -> Mark1 Addr
instantiate body =
  case body of
    Num n -> alloc (Number n)
    Ap expr1 expr2 -> do
      addr1 <- instantiate expr1
      addr2 <- instantiate expr2
      alloc (Application addr1 addr2)
    Var x -> do
      look <- asks (lookup x)
      case look of
        Nothing -> throwError ("instantiate: undefined name " ++ x)
        Just addr -> return addr
    Let NonRecursive defs expr -> do
      bindings <-
        forM defs $ \(x, rhs) -> do
          addr <- instantiate rhs
          return (x, addr)
      local (bindings ++) (instantiate expr)
    Let Recursive defs expr -> do
      bindings <-
        forM defs $ \(x, _) -> do
          addr <- alloc (Number 0)
          return (x, addr)
      local (bindings ++) $ do
        forM_ (zip defs bindings) $ \((_, rhs), (_, root)) -> do
          addr <- instantiate rhs
          update root (Indirection addr)
        instantiate expr
    _ -> throwError ("instantiate: cannot instantiate " ++ show body)

module CoreLang.Language.Mark1 where

import Control.Monad.Except
import Control.Monad.RWS

import Data.Array (Array, (!), (//))
import qualified Data.Array as Array
import Data.Maybe (fromJust, isJust, mapMaybe)

import CoreLang.Language.Parser (parse)
import CoreLang.Language.Syntax

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
  deriving (Show)

data State = State
  { _stack :: [Addr]
  , _heap  :: Array Int (Maybe Node)
  , _free  :: [Int] -- free addresses
  }

emptyState :: State
emptyState = State
  { _stack = []
  , _heap  = Array.listArray (1,1) [Nothing]
  , _free  = [1]
  }

instance Show State where
  show (State { _stack, _heap }) =
    unlines $
      [ "Stack ["
      ]
      ++
      map showStackItem _stack
      ++
      [ "]"
      , "Heap ["
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
      -- showStackNode (Application addr1 addr2@(Addr n)) = unwords
      --   ["App"
      --   , show addr1
      --   , show addr2
      --   , "(" ++ showNode (fromJust (_heap ! n)) ++ ")"
      --   ]
      -- showStackNode node = showNode node
      showNode node =
        case node of
          Application addr1 addr2 -> unwords [ "App", show addr1, show addr2 ]
          Function fun _ _        -> unwords [ "Fun", fun ]
          Number n                -> unwords [ "Num", show n ]
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
  check "lookup" addr
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

run :: Mark1 ()
run = do
  over <- isFinal
  unless over (step >> run)

step :: Mark1 ()
step = do
  get >>= tell . show
  current <- top >>= deref
  case current of
    Number _ -> throwError "step: number applied as function"
    Application addr _ -> push addr
    Function _ args body -> do
      _ <- pop
      bindings <-
        forM args $ \arg -> do
          param <- pop >>= deref
          case param of
            Application _ addr2 -> return (arg, addr2)
            _ -> throwError "step: expected application node"
      local (bindings ++) (instantiate body) >>= push

instantiate :: Expr -> Mark1 Addr
instantiate body =
  case body of
    Num n -> alloc (Number n)
    Ap e1 e2 -> do
      addr1 <- instantiate e1
      addr2 <- instantiate e2
      alloc (Application addr1 addr2)
    Var x -> do
      look <- asks (lookup x)
      case look of
        Nothing -> throwError ("instantiate: undefined name " ++ x)
        Just addr -> return addr

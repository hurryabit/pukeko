module CoreLang.Language.Mark5 where

import Control.Monad.Except
import Control.Monad.RWS.Strict

import Data.Array (Array, (!), (//))
import qualified Data.Array as Array
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.Maybe (mapMaybe)

import CoreLang.Language.Parser (parse)
import CoreLang.Language.Syntax

type Environment = [(Identifier, Addr)]

newtype Addr = Addr Int

instance Show Addr where
  show (Addr n) = 
    let s = show n
        p = if length s < 4 then replicate (4-length s) '0' else ""
    in  '#' : p ++ s

builtins :: [(Identifier, Mark5 ())]
builtins =
  [ ("neg", stepArith1 negate)
  , ("+"  , stepArith2 (+))
  , ("-"  , stepArith2 (-))
  , ("*"  , stepArith2 (*))
  , ("/"  , stepArith2 div)
  , ("<"  , stepRel2 (<) )
  , ("<=" , stepRel2 (<=))
  , ("==" , stepRel2 (==))
  , ("!=" , stepRel2 (/=))
  , (">=" , stepRel2 (>=))
  , (">"  , stepRel2 (>) )
  , ("if"    , stepUnbool)
  , ("unpair", stepUnpair)
  , ("unlist", stepUnlist)
  , ("abort" , throwError "user aborted execution")
  , ("print" , stepPrint)
  ]

data Node
  = Free
  | Marked      Node
  | Application Addr Addr
  | Function    Identifier [Identifier] Expr
  | Number      Integer
  | Indirection Addr
  | Primitive   Identifier (Mark5 ())
  | Constructor Int Int
  | Data        Int [Addr]

step :: Mark5 ()
step = do
  dataStack <- isDataStack
  if dataStack then
    undump
  else do
    topAddr <- top
    topNode <- deref topAddr
    case topNode of
      Number _ -> throwError "step: number applied as function"
      Data _ _ -> throwError "step: data applied as function"
      Application addr1 addr2 -> do
        node2 <- deref addr2
        case node2 of
          Indirection addr3 -> update topAddr (Application addr1 addr3)
          _                 -> push addr1
      Function _ args body -> do
        bindings <-
          forM args $ \arg -> do
            _ <- pop
            Application _ argAddr <- top >>= deref
            return (arg, argAddr)
        root <- top
        local (bindings ++) (instantiateAndUpdate body root)
      Indirection addr -> do
        _ <- pop
        push addr
      Primitive _ primStep -> primStep
      Constructor t n -> do
        addrs <-
          replicateM n $ do
            _ <- pop
            Application _ addr <- top >>= deref
            return addr
        root <- top
        update root (Data t addrs)
      Marked _ -> throwError "marked node"
      Free     -> throwError "free node"

stepArith1 :: (Integer -> Integer) -> Mark5 ()
stepArith1 op = stepPrim1 $ \node ->
  Number <$> op <$> getNumber node

stepPrim1 :: (Node -> Mark5 Node) -> Mark5 ()
stepPrim1 impl = do
  _ <- pop
  appAddr <- top
  Application _ argAddr <- deref appAddr
  argNode <- deref argAddr
  if isDataNode argNode then do
    resNode <- impl argNode
    update appAddr resNode
  else do
    dump
    push argAddr

stepArith2 :: (Integer -> Integer -> Integer) -> Mark5 ()
stepArith2 op = stepPrim2 $ \node1 node2 ->
  Number <$> (op <$> getNumber node1 <*> getNumber node2)

stepRel2 :: (Integer -> Integer -> Bool) -> Mark5 ()
stepRel2 rel = stepPrim2 $ \node1 node2 ->
  mkBool <$> (rel <$> getNumber node1 <*> getNumber node2)

stepPrim2 :: (Node -> Node -> Mark5 Node) -> Mark5 ()
stepPrim2 impl = do
  _ <- pop
  app1Addr <- top
  Application _ arg1Addr <- deref app1Addr
  arg1Node <- deref arg1Addr
  if isDataNode arg1Node then do
    _ <- pop
    app2Addr <- top
    Application _ arg2Addr <- deref app2Addr
    arg2Node <- deref arg2Addr
    if isDataNode arg2Node then do
      resNode <- impl arg1Node arg2Node
      update app2Addr resNode
    else do
      dump
      push arg2Addr
  else do
    dump
    push arg1Addr

stepUnbool :: Mark5 ()
stepUnbool = do
  _ <- pop
  condAppAddr <- top
  Application _ condAddr <- deref condAppAddr
  condNode <- deref condAddr
  if isDataNode condNode then do
    _ <- pop -- condAppAddr
    thenAppAddr <- pop
    elseAppAddr <- top
    let rootAddr = elseAppAddr
    cond <- getBool condNode
    if cond then do
      Application _ thenAddr <- deref thenAppAddr
      update rootAddr (Indirection thenAddr)
    else do
      Application _ elseAddr <- deref elseAppAddr
      update rootAddr (Indirection elseAddr)
  else do
    dump
    push condAddr

stepUnpair :: Mark5 ()
stepUnpair = do
  _ <- pop
  pairAppAddr <- top
  Application _ pairAddr <- deref pairAppAddr
  pairNode <- deref pairAddr
  if isDataNode pairNode then do
    _ <- pop -- pairAppAddr
    funAppAddr <- top -- rootAddr
    Data 0 [arg1Addr, arg2Addr] <- return pairNode
    Application _ funAddr <- deref funAppAddr
    arg1AppAddr <- alloc (Application funAddr arg1Addr)
    update funAppAddr (Application arg1AppAddr arg2Addr)
  else do
    dump
    push pairAddr

stepUnlist :: Mark5 ()
stepUnlist = do
  _ <- pop
  listAppAddr <- top
  Application _ listAddr <- deref listAppAddr
  listNode <- deref listAddr
  if isDataNode listNode then do
    _ <- pop -- listAppAddr
    baseAppAddr <- pop
    stepAppAddr <- top -- rootAddr
    Data t argAddrs <- return listNode
    case (t, argAddrs) of
      (0, []) -> do
        Application _ baseAddr <- deref baseAppAddr
        update stepAppAddr (Indirection baseAddr)
      (1, [arg1Addr, arg2Addr]) -> do
        Application _ stepAddr <- deref stepAppAddr
        arg1AppAddr <- alloc (Application stepAddr arg1Addr)
        update stepAppAddr (Application arg1AppAddr arg2Addr)
      _ -> throwError "pattern match failed in stepUnlist"
  else do
    dump
    push listAddr

stepPrint :: Mark5 ()
stepPrint = do
  [] <- gets _dump
  _ <- pop
  outAppAddr <- top
  Application _ outAddr <- deref outAppAddr
  outNode <- deref outAddr
  if isDataNode outNode then do
    num <- getNumber outNode
    Mark5 $ liftIO $ print num
    _ <- pop -- outAppAddr
    retAppAddr <- top -- rootAddr
    Application _ retAddr <- deref retAppAddr
    update retAppAddr (Indirection retAddr)
  else do
    dump
    push outAddr

instantiate :: Expr -> Mark5 Addr
instantiate body =
  case body of
    Num n -> alloc (Number n)
    Ap expr1 expr2 -> do
      addr1 <- instantiate expr1
      addr2 <- instantiate expr2
      alloc (Application addr1 addr2)
    Var x -> do
      env    <- ask
      global <- gets _global
      case lookup x (env ++ global) of
        Nothing   -> throwError ("unknown variable " ++ x)
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
        forM_ (zip defs bindings) $ \((_, rhs), (_, addr)) -> do
          instantiateAndUpdate rhs addr
        instantiate expr
    Pack t n -> alloc (Constructor t n)
    Case _ _ -> throwError "instantiate: case ... of not implemented"
    Lam  _ _ -> throwError "instantiate: lambdas not implemented"

instantiateAndUpdate :: Expr -> Addr -> Mark5 ()
instantiateAndUpdate body target =
  case body of
    Num n -> update target (Number n)
    Ap expr1 expr2 -> do
      addr1 <- instantiate expr1
      addr2 <- instantiate expr2
      update target (Application addr1 addr2)
    Var x -> do
      Just addr <- asks (lookup x)
      update target (Indirection addr)
    Let NonRecursive defs expr -> do
      bindings <-
        forM defs $ \(x, rhs) -> do
          addr <- instantiate rhs
          return (x, addr)
      local (bindings ++) (instantiateAndUpdate expr target)
    Let Recursive defs expr -> do
      bindings <-
        forM defs $ \(x, _) -> do
          addr <- alloc (Number 0)
          return (x, addr)
      local (bindings ++) $ do
        forM_ (zip defs bindings) $ \((_, rhs), (_, addr)) -> do
          instantiateAndUpdate rhs addr
        instantiateAndUpdate expr target
    Pack t n -> update target (Constructor t n)
    Case _ _ -> throwError "instantiateAndUpdate: case ... of not implemented"
    Lam  _ _ -> throwError "instantiateAndUpdate: lambdas not implemented"

type Stack = [Addr]

data State = State
  { _stack  :: Stack
  , _dump   :: [Stack]
  , _heap   :: Array Int Node
  , _free   :: IntSet -- free addresses
  , _global :: Environment
  }

initState :: Int -> State
initState heapSize = 
  let _heap = Array.listArray (1,heapSize) (repeat Free)
  in  State
        { _stack  = []
        , _dump   = []
        , _heap
        , _free   = IntSet.fromAscList (Array.indices _heap)
        , _global = []
        }

instance Show State where
  show (state@State { _stack, _dump, _heap }) =
    List.intercalate "\n" $ concat $
      [ [ concat ["Heap Usage = ", show (heapUsage state), " / ", show (heapSize state)]
        , "Stack = ["
        ]
      , map showStackItem _stack
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
      showNode node =
        case node of
          Application addr1 addr2 -> unwords [ "App", show addr1, show addr2 ]
          Function    fun _ _     -> unwords [ "Fun", fun ]
          Number      n           -> unwords [ "Num", show n ]
          Indirection addr        -> unwords [ "Ind", show addr ]
          Primitive   fun _       -> unwords [ "Pri", fun ]
          Constructor t n         -> unwords [ "Con", show t, show n ]
          Data        t addrs     -> unwords $ "Dat" : show t : map show addrs
          Marked      node        -> "*" ++ showNode node ++ "*"
          Free                    -> ""
      showStackItem addr@(Addr n) =
        "  " ++ show addr ++ ": " ++ showNode (_heap ! n)
      showDumpItem stack = concat
        [ "  ["
        , unwords $ map show stack
        , "]"
        ]
      showHeapItem (_, Free) = Nothing
      showHeapItem (n, node) = Just $
        "  " ++ show (Addr n) ++ ": " ++ showNode node

data Stats = Stats
  { _ticks    :: !Int
  , _maxHeap  :: !Int
  , _gcRuns   :: !Int
  , _gcVolume :: !Int
  }

instance Show Stats where
  show (Stats { _ticks, _maxHeap, _gcRuns, _gcVolume }) =
    List.intercalate "; "
    [ "Ticks = "          ++ show _ticks
    , "Max Heap Usage = " ++ show _maxHeap
    , "GC Runs = "        ++ show _gcRuns
    , "GC Volume = "      ++ show _gcVolume
    ]

instance Monoid Stats where
  mempty = Stats { _ticks = 0, _maxHeap = 0, _gcRuns = 0, _gcVolume = 0 }
  s1 `mappend` s2 = 
    Stats
      { _ticks    = _ticks    s1 +     _ticks    s2
      , _maxHeap  = _maxHeap  s1 `max` _maxHeap  s2
      , _gcRuns   = _gcRuns   s1 +     _gcRuns   s2
      , _gcVolume = _gcVolume s1 +     _gcVolume s2
      }
  
newtype Mark5 a =
  Mark5 { getMark5 :: ExceptT String (RWST Environment Stats State IO) a }
  deriving ( Functor, Applicative
           , MonadError String
           , MonadReader Environment
           , MonadWriter Stats
           , MonadState State
           )

instance Monad Mark5 where
  return  = Mark5 . return
  m >>= f = Mark5 $ getMark5 m >>= getMark5 . f
  fail    = Mark5 . throwError

runMark5 :: Int -> Mark5 a -> IO (Either String a, State, Stats)
runMark5 heapSize mark1 =
  runRWST (runExceptT (getMark5 mark1)) [] (initState heapSize)

stackSize :: Mark5 Int
stackSize = gets (length . _stack)

isEmpty :: Mark5 Bool
isEmpty = gets (null . _stack)

top :: Mark5 Addr
top = do
  addr:_ <- gets _stack
  return addr

pop :: Mark5 Addr
pop = do
  addr:_stack <- gets _stack
  modify $ \state -> state { _stack }
  return addr

push :: Addr -> Mark5 ()
push addr = 
  modify $ \state@(State { _stack }) -> state { _stack = addr:_stack }

dump :: Mark5 ()
dump = modify $ \state@(State { _stack, _dump }) ->
  state { _stack = [], _dump = _stack : _dump }

undump :: Mark5 ()
undump = do
  _stack:_dump <- gets _dump
  modify $ \state -> state { _stack, _dump }
    
alloc :: Node -> Mark5 Addr
alloc Free = throwError "cannot use Free"
alloc node = do
  view <- gets (IntSet.minView . _free)
  case view of
    Nothing -> throwError "heap full"
    Just (addr, _free) -> do
      modify $ \state@(State { _heap }) ->
        state
          { _heap = _heap // [(addr, node)]
          , _free
          }
      usage <- gets heapUsage
      tell $ mempty { _maxHeap = usage }
      return (Addr addr)

deref :: Addr -> Mark5 Node
deref (Addr addr) = gets ((! addr) . _heap)

update :: Addr -> Node -> Mark5 ()
update _           Free = throwError "cannot use Free"
update (Addr addr) node = do
  modify $ \state@(State { _heap }) ->
    state { _heap = _heap // [(addr, node)] }

executeFile :: Int -> String -> IO ()
executeFile heapSize file = do
  let logfile = takeWhile (/= '.') file ++ ".log"
  code <- readFile file
  (res, state, stats) <- execute heapSize code
  case res of
    Left error -> do
      putStrLn $ "Error = " ++ error
    Right n    -> do
      print stats
      putStrLn $ "Result = " ++ show n
  writeFile logfile (show state)

executeIO :: Int -> String -> IO ()
executeIO heapSize code = do
  (res, state, stats) <- execute heapSize code
  case res of
    Left error -> do
      putStr (show state)
      putStrLn $ "Error = " ++ error
    Right n -> do
      print state
      print stats
      putStrLn $ "Result = " ++ show n

execute :: Int -> String -> IO (Either String Integer, State, Stats)
execute heapSize code = runMark5 heapSize $
  case parse "<interactive>" code of
    Left error -> throwError (show error)
    Right program -> do
      compile program
      run
      top >>= deref >>= getNumber

compile :: Program -> Mark5 ()
compile program = do
  functions <-
    forM (program ++ prelude) $ \(fun, args, body) ->
      (,) fun <$> alloc (Function fun args body)
  primitives <-
    forM builtins $ \(fun, prim) ->
       (,) fun <$> alloc (Primitive fun prim)
  Just addr <- return (lookup "main" functions)
  push addr
  modify $ \state ->
    state { _global = functions ++ primitives }
  return ()

isDataNode :: Node -> Bool
isDataNode node =
  case node of
    Number _ -> True
    Data _ _ -> True
    _        -> False

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

isDataStack :: Mark5 Bool
isDataStack =
  (&&) <$> (isDataNode <$> (top >>= deref)) <*> gets (isSingleton . _stack)

isFinal :: Mark5 Bool
isFinal = (&&) <$> isDataStack <*> gets (null . _dump)

run :: Mark5 ()
run = do
  over <- isFinal
  tell $ mempty { _ticks = 1 }
  unless over (gc >> step >> run)

getNumber :: Node -> Mark5 Integer
getNumber node = do
  Number num <- return node
  return num

mkBool :: Bool -> Node
mkBool bool = Data (fromEnum bool) []
  
getBool :: Node -> Mark5 Bool
getBool node = do
  Data t [] <- return node
  return $ toEnum t

heapSize, heapFree, heapUsage :: State -> Int
heapSize (State { _heap }) = Array.rangeSize (Array.bounds _heap)
heapFree (State { _free }) = IntSet.size _free
heapUsage = (-) <$> heapSize <*> heapFree

debugState :: Mark5 ()
debugState = do
  let sep = replicate 50 '='
  debug sep
  get >>= debug . show
  debug sep

debug :: String -> Mark5 ()
debug = Mark5 . liftIO . putStrLn

markFrom :: Addr -> Mark5 ()
-- markFrom :: Addr -> Mark5 Addr
markFrom addr = do
  node <- deref addr
  case node of
    Free     -> throwError "found Free"
    Marked _ -> return ()
    _        -> update addr (Marked node)
    -- Indirection _ -> return ()
    -- _             -> update addr (Marked node)
  case node of
    Free -> throwError "found Free"
    Application addr1 addr2 -> markFrom addr1 >> markFrom addr2
    Indirection addr        -> markFrom addr
    Data        _ addrs     -> forM_ addrs markFrom
    _                       -> return ()
    -- Marked _ -> return addr
    -- Application addr1 addr2 -> do
    --   addr1 <- markFrom addr1
    --   addr2 <- markFrom addr2
    --   update addr (Marked (Application addr1 addr2))
    --   return addr
    -- Indirection addr1 -> do
    --   addr1 <- markFrom addr1
    --   update addr (Indirection addr1)
    --   return addr1
    -- Data t addrs -> do
    --   addrs <- mapM markFrom addrs
    --   update addr (Marked (Data t addrs))
    --   return addr
    -- Number      _     -> return addr
    -- Function    _ _ _ -> return addr
    -- Primitive   _ _   -> return addr
    -- Constructor _ _   -> return addr

scan :: Mark5 ()
scan = do
  modify $ \state@(State { _heap, _free }) ->
    let isCollectible node =
          case node of
            Free     -> False
            Marked _ -> False
            _        -> True
        collectibleAddrs =
          [ addr | (addr, node) <- Array.assocs _heap, isCollectible node ]
        unmark node =
          case node of
            Marked node -> node
            _           -> Free
    in  state
          { _heap = fmap unmark _heap
          , _free = IntSet.union _free (IntSet.fromAscList collectibleAddrs)
          }
gc :: Mark5 ()
gc = do
  needed <- gets (\state -> 5 * heapFree state < heapSize state)
  when needed $ do
    oldUsage <- gets heapUsage
    stackRootAddrs <- gets _stack
    dumpRootAddrs <- gets (concat . _dump)
    globalRootAddrs <- gets (map snd . _global)
    let rootAddrs = concat
          [stackRootAddrs, dumpRootAddrs, globalRootAddrs]
    -- debug "before gc"
    -- debugState
    forM_ rootAddrs markFrom
    -- debug "between gc"
    -- debugState
    scan
    -- debug "after gc"
    -- debugState
    newUsage <- gets heapUsage
    tell $ mempty { _gcRuns = 1, _gcVolume = oldUsage - newUsage }
    debug $ "GC: " ++ show oldUsage ++ " -> " ++ show newUsage
    -- debugState

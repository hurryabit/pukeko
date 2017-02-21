module Pukeko.TempInst.Interpreter
   ( interpret
   , interpretFile
   , interpretProgram
   )
   where

import Text.Printf

import Pukeko.Language.Syntax (Program)
import Pukeko.TempInst.Instantiate
import Pukeko.TempInst.TIM

import qualified Pukeko.Language.Parser   as Parser
import qualified Pukeko.TempInst.Builtins as Builtins
import qualified Pukeko.TempInst.GC       as GC
import qualified Pukeko.TempInst.Stats    as Stats

interpretProgram :: Int -> Program -> ExceptT String IO (Integer, String)
interpretProgram heapSize program = do
  (res, state, stats) <- liftIO $ runTIM heapSize $ do
    load program
    run
    top >>= deref >>= getNumber
  case res of
    Left error -> throwError $ printf "%s\nError = %s" (show state) (show error)
    Right num  -> return (num, show stats)
  

interpretFile :: Int -> String -> IO ()
interpretFile heapSize file =
  readFile file >>= interpret' heapSize file

interpret :: Int -> String -> IO ()
interpret heapSize = interpret' heapSize "<interactive>"

interpret' :: Int -> String -> String -> IO ()
interpret' heapSize file code = do
  (res, state, stats) <- runTIM heapSize $ do
    case Parser.parseProgram file code of
      Left error -> throwError (show error)
      Right program -> do
        load program
        run
        top >>= deref >>= getNumber
  case res of
    Left error -> printf "%s\n%s\nError = %s\n" state (show stats) error
    Right num  -> printf "%s\nResult = %d\n" (show stats) num


load :: Program -> TIM ()
load program = do
  globals <-
    mapM (\(fun, node) -> (,) fun <$> alloc node) $ concat
      [ [ (fun, Function fun args body) | (fun, args, body) <- program               ]
      , [ (fun, Constructor tag arity)  | (fun, tag, arity) <- Builtins.constructors ]
      , [ (fun, Primitive fun tim)      | (fun, tim)        <- Builtins.primitives   ]
      ]
  put globals
  addr <- resolve "main"
  push addr


run :: TIM ()
run = do
  gc_needed <- (\free size -> 5 * free < size) <$> heapFree <*> heapSize
  when gc_needed $ do
    oldUsage <- heapUsage
    GC.run
    newUsage <- heapUsage
    tell $ Stats.gcRun <> Stats.gcVolume (oldUsage - newUsage)
  topNode <- top >>= deref
  size    <- stackSize
  noDump  <- dumpNull
  unless (isDataNode topNode && size == 1 && noDump) $ do
    step
    usage <- heapUsage
    tell $ Stats.tick <> Stats.heapUsage usage
    run

step :: TIM ()
step = do
  topAddr <- top
  topNode <- deref topAddr
  size <- stackSize
  if isDataNode topNode && size == 1 then
    undump
  else do
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
            pop
            Application _ argAddr <- top >>= deref
            return (arg, argAddr)
        root <- top
        local (bindings ++) (instantiateWithUpdate body root)
      Indirection addr -> do
        pop
        push addr
      Primitive _ primStep -> primStep
      Constructor t n -> do
        addrs <-
          replicateM n $ do
            pop
            Application _ addr <- top >>= deref
            return addr
        root <- top
        update root (Data t addrs)
      Marked _ -> throwError "marked node"

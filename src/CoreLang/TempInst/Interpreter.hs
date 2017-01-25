module CoreLang.TempInst.Interpreter
   ( interpret
   , interpretFile
   )
   where

import Text.Printf

import CoreLang.Language.Syntax (Program)
import CoreLang.TempInst.Instantiate
import CoreLang.TempInst.TIM

import qualified CoreLang.Language.Parser   as Parser
import qualified CoreLang.Language.Syntax   as Syntax
import qualified CoreLang.TempInst.Builtins as Builtins
import qualified CoreLang.TempInst.GC       as GC
import qualified CoreLang.TempInst.Stats    as Stats

interpretFile :: Int -> String -> IO ()
interpretFile heapSize file =
  readFile file >>= interpret' heapSize file

interpret :: Int -> String -> IO ()
interpret heapSize = interpret' heapSize "<interactive>"

interpret' :: Int -> String -> String -> IO ()
interpret' heapSize file code = do
  (res, state, stats) <- runTIM heapSize $ do
    case Parser.parse file code of
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
  functions <-
    forM (program ++ Syntax.prelude) $ \(fun, args, body) ->
      (,) fun <$> alloc (Function fun args body)
  primitives <-
    forM Builtins.everything $ \(fun, prim) ->
       (,) fun <$> alloc (Primitive fun prim)
  put (functions ++ primitives)
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

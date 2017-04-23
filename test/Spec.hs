import Data.List (sort)
import System.Directory (setCurrentDirectory)
import System.Exit (ExitCode (..))
import System.Process (callCommand, readProcessWithExitCode)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

render :: [Int] -> String
render = unlines . map show

runProg :: FilePath -> [Int] -> IO (ExitCode, String)
runProg prog input = do
  (exitCode, stdout, _) <- readProcessWithExitCode ("./" ++ prog) [] (render input)
  return (exitCode, stdout)

testBasic name input output =
  specify name $ runProg name input `shouldReturn` (ExitSuccess, render output)

prop_prog_correct :: Gen [Int] -> ([Int] -> [Int]) -> FilePath -> Property
prop_prog_correct gen spec prog = monadicIO $ do
  xs <- pick gen
  (exitCode, stdout) <- run $ runProg prog xs
  assert (exitCode == ExitSuccess)
  let ys = spec xs
  assert (stdout == render ys)

prop_sort_correct :: FilePath -> Property
prop_sort_correct =
  let gen = do
        n <- choose (0, 100)
        xs <- vectorOf n (choose (-1000, 1000))
        return (n:xs)
  in  prop_prog_correct gen (sort . tail)

-- testSort :: String -> Test
testSort name = specify name (prop_sort_correct name)

prop_function_correct :: Int -> (Int -> Int) -> FilePath -> Property
prop_function_correct bound spec =
  let gen = do
        n <- choose (0, bound)
        return [n]
  in  prop_prog_correct gen (\[x] -> [spec x])

-- testFunction :: String -> Int -> (Int -> Int) -> Test
testFunction name bound spec =
  specify name (prop_function_correct bound spec name)

spec_catalan :: Int -> Int
spec_catalan n =
  let p = 100000007
      sols = 1 : map (sum . zipWith (*) sols) (tail $ scanl (flip (:)) [] sols)
  in  fromInteger $ (sols !! n) `mod` p

spec_queens :: Int -> Int
spec_queens n =
  let diff [] _  = []
      diff xs [] = xs
      diff (x:xs) (y:ys) = case x `compare` y of
        LT -> x : diff xs (y:ys)
        EQ -> diff xs ys
        GT -> diff (x:xs) ys
      solve' [] = [[]]
      solve' (ks:kss) = do
        k <- ks
        qs <- solve' $ zipWith (\ls i -> diff ls [k-i, k, k+i]) kss [1..]
        return $ k:qs
      solve n = solve' (replicate n [1..n])
  in  length $ solve n

spec_fibs :: Int -> Int
spec_fibs n =
  let p = 1000000000039
      fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
  in  fromInteger $ (fibs !! n) `mod` p

spec_primes :: Int -> Int
spec_primes n =
  let sieve (p:ks) = p : sieve (filter ((0 /=) . (`mod` p)) ks)
      primes = 2 : 3 : sieve (scanl1 (+) (5 : cycle [2, 4]))
  in  primes !! n

compile :: IO ()
compile = do
  setCurrentDirectory "test"
  callCommand "make all"

main :: IO ()
main = hspec $ beforeAll_ compile $ do
  describe "test basics" $ do
    testBasic "monad_io" [3, 2]  (concat $ replicate 3 [2, 1, 0])
    testBasic "wildcard" [7, 13] [7, 13]
  describe "test sorting" $ mapM_ testSort
    [ "isort"
    , "qsort"
    , "tsort"
    ]
  describe "test number theory/combinatorics" $ do
    testFunction "fibs"    1000 spec_fibs
    testFunction "catalan"  100 spec_catalan
    testBasic    "queens"   [8] [92]
    testFunction "primes"   100 spec_primes

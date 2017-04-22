import Data.List (sort)
import System.Directory (setCurrentDirectory)
import System.Exit (ExitCode (..))
import System.Process (callCommand, readProcessWithExitCode)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

prop_prog_correct :: Gen [Int] -> ([Int] -> [Int]) -> FilePath -> Property
prop_prog_correct gen spec prog = monadicIO $ do
  xs <- pick gen
  let render = unlines . map show
  (exitCode, stdout, _) <- run $
    readProcessWithExitCode ("./" ++ prog) [] (render xs)
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

testSort :: String -> Test
testSort name = testProperty name (prop_sort_correct name)

prop_function_correct :: Int -> (Int -> Int) -> FilePath -> Property
prop_function_correct bound spec =
  let gen = do
        n <- choose (0, bound)
        return [n]
  in  prop_prog_correct gen (\[x] -> [spec x])

testFunction :: String -> Int -> (Int -> Int) -> Test
testFunction name bound spec =
  testProperty name (prop_function_correct bound spec name)

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

tests =
  [ testGroup "sort" $ map testSort
    [ "isort"
    , "qsort"
    , "tsort"
    ]
  , testGroup "functions"
    [ testFunction "fibs"    1000 spec_fibs
    , testFunction "catalan"  100 spec_catalan
    , testFunction "queens"    10 spec_queens
    , testFunction "primes"   100 spec_primes
    ]
  ]

main :: IO ()
main = do
  setCurrentDirectory "test"
  callCommand "make all"
  defaultMain tests

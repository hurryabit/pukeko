module Main where

import Pukeko.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Options.Applicative hiding (renderFailure)
import           System.Exit

import           Pukeko.AST.Name
import qualified Pukeko.BackEnd as BackEnd
import qualified Pukeko.FrontEnd as FrontEnd
import qualified Pukeko.FrontEnd.Parser as Parser
import qualified Pukeko.MiddleEnd as MiddleEnd
import           Pukeko.Pretty

type CompilerM = Eff [Error Failure, NameSource, IO]

data Options = Options
  { optInfile :: FilePath
  , optOutfile :: Maybe FilePath
  , optUnsafe :: Bool
  , optDetailed :: Bool
  }

writeFileString :: Options -> String -> CompilerM ()
writeFileString opts x =
  sendM $ maybe putStr writeFile (optOutfile opts) x

writeFilePretty :: Pretty a => Options -> a -> CompilerM ()
writeFilePretty opts x = writeFileString opts $ render (optDetailed opts) (pretty x) ++ "\n"

writeFileJSON :: Aeson.ToJSON a => Options -> a -> CompilerM ()
writeFileJSON opts x = do
  let config = Aeson.defConfig{Aeson.confIndent = Aeson.Spaces 2}
  sendM $ maybe BS.putStrLn BS.writeFile (optOutfile opts) $
    Aeson.encodePretty' config (Aeson.toJSON x)

runCompilerM :: CompilerM () -> IO ()
runCompilerM act =
  runM (runNameSource (runError act)) >>= \case
    Left err -> do
      putStrLn $ "Error: " ++ renderFailure err
      exitWith (ExitFailure 1)
    Right () -> exitSuccess

frontEnd :: Options -> CompilerM FrontEnd.Module
frontEnd opts = do
  package <- Parser.parsePackage (optInfile opts)
  FrontEnd.run (optUnsafe opts) package

middleEnd :: Options -> [MiddleEnd.Optimization] -> CompilerM MiddleEnd.Module
middleEnd opts optims = do
  module_sf <- frontEnd opts
  let cfg = MiddleEnd.Config optims (not (optUnsafe opts))
  MiddleEnd.run cfg module_sf


check :: Options -> IO ()
check opts = runCompilerM $
  frontEnd opts >>= writeFilePretty opts

core :: Options -> [MiddleEnd.Optimization] -> IO ()
core opts optims = runCompilerM $
  middleEnd opts optims >>= writeFilePretty opts . fst

bytecode :: Options -> [MiddleEnd.Optimization] -> IO ()
bytecode opts optims = runCompilerM $ do
  module_bc <- snd <$> middleEnd opts optims
  if optDetailed opts
    then writeFilePretty opts module_bc
    else writeFileJSON opts module_bc

compile :: Options -> [MiddleEnd.Optimization] -> IO ()
compile opts optims = runCompilerM $ do
  module_bc <- snd <$> middleEnd opts optims
  nasm <- BackEnd.run module_bc
  writeFileString opts nasm

options :: Parser Options
options = Options
  <$> strArgument
      (  metavar "INFILE"
      <> help "Read input from INFILE")
  <*> optional (strOption
      (  long "output"
      <> short 'o'
      <> metavar "OUTFILE"
      <> help "Write output to OUTFILE" ))
  <*> switch (long "unsafe" <> help "Don't run the type checker")
  <*> switch (long "detailed" <> help "Render output in detailed mode")

optimizations :: Parser [MiddleEnd.Optimization]
optimizations =
  option (maybeReader f)
  (long "optimize" <> value (MiddleEnd.optimizations MiddleEnd.defaultConfig))
  where
    f = traverse $ \case
      'E' -> Just MiddleEnd.EtaReduction
      'A' -> Just MiddleEnd.AliasInlining
      'I' -> Just MiddleEnd.Inlining
      'D' -> Just MiddleEnd.DeadCodeElimination
      'P' -> Just MiddleEnd.Prettification
      _   -> Nothing

cmds :: Parser (IO ())
cmds = hsubparser $ mconcat
  [ command "check"    (info (check    <$> options                  ) idm)
  , command "core"     (info (core     <$> options <*> optimizations) idm)
  , command "bytecode" (info (bytecode <$> options <*> optimizations) idm)
  , command "compile"  (info (compile  <$> options <*> optimizations) idm)
  ]

main :: IO ()
main = join $ execParser (info (cmds <**> helper) idm)

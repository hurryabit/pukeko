module Main where

import Pukeko.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BS
import           Options.Applicative hiding (renderFailure)
import           System.Exit
import           System.FilePath ((-<.>), (<.>))

import           Pukeko.AST.Name
import qualified Pukeko.BackEnd as BackEnd
import qualified Pukeko.FrontEnd as FrontEnd
import qualified Pukeko.FrontEnd.Parser as Parser
import qualified Pukeko.MiddleEnd as MiddleEnd
import           Pukeko.Pretty

type CompilerM = Eff [Error Failure, NameSource, IO]

data Options = Options
  { optUnsafe :: Bool
  , optDetailed :: Bool
  }

writeFilePretty :: Pretty a => Options -> FilePath -> a -> CompilerM ()
writeFilePretty opts file x =
  sendM $ writeFile file (render (optDetailed opts) (pretty x) ++ "\n")

writeFileJSON :: Aeson.ToJSON a => FilePath -> a -> CompilerM ()
writeFileJSON file x = do
  let config = Aeson.defConfig{Aeson.confIndent = Aeson.Spaces 2}
  sendM $ BS.writeFile file (Aeson.encodePretty' config (Aeson.toJSON x))

runCompilerM :: CompilerM () -> IO ()
runCompilerM act =
  runM (runNameSource (runError act)) >>= \case
    Left err -> do
      putStrLn $ "Error: " ++ renderFailure err
      exitWith (ExitFailure 1)
    Right () -> exitSuccess

frontEnd :: Options -> FilePath -> CompilerM FrontEnd.Module
frontEnd opts file = do
  package <- Parser.parsePackage file
  FrontEnd.run (optUnsafe opts) package

middleEnd :: Options -> [MiddleEnd.Optimization] -> FilePath -> CompilerM MiddleEnd.Module
middleEnd opts optims file = do
  module_sf <- frontEnd opts file
  let cfg = MiddleEnd.Config optims (not (optUnsafe opts))
  MiddleEnd.run cfg module_sf


check :: Options -> FilePath -> IO ()
check opts file = runCompilerM $
  frontEnd opts file >>= writeFilePretty opts (file -<.> "put")

core :: Options -> [MiddleEnd.Optimization] -> FilePath -> IO ()
core opts optims file = runCompilerM $
  middleEnd opts optims file >>= writeFilePretty opts (file -<.> "puc") . fst

bytecode :: Options -> [MiddleEnd.Optimization] -> FilePath -> IO ()
bytecode opts optims file = runCompilerM $ do
  module_bc <- snd <$> middleEnd opts optims file
  if optDetailed opts
    then writeFilePretty opts (file -<.> "pub" <.> "txt") module_bc
    else writeFileJSON (file -<.> "pub") module_bc

compile :: Options -> [MiddleEnd.Optimization] -> String -> IO ()
compile opts optims file = runCompilerM $ do
  module_bc <- snd <$> middleEnd opts optims file
  nasm <- BackEnd.run module_bc
  sendM $ writeFile (file -<.> "asm") nasm

options :: Parser Options
options = Options
  <$> switch (long "unsafe" <> help "Don't run the type checker")
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
  [ command "check"    (info (check    <$> options <*>                   pukekoFile) idm)
  , command "core"     (info (core     <$> options <*> optimizations <*> pukekoFile) idm)
  , command "bytecode" (info (bytecode <$> options <*> optimizations <*> pukekoFile) idm)
  , command "compile"  (info (compile  <$> options <*> optimizations <*> pukekoFile) idm)
  ]
  where
    pukekoFile = argument str (metavar "PUKEKO-FILE")

main :: IO ()
main = join $ execParser (info (cmds <**> helper) idm)

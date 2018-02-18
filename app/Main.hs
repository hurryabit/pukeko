module Main where

import Pukeko.Prelude

import Options.Applicative hiding (renderFailure)
import System.FilePath ((<.>), (-<.>))
import System.Exit
import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy     as BS
import qualified Data.Text.Lazy.IO        as T

import           Pukeko.AST.Name
import qualified Pukeko.FrontEnd.Parser as Parser
import qualified Pukeko.FrontEnd        as FrontEnd
import qualified Pukeko.MiddleEnd       as MiddleEnd
import qualified Pukeko.MiddleEnd.CallGraph as G
import qualified Pukeko.BackEnd         as BackEnd

compile :: Bool -> Bool -> Bool -> Bool -> Bool -> [MiddleEnd.Optimization] -> String -> IO ()
compile write_pl stop_tc unsafe json graph opts file = do
  ok_or_error <- runM . runNameSource . runError $ do
    package <- Parser.parsePackage file
    module_sf <- FrontEnd.run unsafe package
    if stop_tc
      then do
        sendM $ writeFile (file -<.> "ti")
          (render (pretty module_sf) ++ "\n")
      else do
        let cfg = MiddleEnd.Config opts (not unsafe)
        (module_pl, module_lm) <- MiddleEnd.run cfg module_sf
        nasm <- BackEnd.run module_lm
        sendM $ do
          when write_pl $ do
            writeFile (file -<.> "pl") $ render (pretty module_pl) ++ "\n"
          when json $ do
            let config = Aeson.defConfig{Aeson.confIndent = Aeson.Spaces 2}
            BS.writeFile (file -<.> "pl" <.> "json")
              (Aeson.encodePretty' config (Aeson.toJSON module_pl))
          when graph $ do
            T.writeFile (file -<.> "dot") $
              G.renderCallGraph (G.makeCallGraph module_pl)
          writeFile (file -<.> "asm") nasm
  case ok_or_error of
    Left err -> do
      putStrLn $ "Error: " ++ renderFailure err
      exitWith (ExitFailure 1)
    Right () -> exitWith ExitSuccess

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

options :: Parser (IO ())
options =
  compile
    <$> switch (short 'l' <> help "Write result of lambda lifter")
    <*> switch (short 't' <> help "Stop after type checking")
    <*> switch (short 'u' <> help "Don't run the type checker")
    <*> switch (short 'j' <> help "Write .pl.json file")
    <*> switch (short 'g' <> help "Save dependency graph of last stage")
    <*> optimizations
    <*> argument str (metavar "FILE")

main :: IO ()
main = join $ execParser (info (options <**> helper) idm)

import Prelude

import qualified Options.Applicative as Opt

import Data.Prune (parseStackYaml)

data Opts = Opts
  { optsStackYamlFile :: FilePath
  }

parseArgs :: IO Opts
parseArgs = Opt.execParser (Opt.info (Opt.helper <*> parser) $ Opt.progDesc "Prune a Stack project's dependencies")
  where
    parser = Opts
      <$> Opt.strOption (
        Opt.long "stack-yaml-file"
          <> Opt.metavar "STACK_YAML_FILE"
          <> Opt.help "Location of stack.yaml"
          <> Opt.value "stack.yaml"
          <> Opt.showDefault )

main :: IO ()
main = do
  Opts {..} <- parseArgs
  packages <- parseStackYaml optsStackYamlFile
  putStrLn $ show packages

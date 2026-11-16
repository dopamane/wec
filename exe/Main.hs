module Main (main) where

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import Data.Version
import Options.Applicative
import Paths_wec
import Sound.WEC

main :: IO ()
main = do
  CLI p nM oM fM <- customExecParser prefs' $ pinfo $ showVersion version
  r <- runGet getRiff <$> maybe BS.getContents BS.readFile fM
  let r' = maybe id wec nM r
  case oM of
    Nothing | p -> print r'
            | otherwise -> BS.putStr $ runPut $ putRiff r'
    Just o  | p -> writeFile o $ show r'
            | otherwise -> BS.writeFile o $ runPut $ putRiff r'

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $ progDesc "wav echo effect"

data CLI = CLI Bool (Maybe Int) (Maybe FilePath) (Maybe FilePath)

parser :: Parser CLI
parser = CLI <$> switch (short 'p' <> help "prettyprint")
  <*> optional (option auto $ short 'd' <> metavar "INT" <> help "Sample delay")
  <*> optional (strOption $ short 'o' <> metavar "FILE" <> completer (bashCompleter "file")
      <> help "File output otherwise STDOUT")
  <*> optional (strArgument $ metavar "FILE" <> completer (bashCompleter "file")
      <> help "Optional RIFF file otherwise STDIN")

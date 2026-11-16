module Main (main) where

import Data.Binary.Get
import qualified Data.ByteString.Lazy as BS
import Data.Version
import Options.Applicative
import Paths_wec
import Sound.WEC

main :: IO ()
main = do
  CLI fM <- customExecParser prefs' $ pinfo $ showVersion version
  print . runGet getRiff =<< maybe BS.getContents BS.readFile fM

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $ progDesc "wav echo effect"

data CLI = CLI (Maybe FilePath)

parser :: Parser CLI
parser = CLI <$> optional fileIn

fileIn :: Parser FilePath
fileIn = strArgument $ metavar "FILE" <> completer (bashCompleter "file")
  <> help "Optional RIFF file otherwise STDIN"

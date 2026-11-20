module Main (main) where

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import Data.Rap
import Data.Version
import Options.Applicative
import Paths_wec
import Sound.WEC

main :: IO ()
main = do
  CLI s d v oM fM <- customExecParser prefs' $ pinfo $ showVersion version
  r  <- runGet getRap <$> maybe BS.getContents BS.readFile fM
  r' <- either fail return $ wec d v r
  case oM of
    Nothing | s -> print r'
            | otherwise -> BS.putStr $ runPut $ putRap r'
    Just o  | s -> writeFile o $ show r'
            | otherwise -> BS.writeFile o $ runPut $ putRap r'

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $ progDesc "wav echo effect"

data CLI = CLI Bool Double Double (Maybe FilePath) (Maybe FilePath)

parser :: Parser CLI
parser = CLI <$> switch (short 's' <> help "show")
  <*> option auto (short 'd' <> metavar "DBL" <> help "Sample delay in seconds")
  <*> option auto (short 'v' <> metavar "DBL" <> help "Volume control percent")
  <*> optional (strOption $ short 'o' <> metavar "FILE" <> completer (bashCompleter "file")
      <> help "File output otherwise STDOUT")
  <*> optional (strArgument $ metavar "FILE" <> completer (bashCompleter "file")
      <> help "Optional RIFF file otherwise STDIN")

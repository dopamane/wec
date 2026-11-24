module Main (main) where

import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Int
import Data.Rap
import Data.Version
import Data.Word
import Options.Applicative
import Paths_wec
import Sound.Echo

main :: IO ()
main = do
  CLI s d v oM fM <- customExecParser prefs' $ pinfo $ showVersion version
  when (v < 0 || v > 100) $ fail $ "volume decay out of range: " ++ show v
  r  <- runGet getRap <$> maybe BS.getContents BS.readFile fM
  rs <- one "wave" [rs | Stanza "RIFF" "WAVE" rs <- r]
  (fmt, sr) <- join $ one "fmt" [(,) fmt <$> readSampleRate bs | fmt@(Verse "fmt " bs) <- rs]
  let n = round $ fromIntegral sr * d
  samples <- one "data" [runGet (many getInt16le) bs | Verse "data" bs <- rs]
  let samples' = take (length samples + n * 3) $ echo n 3 0.5 $ map norm samples
      r' = [Stanza "RIFF" "WAVE"
           [fmt, Verse "data" $ runPut $ foldMap (putInt16le . denorm) samples']]
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
  <*> option auto (short 'v' <> metavar "DBL" <> help "Volume decay percent")
  <*> optional (strOption $ short 'o' <> metavar "FILE" <> completer (bashCompleter "file")
      <> help "File output otherwise STDOUT")
  <*> optional (strArgument $ metavar "FILE" <> completer (bashCompleter "file")
      <> help "Optional RIFF file otherwise STDIN")

readSampleRate :: ByteString -> IO Word32
readSampleRate bs =
  case runGetOrFail (getWord16le *> getWord16le *> getWord32le) bs of
    Left  (_, _, er) -> fail $ "readSampleRate: " ++ er
    Right (_, _, sr) -> return sr

one :: String -> [a] -> IO a
one lbl vs = case vs of
  []  -> fail $ "no " ++ lbl ++ " found"
  [v] -> return v
  _   -> fail $ "more than one " ++ lbl ++ " found"

denorm :: Double -> Int16
denorm d
  | d > 0 = fromIntegral $ min (maxb :: Integer) $ round (d * maxb)
  | d < 0 = fromIntegral $ max (minb :: Integer) $ negate $ round (d * minb)
  | otherwise = 0

maxb :: Num a => a
maxb = fromIntegral (maxBound :: Int16)

minb :: Num a => a
minb = fromIntegral (minBound :: Int16)

norm :: Int16 -> Double
norm i
  | i > 0 = min   1  $ fromIntegral i / maxb
  | i < 0 = max (-1) $ negate $ fromIntegral i / minb
  | otherwise = 0

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
  (s, t, p, c, iM, oM) <- customExecParser prefs' $
    info (parser <**> simpleVersioner (showVersion version) <**> helper) $
    progDesc "wav echo effect"
  r' <- either fail return . wec t p c =<< maybe BS.getContents BS.readFile iM
  case oM of
    Nothing | s -> print r'
            | otherwise -> BS.putStr $ runPut $ putRap r'
    Just o  | s -> writeFile o $ show r'
            | otherwise -> BS.writeFile o $ runPut $ putRap r'

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

parser :: Parser (Bool, Double, Double, Int, Maybe FilePath, Maybe FilePath)
parser = (,,,,,) <$> switch (short 's' <> help "show")
  <*> option auto (short 't' <> metavar "DBL" <> help "delay seconds")
  <*> option auto (short 'p' <> metavar "DBL" <> help "decay percent")
  <*> option auto (short 'c' <> metavar "INT" <> help "count")
  <*> optional (strOption $ short 'i' <> metavar "FILE" <> completer (bashCompleter "file")
      <> help "input wav file otherwise STDIN")
  <*> optional (strOption $ short 'o' <> metavar "FILE" <> completer (bashCompleter "file")
      <> help "output wav file otherwise STDOUT")

wec :: Double -> Double -> Int -> ByteString -> Either String Rap
wec t p c i
  | t < 0 = Left $ "delay seconds must be non-negative: " ++ show t
  | p < 0 || p > 100 = Left $ "decay percent out of range: " ++ show p
  | c < 0 = Left $ "echo count must be non-negative: " ++ show c
  | otherwise = do
    r <- case runGetOrFail getRap i of
      Left  (_, _, e) -> Left $ "getRap: " ++ e
      Right (_, _, a) -> Right a
    rs <- one "wave" [rs | Stanza "RIFF" "WAVE" rs <- r]
    (fmt, sr) <- join $ one "fmt" [(,) fmt <$> readSampleRate bs | fmt@(Verse "fmt " bs) <- rs]
    let n = round $ fromIntegral sr * t
    samples <- one "data" [runGet (many getInt16le) bs | Verse "data" bs <- rs]
    let n' = length samples + n * c
    when (fromIntegral n' > (maxBound :: Word32)) $ Left $ "echo sample count too large: " ++ show n'
    let samples' = take n' $ echo n c (1 - p / 100) $ map norm samples
    return [Stanza "RIFF" "WAVE"
           [fmt, Verse "data" $ runPut $ foldMap (putInt16le . denorm) samples']]

readSampleRate :: ByteString -> Either String Word32
readSampleRate bs =
  case runGetOrFail (getWord16le *> getWord16le *> getWord32le) bs of
    Left  (_, _, er) -> Left $ "readSampleRate: " ++ er
    Right (_, _, sr) -> Right sr

one :: String -> [a] -> Either String a
one lbl vs = case vs of
  []  -> Left $ "no " ++ lbl ++ " found"
  [v] -> Right v
  _   -> Left $ "more than one " ++ lbl ++ " found"

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

module Sound.WEC (wec) where

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import Data.Int
import Data.Rap

-- | wav echo
wec
  :: Double -- ^ delay seconds
  -> Double -- ^ volume percent
  -> Rap -> Either String Rap
wec d v r
  | v < 0 || v > 100 = Left $ "volume " ++ show v ++ "% out of bounds"
  | otherwise = do
    rs <- one "wave" [rs | Stanza "RIFF" "WAVE" rs <- r]
    (fmt, sr) <- join $ one "fmt" [(,) fmt <$> readSampleRate bs | fmt@(Verse "fmt " bs) <- rs]
    let n = round $ fromIntegral sr * d
    bs <- one "data" [bs | Verse "data" bs <- rs]
    return [Stanza "RIFF" "WAVE" [fmt, Verse "data" $ echo n v bs]]

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

echo
  :: Int    -- ^ delay sample count
  -> Double -- ^ volume percent
  -> ByteString -> ByteString
echo n v = runPut . foldMap putInt16le . echo' n v . runGet (many getInt16le)

echo' :: Int -> Double -> [Int16] -> [Int16]
echo' n v xs = zipWith satSum (xs ++ replicate n 0) (replicate n 0 ++ map (volume v) xs)

volume :: Double -> Int16 -> Int16
volume v s = round $ fromIntegral s * v / 100

satSum :: Int16 -> Int16 -> Int16
satSum x y = clip $ fromIntegral x + fromIntegral y

clip :: Integer -> Int16
clip x
  | x > fromIntegral (maxBound :: Int16) = maxBound
  | x < fromIntegral (minBound :: Int16) = minBound
  | otherwise = fromIntegral x

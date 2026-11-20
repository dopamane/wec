module Sound.WEC (wec) where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import Data.Int
import Data.Rap

-- | wav echo
wec
  :: Double -- ^ delay seconds
  -> Rap -> Either String Rap
wec d r = do
  (fmt, sr) <- fmtSampleRate r
  let n = round $ fromIntegral sr * d
  bs <- dataBytes r
  return [Stanza "RIFF" "WAVE" [fmt, Verse "data" $ dmix n bs]]

fmtSampleRate :: Rap -> Either String (Riff, Word32)
fmtSampleRate r =
  case [(fmt, bs) | Stanza "RIFF" "WAVE" s <- r
                  , fmt@(Verse "fmt " bs)  <- s] of
  []          -> Left "no fmt found"
  [(fmt, bs)] -> (,) fmt <$> readSampleRate bs
  _           -> Left "more than one fmt found"

readSampleRate :: ByteString -> Either String Word32
readSampleRate bs =
  case runGetOrFail (getWord16le *> getWord16le *> getWord32le) bs of
    Left  (_, _, er) -> Left $ "getSampleRate: " ++ er
    Right (_, _, sr) -> Right sr

dataBytes :: Rap -> Either String ByteString
dataBytes r =
  case [bs | Stanza "RIFF" "WAVE" s <- r, Verse "data" bs <- s] of
    []   -> Left "no data found"
    [bs] -> Right bs
    _    -> Left "more than one data found"

dmix
  :: Int -- ^ delay sample count
  -> ByteString -> ByteString
dmix n = runPut . foldMap putInt16le . delayMix n . runGet (many getInt16le)

delayMix :: Int -> [Int16] -> [Int16]
delayMix n xs = zipWith satSum (xs ++ replicate n 0) (replicate n 0 ++ xs)

satSum :: Int16 -> Int16 -> Int16
satSum x y = clip $ fromIntegral x + fromIntegral y

clip :: Integer -> Int16
clip x
  | x > fromIntegral (maxBound :: Int16) = maxBound
  | x < fromIntegral (minBound :: Int16) = minBound
  | otherwise = fromIntegral x

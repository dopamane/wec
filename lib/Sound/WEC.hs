module Sound.WEC (wec, Riff, getRiff, putRiff, Ck(..)) where

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Int

type Riff = [Ck]

getRiff :: Get Riff
getRiff = many get

putRiff :: Riff -> Put
putRiff = foldMap put

data Ck = Ck{ckID :: String, ckSz :: Word32, ckTy :: String
  , ckDat :: Either Riff ByteString}
  deriving (Eq, Read, Show)

instance Binary Ck where
  get = do
    cid <- BSC.unpack <$> getLazyByteString 4
    csz <- getWord32le
    if cid == "RIFF" || cid == "LIST"
      then Ck cid csz <$> fmap BSC.unpack (getLazyByteString 4) <*> fmap Left getRiff
      else Ck cid csz "" . Right <$> getLazyByteString (fromIntegral csz) <* when (odd csz) (void getWord8)

  put (Ck cid csz cty cdat) = do
    putLazyByteString $ BSC.pack cid
    putWord32le csz
    putLazyByteString $ BSC.pack cty
    case cdat of
      Left  r -> putRiff r
      Right d -> putLazyByteString d >> when (odd csz) (putWord8 0)

-- | wav echo
wec
  :: Double -- ^ delay seconds
  -> Riff -> Either String Riff
wec d r = do
  cs  <- wavChunks r
  fmt <- findChunk "fmt " cs
  sr  <- readSampleRate fmt
  let n = round $ fromIntegral sr * d
  bs  <- readBytes =<< findChunk "data" cs
  return $ mkwav fmt $ mkdat $ dmix n bs

mkwav :: Ck -> Ck -> Riff
mkwav fmt dat = [Ck "RIFF" len "WAVE" $ Left [fmt, dat]]
  where
    len = sum
      [ 4 -- WAVE ID
      , 8 + ckSz fmt
      , 8 + ckSz dat]

wavChunks :: Riff -> Either String Riff
wavChunks r = do
  riff <- findChunk "RIFF" r
  if ckTy riff /= "WAVE"
    then Left $ "Expecting WAVE type but got " ++ ckTy riff
    else case ckDat riff of
      Right _ -> Left "Invalid RIFF structure, expecting LIST but got bytes"
      Left cs -> Right cs

mkdat :: ByteString -> Ck
mkdat bs = Ck "data" (fromIntegral $ BS.length bs) "" $ Right bs

readSampleRate :: Ck -> Either String Word32
readSampleRate fmt = do
  fbs <- readBytes fmt
  case runGetOrFail (getWord16le *> getWord16le *> getWord32le) fbs of
    Left (_, _, er) -> Left $ "getSampleRate: " ++ er
    Right (_, _, sr) -> return sr

findChunk :: String -> Riff -> Either String Ck
findChunk cid r = case filter ((cid ==) . ckID) r of
  [] -> Left $ cid ++ " chunk not found"
  [ck] -> Right ck
  _ -> Left $ "More than one " ++ cid ++ " chunk found"

readBytes :: Ck -> Either String ByteString
readBytes Ck{ckID=cid, ckDat=dat} = case dat of
  Left _ -> Left $ "Invalid " ++ cid ++ " structure, expecting bytes but got LIST"
  Right bs -> Right bs

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

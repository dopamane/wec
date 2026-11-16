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

wec :: Int -> Riff -> Riff
wec n [riff@Ck{ckID="RIFF", ckDat=Left[fmt, dat@Ck{ckID="data", ckSz=datSz, ckDat=Right bs}]}
      ] = [riff{ckDat=Left[fmt, dat{ckDat=Right bs'}]}]
  where
    bs' = BS.take (fromIntegral datSz) $ dmix n bs
wec _ _ = error "invalid format"

dmix :: Int -> ByteString -> ByteString
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

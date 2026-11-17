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

wec :: Int -> Riff -> Either String Riff
wec n r = case filter (("RIFF" ==) . ckID) r of
  [] -> Left "RIFF chunk not found"
  [riff]
    | ckTy riff == "WAVE" -> case ckDat riff of
      Left cs -> do
       fmt <- case filter (("fmt " ==) . ckID) cs of
         [] -> Left "fmt chunk not found"
         [fmt] -> Right fmt
         _ -> Left "More than one fmt chunk found"
       case filter (("data" ==) . ckID) cs of
         [] -> Left "data chunk not found"
         [Ck{ckSz=datSz, ckDat=dat}] -> case dat of
           Left _ -> Left "Invalid data structure, expecting bytes but got list"
           Right bs -> do
             let bs' = BS.take (fromIntegral datSz) $ dmix n bs
             return [riff{ckDat=Left[fmt, mkdat bs']}]
         _ -> Left "More than one data chunk found"
      Right _ -> Left "Invalid RIFF structure, expecting format and data but got bytes"
    | otherwise -> Left $ "Expecting WAVE type but got " ++ ckTy riff
  _ -> Left "More than one RIFF chunk found"

mkdat :: ByteString -> Ck
mkdat bs = Ck "data" (fromIntegral $ BS.length bs) "" $ Right bs

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

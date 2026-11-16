module Sound.WEC (wec, Riff, getRiff, putRiff, Ck(..)) where

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC

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
wec _ = id

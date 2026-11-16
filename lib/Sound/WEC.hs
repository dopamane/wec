module Sound.WEC (wec, Riff, Ck(..)) where

import Data.ByteString.Lazy (ByteString)
import Data.Word

type Riff = [Ck]

data Ck = Ck{ckID :: String, ckSz :: Word32, ckDat :: Either Riff ByteString}
  deriving (Eq, Read, Show)

wec :: Int -> Riff -> Riff
wec _ = id

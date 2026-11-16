module Sound.WEC (wec, Riff(..)) where

data Riff = Riff
  deriving (Eq, Read, Show)

wec :: Int -> Riff -> Riff
wec _ = id

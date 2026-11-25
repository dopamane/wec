module Main (main) where

import Sound.Echo
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Sound.Echo"
  [ testCase "empty" $ echo 0 0 0 [] @?= []
  , testCase "id" $ echo 5 0 1 [1, 1, 1] @?= [1, 1, 1]
  , testCase "single" $ echo 2 1 1 [1] @?= [1, 0, 1]
  , testCase "multi" $ echo 3 2 1 [0.25, 1] @?= [0.25, 1, 0, 0.25, 1, 0, 0.25, 1]
  , testCase "infinite" $ take 10 (echo 2 1 0.5 [1..]) @?= replicate 10 1
  , testCase "amps" $ echo 2 2 0.5 [1] @?= [1, 0, 0.5, 0, 0.25]
  , testCase "sum" $ echo 1 1 1 [0.5, 0.5] @?= [0.5, 1, 0.5]
  , testCase "clip" $ echo 1 1 1 [0.75, 0.75] @?= [0.75, 1, 0.75]
  , testCase "overlap" $ echo 0 1 1 [0.5, 0.25] @?= [1, 0.5]
  ]

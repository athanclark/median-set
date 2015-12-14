module Data.MedianSpec (spec) where

import qualified Data.Median     as M
import Data.List (sort)

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Instances


spec :: TestTree
spec = testGroup "Data.Median"
  [ QC.testProperty "always finds median" isMedian
  ]


-- newtype OddList

isMedian :: [Int] -> Property
isMedian xs =
  let len = length xs
  in  len > 0 && odd len ==>
      let m = foldr M.insert (M.singleton $ head xs) (tail xs)
      in  M.findMedian m == (sort xs) !! (floor $ fromIntegral len / 2)

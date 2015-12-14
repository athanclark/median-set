{-# LANGUAGE
    OverloadedStrings
  #-}

module Main where

import Data.Median (MedianSet)
import qualified Data.Median as M
import Criterion.Main


type Key = Integer
type Content = Integer


insertBench :: Int -> Benchmark
insertBench x = bench (show x) $
  whnf (\acc -> foldr M.insert acc [1..x]) (M.singleton 0)

main :: IO ()
main =
  defaultMain
    [ bgroup "insert" $
        insertBench <$> [10,20..100]
    ]

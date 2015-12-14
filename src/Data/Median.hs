{-# LANGUAGE
    BangPatterns
  #-}

module Data.Median where

import qualified Data.MultiSet as M



data MedianSet a = MedianSet
  { medianLeft  :: !(M.MultiSet a)
  , medianValue :: !a
  , medianCount :: {-# UNPACK #-} !Int
  , medianRight :: !(M.MultiSet a)
  } deriving (Show, Eq, Ord)


singleton :: a -> MedianSet a
singleton x = MedianSet M.empty x 1 M.empty

size :: MedianSet a -> Int
size (MedianSet ls _ i rs) = i + M.size ls + M.size rs


insert :: Ord a => a -> MedianSet a -> MedianSet a
insert x (MedianSet ls y i rs)
  | x == y = (MedianSet ls y $! i+1) rs
  | x <  y = if (M.size ls + 1) - i > M.size rs
             then let (y',ls') = M.deleteFindMax $! M.insert x ls
                      rs'      = M.insertMany y i rs
                      j        = M.occur y' ls'
                  in ((MedianSet $! M.deleteAll y' ls') y' $! j+1) rs'
             else (MedianSet $! M.insert x ls) y i rs
  | x >  y = if M.size ls < (M.size rs+1) - i
             then let (y',rs') = M.deleteFindMin $! M.insert x rs
                      ls'      = M.insertMany y i ls
                      j        = M.occur y' rs'
                  in (MedianSet ls' y' $! j+1) $! M.deleteAll y' rs'
             else MedianSet ls y i $! M.insert x rs
  | otherwise = error "Somehow non-exhaustive pattern match"

{-# INLINEABLE insert #-}


findMedian :: MedianSet a -> a
findMedian = medianValue

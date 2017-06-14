module Main where

import Data.LeftishHeap 
import Data.Heap.Class

fromList' :: Ord a => [a] -> LeftishHeap a
fromList' = head . go . (map singleton)
  where
    go []       = [empty]
    go [x1]     = [x1]
    go [x1,x2]  = [x1 `merge` x2]
    go (x1:x2:xs) = go $ (x1 `merge` x2) : go xs
  -- https://drive.google.com/file/d/0B6eWGr-ow12sMjczNzFlM2MtNjYyOC00Yzc4LWFjMmQtNDkxNDRhNDM2MzM0/view
  -- http://d.hatena.ne.jp/nowokay/20090106
main = do
  print $ fromList' [1, 30, 5, 4, 3, 20, 11, 40]

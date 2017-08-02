module Data.Queue.Class
  where

class Queue a where
  empty :: a b
  isEmpty :: a b -> Bool
  snoc :: a b -> b -> a b
  head :: a b -> b
  tail :: a b -> a b

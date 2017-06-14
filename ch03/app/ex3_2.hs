module Main where
import Data.Heap.Class
import Data.LeftishHeap

-- Tests
import Control.DeepSeq (force)
import Data.List (foldl')
import Test.Hspec
import Test.Hspec.QuickCheck

insert' :: (Ord a) => a -> LeftishHeap a -> LeftishHeap a
insert' x E = singleton x
insert' x h@(H _ y l r)
  | x <= y    = makeT x E h
  | otherwise = makeT y l (insert' x r)

-- Class instance
newtype Ex3_2 a = Ex3_2 { unEx3_2 :: LeftishHeap a} deriving (Show)

instance Heap Ex3_2 where
    empty   = Ex3_2 empty
    isEmpty (Ex3_2 w)   = isEmpty w
    insert  x (Ex3_2 w) = Ex3_2 $ insert' x w
    merge   (Ex3_2 w1) (Ex3_2 w2) = Ex3_2 $ merge w1 w2
    findMin (Ex3_2 w)   = findMin w 
    deleteMin (Ex3_2 w) = Ex3_2 $ deleteMin w
    
-- Specs and tests
ex3_2_spec = do
    describe "ex3 is correct leftish heap" $
        prop "is balanced" (prop_balanced :: [Int] -> Bool)
  where
    prop_balanced = inv_leftish . unEx3_2 . fromList

ex3_2_tests = do
    heap_spec (T :: T (Ex3_2 Int))
    ex3_2_spec
    -- TODO: reuse leftish heap spec
    --    leftish_heap_spec (T :: T (Ex3_2 Int))
 
pinsert :: (Heap x, Ord a, Show (x a)) => a -> IO (x a) -> IO (x a) 
pinsert x h = do
  h' <- h
  let z = insert x h'
  print z
  return z

-- main = hspec $ do
--   ex3_2_tests
main = do
  let x = Ex3_2 $ singleton (1 :: Int)
  let y = Ex3_2 $ singleton (10 :: Int)
  pinsert 4 $pinsert 3 $ pinsert 5 $ pinsert 8 $ return (merge x y)

  let x = singleton (1 :: Int)
  let y = singleton (10 :: Int)
  pinsert 4 $pinsert 3 $ pinsert 5 $ pinsert 8 $ return (merge x y)


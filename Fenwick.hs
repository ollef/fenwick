module Fenwick where

import Data.Monoid

data Tree a
  = Leaf a
  | Node (Tree a) !Int a (Tree a)
  deriving (Eq, Ord, Show)

balanceNode :: Monoid a => Tree a -> Int -> Tree a -> Int -> Tree a
balanceNode l llen r rlen
  | rlen > llen * 2 = rotateLeft l llen r
  | llen > rlen * 2 = rotateRight l llen r
  | otherwise = Node l llen (treeValue l <> treeValue r) r

rotateLeft :: Monoid a => Tree a -> Int -> Tree a -> Tree a
rotateLeft l llen r@(Leaf a) = Node l llen (treeValue l <> a) r
rotateLeft l llen (Node m mlen mr r) = Node (Node l llen lm m) lmlen (lval <> mr) r
  where
    lmlen = llen + mlen
    lval = treeValue l
    lm = lval <> treeValue m

rotateRight :: Monoid a => Tree a -> Int -> Tree a -> Tree a
rotateRight l@(Leaf a) llen r = Node l llen (a <> treeValue r) r
rotateRight (Node l llen lm m) lmlen r = Node l llen (lm <> rval) (Node m mlen mr r)
  where
    mlen = lmlen - llen
    rval = treeValue r
    mr = treeValue m <> rval

treeValue :: Tree a -> a
treeValue (Node _ _ a _) = a
treeValue (Leaf a) = a

data Fenwick a
  = Empty
  | Fenwick (Tree a) !Int
  deriving (Eq, Ord, Show)

showStructure :: Fenwick a -> String
showStructure Empty = "[empty]"
showStructure (Fenwick t _) = go t
  where
    go (Leaf _) = "x"
    go (Node l _ _ r) = "(" ++ go l ++ go r ++ ")"

value :: Monoid a => Fenwick a -> a
value Empty = mempty
value (Fenwick t _) = treeValue t

singleton :: a -> Fenwick a
singleton a = Fenwick (Leaf a) 1

insert :: Monoid a => Int -> a -> Fenwick a -> Fenwick a
insert 0 element Empty = Fenwick (Leaf element) 1
insert _ _ Empty = error "insert: index out of bounds"
insert index element (Fenwick rootTree rootLen)
  = Fenwick (go 0 rootTree rootLen) (rootLen + 1)
  where
    go leftIndex other@(Leaf a) _
      | index == leftIndex = Node (Leaf element) 1 (element <> a) other
      | index == leftIndex + 1 = Node other 1 (a <> element) (Leaf element)
      | otherwise = error "insert: index out of bounds"
    go leftIndex (Node left leftLen _ right) len
      | index < rightIndex = do
        let left' = go leftIndex left leftLen
        balanceNode left' (leftLen + 1) right rightLen
      | otherwise = do
        let right' = go rightIndex right rightLen
        balanceNode left leftLen right' (rightLen + 1)
      where
        rightLen = len - leftLen
        rightIndex = leftIndex + leftLen

prefix :: Monoid a => Int -> Fenwick a -> a
prefix 0 Empty = mempty
prefix _ Empty = error "prefix: index out of bounds"
prefix index (Fenwick _ rootLen) | index > rootLen = error "prefix: index out of bounds"
prefix index (Fenwick rootTree rootLen) = go 0 rootTree rootLen
  where
    go leftIndex (Leaf a) _
      | index == leftIndex = mempty
      | index == leftIndex + 1 = a
      | otherwise = error "prefix: index out of bounds"
    go leftIndex (Node left leftLen a right) len
      | index == leftIndex + len = a
      | index < leftIndex + leftLen = go leftIndex left leftLen
      | otherwise = treeValue left <> go rightIndex right rightLen
      where
        rightLen = len - leftLen
        rightIndex = leftIndex + leftLen

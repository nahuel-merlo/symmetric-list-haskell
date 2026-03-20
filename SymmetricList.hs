module SymmetricList where

import Prelude hiding (head, tail, last, dropWhile)
import qualified Data.List as Lst

{-- 
    SymmetricList (xs, ys) = xs ++ reverse ys
    Invariants over SymmetricLists:
    null xs = single ys || null ys
    null ys = single xs || null xs
--}

type SymmetricList a = ([a], [a])

init , fromList :: [a] -> SymmetricList a
init = fromList
fromList l = (xs, Lst.reverse ys) 
    where
        (xs, ys) = Lst.splitAt (Lst.length l `div` 2) l

fromSymetricList :: SymmetricList a -> [a]
fromSymetricList (xs, ys) = xs ++ reverse ys

nil :: SymmetricList a
nil = ([], [])

cons :: a -> SymmetricList a -> SymmetricList a
cons x (xs, ys)
    | Lst.null ys = (ys, [x])
    | otherwise   = (x:xs, ys)

snoc :: a -> SymmetricList a -> SymmetricList a
snoc y (xs, ys)
    | Lst.null xs = (ys, [y])
    | otherwise   = (xs, y:ys)

head :: SymmetricList a -> a
head ([], [])    = error "head on empty symmetric list"
head ((x:xs), _) = x
head (_, ys)     = (Lst.head . Lst.reverse) ys

last :: SymmetricList a -> a
last ([], [])    = error "last on empty symmetric list"
last (_, (y:ys)) = y
last (xs, _)     = (Lst.head . Lst.reverse) xs

tail :: SymmetricList a -> SymmetricList a
tail ([], [])  = error "tail on empty symmetric list"
tail ([], [y]) = nil
tail (xs, ys)
    | (length xs) == 1 = (reverse vs, us)
    | otherwise        = (Lst.tail xs, ys)
    where
        (us, vs) = Lst.splitAt ((Lst.length ys) `div` 2) ys

dropWhile :: (a -> Bool) -> SymmetricList a -> SymmetricList a
dropWhile p sl@(xs, ys) 
    | p $ Lst.head xs = dropWhile p (tail sl)
    | otherwise       = sl

null :: SymmetricList a -> Bool
null ([], []) = True
null _        = False

single :: SymmetricList a -> Bool
single ([x], []) = True
single ([], [y]) = True
single _         = False

{-- Test values --}
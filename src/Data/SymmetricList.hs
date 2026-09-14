module Data.SymmetricList (
    SymmetricList,
    fromList,
    toList,
    nil,
    cons,
    snoc,
    head,
    last,
    tail,
    dropWhile,
    null,
    single
) where

import Prelude hiding (head, tail, last, dropWhile, init, null)
import qualified Data.List as Lst

{-- 
    SymmetricList (xs, ys) = xs ++ reverse ys
    Invariants over SymmetricLists:
    null xs = single ys || null ys
    null ys = single xs || null xs
--}

newtype SymmetricList a = SL ([a], [a]) 

init , fromList :: [a] -> SymmetricList a
init = fromList
fromList l = SL (xs, Lst.reverse ys) 
    where
        (xs, ys) = Lst.splitAt (Lst.length l `div` 2) l

toList :: SymmetricList a -> [a]
toList (SL (xs, ys)) = xs ++ reverse ys

nil :: SymmetricList a
nil = SL ([], [])

cons :: a -> SymmetricList a -> SymmetricList a
cons x (SL (xs, ys))
    | Lst.null ys = SL (ys, [x])
    | otherwise   = SL (x:xs, ys)

snoc :: a -> SymmetricList a -> SymmetricList a
snoc y (SL (xs, ys))
    | Lst.null xs = SL (ys, [y])
    | otherwise   = SL (xs, y:ys)

head :: SymmetricList a -> a
head (SL ([], []))    = error "head on empty symmetric list"
head (SL ((x:_), _)) = x
head (SL (_, [y]))     = y

last :: SymmetricList a -> a
last (SL ([], []))    = error "last on empty symmetric list"
last (SL (_, (y:_))) = y
last (SL ([x], _))     = x

tail :: SymmetricList a -> SymmetricList a
tail (SL ([], []))  = error "tail on empty symmetric list"
tail (SL ([], [_])) = nil
tail (SL (xs, ys))
    | (length xs) == 1 = SL (reverse vs, us)
    | otherwise        = SL (Lst.tail xs, ys)
    where
        (us, vs) = Lst.splitAt ((Lst.length ys) `div` 2) ys

dropWhile :: (a -> Bool) -> SymmetricList a -> SymmetricList a
dropWhile p sl@(SL (xs, _)) 
    | p $ Lst.head xs = dropWhile p (tail sl)
    | otherwise       = sl

null :: SymmetricList a -> Bool
null (SL ([], [])) = True
null _           = False

single :: SymmetricList a -> Bool
single (SL ([_], [])) = True
single (SL ([], [_])) = True
single _            = False

{-- Instances --}

instance Show a => Show (SymmetricList a) where
    -- show (SL l) = show l
    show sl = show $ toList sl 
    -- show (SL (xs, ys)) = ("<- ") ++ (show xs) ++ (" | ") ++ (show $ reverse ys) ++ (" ->") 

instance Functor SymmetricList where
    fmap f (SL (xs, ys)) = SL ([f x | x <- xs], [f y | y <- ys]) 

instance Foldable SymmetricList where
    foldr f i (SL (xs, ys)) = foldr f (foldr f i (reverse ys)) xs

instance Applicative SymmetricList where
    pure x = init [x]
    (<*>) (SL (fsLeft, fsRight)) (SL (xsLeft, xsRight)) = SL ([ f x | f <- fsLeft, x <- xsLeft], [ f x | f <- fsRight, x <- xsRight]) 
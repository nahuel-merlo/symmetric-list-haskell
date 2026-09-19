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
    single,
    reverse,
    map,
    uncons,
    unsnoc
) where

import Prelude hiding (head, tail, last, dropWhile, init, null, reverse, map)
import qualified Data.List as Lst

{-- 
    SymmetricList (xs, ys) = xs ++ reverse ys
    Invariants over SymmetricLists:
    null xs = single ys || null ys
    null ys = single xs || null xs
--}

data SymmetricList a = SL !Int ([a], [a]) 

fromList :: [a] -> SymmetricList a
fromList l = SL (Lst.length l) (xs, Lst.reverse ys) 
    where
        (xs, ys) = Lst.splitAt (Lst.length l `div` 2) l

toList :: SymmetricList a -> [a]
toList (SL _ (xs, ys)) = xs ++ Lst.reverse ys

nil :: SymmetricList a
nil = SL 0 ([], [])

cons :: a -> SymmetricList a -> SymmetricList a
cons x (SL len (xs, ys))
    | Lst.null ys = SL (len+1) (ys, [x])
    | otherwise   = SL (len+1) (x:xs, ys)

snoc :: a -> SymmetricList a -> SymmetricList a
snoc y (SL len (xs, ys))
    | Lst.null xs = SL (len+1) (ys, [y])
    | otherwise   = SL (len+1) (xs, y:ys)

head :: SymmetricList a -> a
head (SL _ ([], []))    = error "head on empty symmetric list"
head (SL _ ((x:_), _)) = x
head (SL _ (_, [y]))     = y

last :: SymmetricList a -> a
last (SL _ ([], []))    = error "last on empty symmetric list"
last (SL _ (_, (y:_))) = y
last (SL _ ([x], _))     = x

tail :: SymmetricList a -> SymmetricList a
tail (SL _ ([], []))  = error "tail on empty symmetric list"
tail (SL _ ([], [_])) = nil
tail (SL len (xs, ys))
    | Lst.length xs == 1 = SL (len-1) (Lst.reverse vs, us)
    | otherwise        = SL (len-1) (Lst.tail xs, ys)
    where
        (us, vs) = Lst.splitAt ((Lst.length ys) `div` 2) ys

init :: SymmetricList a -> SymmetricList a
init (SL _ ([], [])) = error "init on empty symmetric list"
init (SL _ ([], [_])) = nil
init (SL len (xs, [y])) = let (us, vs) = Lst.splitAt ((Lst.length xs) `div` 2) xs in SL (len-1) (Lst.reverse vs, us)
init (SL len (xs, _:ys)) =  SL (len-1) (xs, ys)

dropWhile :: (a -> Bool) -> SymmetricList a -> SymmetricList a
dropWhile _ (SL _ ([], [])) = nil
dropWhile p sl
    | p $ head sl = dropWhile p (tail sl)
    | otherwise       = sl

null :: SymmetricList a -> Bool
null (SL _ ([], [])) = True
null _           = False

single :: SymmetricList a -> Bool
single (SL _ ([_], [])) = True
single (SL _ ([], [_])) = True
single _            = False

reverse :: SymmetricList a -> SymmetricList a
reverse (SL len (xs, ys)) = SL len (ys, xs)

map :: (a -> b) -> SymmetricList a -> SymmetricList b 
map = fmap

uncons :: SymmetricList a -> Maybe (a, SymmetricList a) 
uncons (SL _ ([], [])) = Nothing
uncons (SL len (x:xs, ys)) = Just (x, SL (len-1) (xs,ys))
uncons (SL _ ([], [y])) = Just (y, nil)

unsnoc :: SymmetricList a -> Maybe (SymmetricList a, a)
unsnoc (SL _ ([], [])) = Nothing 
unsnoc (SL len (xs, y:ys)) = Just (SL (len-1) (xs, ys), y)
unsnoc (SL _ ([x], [])) = Just (nil, x)

{-- Instances --}

instance Show a => Show (SymmetricList a) where
    -- show (SL _ l) = show l
    show sl = show $ toList sl 
    -- show (SL len (xs, ys)) = ("<- ") ++ (show xs) ++ (" | ") ++ (show $ reverse ys) ++ (" ->") 

instance Eq a => Eq (SymmetricList a) where
    (==) (SL _ (l, r)) (SL _ (l', r')) = l == l' && r == r'
    
instance Ord a => Ord (SymmetricList a) where
    (<=) (SL _ (l, r)) (SL _ (l', r')) = l <= l' && Lst.reverse r <= Lst.reverse r'

instance Functor SymmetricList where
    fmap f (SL len (xs, ys)) = SL len ([f x | x <- xs], [f y | y <- ys]) 

instance Foldable SymmetricList where
    foldr f i (SL _ (xs, ys)) = foldr f (foldr f i (Lst.reverse ys)) xs
    length (SL len _) = len

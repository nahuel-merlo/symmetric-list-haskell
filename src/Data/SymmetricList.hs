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
    (++),
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

data SymmetricList a = SL !Int !Int ([a], [a]) 

fromList :: [a] -> SymmetricList a
fromList l = SL lenL lenR (xs, Lst.reverse ys) 
    where
        len = Lst.length l
        lenL = (len `div` 2)
        lenR = len - lenL
        (xs, ys) = Lst.splitAt lenL l

toList :: SymmetricList a -> [a]
toList (SL _ _ (xs, ys)) = xs Lst.++ Lst.reverse ys

nil :: SymmetricList a
nil = SL 0 0 ([], [])

cons :: a -> SymmetricList a -> SymmetricList a
cons x (SL lenL lenR (xs, ys))
    | Lst.null ys = SL (lenL+1) lenR (ys, [x])
    | otherwise   = SL (lenL+1) lenR (x:xs, ys)

snoc :: a -> SymmetricList a -> SymmetricList a
snoc y (SL lenL lenR (xs, ys))
    | Lst.null xs = SL (lenL) (lenR+1) (ys, [y])
    | otherwise   = SL (lenL) (lenR+1) (xs, y:ys)

head :: SymmetricList a -> a
head (SL _ _ ([], []))    = error "head on empty symmetric list"
head (SL _ _ ((x:_), _)) = x
head (SL _ _ (_, [y]))     = y

last :: SymmetricList a -> a
last (SL _ _ ([], []))    = error "last on empty symmetric list"
last (SL _ _ (_, (y:_))) = y
last (SL _ _([x], _))     = x

tail :: SymmetricList a -> SymmetricList a
tail (SL _ _ ([], []))  = error "tail on empty symmetric list"
tail (SL _ _([], [_])) = nil
tail (SL _ lenR ([_], ys)) = SL newLenL newLenR (Lst.reverse vs, us)
    where
        newLenR = (lenR) `div` 2
        newLenL = lenR - newLenR
        (us, vs) = Lst.splitAt (newLenR) ys
tail (SL lenL lenR (_:xs, ys)) = SL (lenL-1) lenR (xs, ys)    

init :: SymmetricList a -> SymmetricList a
init (SL _ _ ([], [])) = error "init on empty symmetric list"
init (SL _ _ ([], [_])) = nil
init (SL lenL lenR (xs, [_])) = SL newLenL newLenR (us, Lst.reverse vs)
    where 
        newLenL = (lenL `div` 2)
        newLenR = lenL - newLenL
        (us, vs) = Lst.splitAt newLenL xs
init (SL lenL lenR (xs, _:ys)) =  SL lenL (lenR-1) (xs, ys)

dropWhile :: (a -> Bool) -> SymmetricList a -> SymmetricList a
dropWhile _ (SL _ _ ([], [])) = nil
dropWhile p sl
    | p $ head sl = dropWhile p (tail sl)
    | otherwise       = sl

null :: SymmetricList a -> Bool
null (SL _ _ ([], [])) = True
null _           = False

single :: SymmetricList a -> Bool
single (SL _ _ ([_], [])) = True
single (SL _ _ ([], [_])) = True
single _            = False

reverse :: SymmetricList a -> SymmetricList a
reverse (SL lenL lenR (xs, ys)) = SL lenR lenL (ys, xs)

map :: (a -> b) -> SymmetricList a -> SymmetricList b 
map = fmap

uncons :: SymmetricList a -> Maybe (a, SymmetricList a) 
uncons (SL _ _ ([], [])) = Nothing
uncons (SL lenL lenR (x:xs, ys)) = Just (x, SL (lenL-1) lenR (xs,ys))
uncons (SL _ _ ([], [y])) = Just (y, nil)

unsnoc :: SymmetricList a -> Maybe (SymmetricList a, a)
unsnoc (SL _ _ ([], [])) = Nothing 
unsnoc (SL lenL lenR (xs, y:ys)) = Just (SL lenL (lenR-1) (xs, ys), y)
unsnoc (SL _ _ ([x], [])) = Just (nil, x)

(++) :: SymmetricList a -> SymmetricList a -> SymmetricList a
(++) l@(SL lenL lenR (xs, ys)) r@(SL lenL' lenR' (xs', ys')) 
    | null l = r
    | null r = l
    | single l && single r = SL 1 1 (xs Lst.++ ys, ys' Lst.++ xs')
    | single l = SL (1+lenL') lenR'  (xs Lst.++ (ys Lst.++ xs'), ys')
    | single r = SL lenL (lenR+1) (xs, (ys' Lst.++ xs') Lst.++ ys)
    | length l <= length r = SL (lenL + lenR + lenL') lenR' (xs Lst.++ (ys `revAppend` xs'), ys')
    | otherwise = SL lenL (lenR' + lenL' + lenR) (xs, ys' Lst.++ (xs' `revAppend` ys))
    where
        revAppend :: [a] -> [a] -> [a]
        revAppend []     acc = acc
        revAppend (z:zs) acc = revAppend zs (z:acc)

{-- Instances --}

instance Show a => Show (SymmetricList a) where
    -- show (SL _ l) = show l
    show sl = show $ toList sl 
    -- show (SL len (xs, ys)) = ("<- ") ++ (show xs) ++ (" | ") ++ (show $ reverse ys) ++ (" ->") 

instance Eq a => Eq (SymmetricList a) where
    (==) (SL _ _ (l, r)) (SL _ _ (l', r')) = l == l' && r == r'
    
instance Ord a => Ord (SymmetricList a) where
    (<=) (SL _ _ (l, r)) (SL _ _ (l', r')) = l <= l' && Lst.reverse r <= Lst.reverse r'

instance Functor SymmetricList where
    fmap f (SL lenL lenR (xs, ys)) = SL lenL lenR ([f x | x <- xs], [f y | y <- ys]) 

instance Foldable SymmetricList where
    foldr f i (SL _ _ (xs, ys)) = foldr f (foldr f i (Lst.reverse ys)) xs
    length (SL lenL lenR _) = lenL + lenR

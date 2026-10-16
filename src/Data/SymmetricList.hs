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
    singleton,
    reverse,
    map,
    (++),
    uncons,
    unsnoc,
) where

import Prelude hiding (head, tail, last, dropWhile, init, null, reverse, map, filter, (++))
import qualified Data.List as Lst

{-- 
    SymmetricList (xs, ys) = xs ++ reverse ys
    Invariants over SymmetricLists:
    null xs = single ys || null ys
    null ys = single xs || null xs
--}

data SymmetricList a = SL !Int !Int ([a], [a]) 

-- | \(\mathcal{O}(n)\). Returns the symmetric representation of the list
--  
-- > xs == (toList . fromList) xs
fromList :: [a] -> SymmetricList a
fromList l = SL lenL lenR (xs, Lst.reverse ys) 
    where
        len = Lst.length l
        lenL = (len `div` 2)
        lenR = len - lenL
        (xs, ys) = Lst.splitAt lenL l

-- | \(\mathcal{O}(n)\). Returns the list representation of the symmetric lits
--
-- > (toList . fromList) xs == xs
toList :: SymmetricList a -> [a]
toList (SL _ _ (xs, ys)) = xs Lst.++ Lst.reverse ys

-- | \(\mathcal{O}(n)\). Splits the elements evenly between the left and right lists
--
-- > toList (balance sl) == toList sl
balance :: SymmetricList a -> SymmetricList a
balance (SL lenL lenR (xs, ys)) = (SL newLenL newLenR (us, Lst.reverse vs))
    where 
        totalLen = lenL + lenR
        newLenL = (totalLen `div` 2)
        newLenR = totalLen - newLenL
        (us, vs) = Lst.splitAt newLenL (xs Lst.++ (Lst.reverse ys))

-- | \(\mathcal{O}(1)\). Returns an empty symmetric list
--
-- On lists: @[]@ is \(\mathcal{O}(1)\).
--
-- > toList nil == []
nil :: SymmetricList a
nil = SL 0 0 ([], [])

-- | \(\mathcal{O}(1)\). Adds an element to the front of a symmetric list.
--
-- On lists: @(:)@ is \(\mathcal{O}(1)\).
--
-- > toList (cons x (fromList xs)) == x:xs
cons :: a -> SymmetricList a -> SymmetricList a
cons x (SL lenL lenR (xs, ys))
    | Lst.null ys = SL 1 (lenL)  ([x], xs)
    | otherwise   = SL (lenL+1) lenR (x:xs, ys)

-- | \(\mathcal{O}(1)\). Adds an element to the end of a symmetric list.
--
-- __Faster than lists:__ @xs ++ [x]@ is \(\mathcal{O}(n)\).
--
-- > toList (snoc x (fromList xs)) == xs ++ [x]
snoc :: a -> SymmetricList a -> SymmetricList a
snoc y (SL lenL lenR (xs, ys))
    | Lst.null xs = SL (lenR) 1 (ys, [y])
    | otherwise   = SL (lenL) (lenR+1) (xs, y:ys)

-- | \(\mathcal{O}(1)\). Returns the first element of a non-empty symmetric list.
-- Fails on an empty symmetric list.
--
-- On lists: 'Prelude.head' from "Prelude" is \(\mathcal{O}(1)\).
--
-- > head (fromList xs) == Prelude.head xs
head :: SymmetricList a -> a
head (SL _ _ ([], []))    = error "head on empty symmetric list"
head (SL _ _ ((x:_), _)) = x
head (SL _ _ (_, [y]))     = y

-- | \(\mathcal{O}(1)\). Returns the last element of a non-empty symmetric list.
-- Fails on an empty symmetric list.
--
-- __Faster than lists:__ 'Prelude.last' from "Prelude" is \(\mathcal{O}(n)\).
--
-- > last (fromList xs) == Prelude.last xs
last :: SymmetricList a -> a
last (SL _ _ ([], []))    = error "last on empty symmetric list"
last (SL _ _ (_, (y:_))) = y
last (SL _ _([x], _))     = x

-- | \(\mathcal{O}(1)\) amortized, \(\mathcal{O}(n)\) worst case. Removes the first element of a
-- non-empty symmetric list. Fails on an empty symmetric list.
--
-- On lists: 'Prelude.tail' from "Prelude" is \(\mathcal{O}(1)\).
--
-- > toList (tail (fromList xs)) == Prelude.tail xs
tail :: SymmetricList a -> SymmetricList a
tail (SL _ _ ([], []))  = error "tail on empty symmetric list"
tail (SL _ _([], [_])) = nil
tail (SL _ lenR ([_], ys)) = balance (SL 0 lenR ([], ys))
tail (SL lenL lenR (_:xs, ys)) = SL (lenL-1) lenR (xs, ys)    

-- | \(\mathcal{O}(1)\) amortized, \(\mathcal{O}(n)\) worst case. Removes the last element of a
-- non-empty symmetric list. Fails on an empty symmetric list.
--
-- __Faster than lists:__ 'Prelude.init' from "Prelude" is \(\mathcal{O}(n)\).
--
-- > toList (init (fromList xs)) == Prelude.init xs
init :: SymmetricList a -> SymmetricList a
init (SL _ _ ([], [])) = error "init on empty symmetric list"
init (SL _ _ ([], [_])) = nil
init (SL lenL _ (xs, [_])) = balance (SL lenL 0 (xs, []))
init (SL lenL lenR (xs, _:ys)) =  SL lenL (lenR-1) (xs, ys)

-- | \(\mathcal{O}(n)\). Removes the longest prefix whose elements satisfy the predicate.
--
-- On lists: 'Prelude.dropWhile' from "Prelude" is \(\mathcal{O}(n)\).
--
-- > toList (dropWhile p (fromList xs)) == Prelude.dropWhile p xs
dropWhile :: (a -> Bool) -> SymmetricList a -> SymmetricList a
dropWhile _ (SL _ _ ([], [])) = nil
dropWhile p sl
    | p $ head sl = dropWhile p (tail sl)
    | otherwise       = sl

-- | \(\mathcal{O}(1)\). Checks whether a symmetric list is empty.
--
-- On lists: 'Prelude.null' from "Prelude" is \(\mathcal{O}(1)\).
--
-- > null (fromList xs) == Prelude.null xs
null :: SymmetricList a -> Bool
null (SL _ _ ([], [])) = True
null _           = False

-- | \(\mathcal{O}(1)\). Checks whether a symmetric list has exactly one element.
--
-- > single (fromList xs) == (length xs == 1)
single :: SymmetricList a -> Bool
single (SL _ _ ([_], [])) = True
single (SL _ _ ([], [_])) = True
single _            = False

-- | \(\mathcal{O}(1)\). Returns a symmetric list with a single element.
--
-- On lists: 'Data.List.singleton' from "Data.List" is \(\mathcal{O}(1)\).
--
-- > toList (singleton x) == [x]
singleton :: a -> SymmetricList a  
singleton x = cons x nil

-- | \(\mathcal{O}(1)\). Reverses a symmetric list by swapping its left and right lists.
--
-- __Faster than lists:__ 'Prelude.reverse' from "Prelude" is \(\mathcal{O}(n)\).
--
-- > toList (reverse (fromList xs)) == Prelude.reverse xs
reverse :: SymmetricList a -> SymmetricList a
reverse (SL lenL lenR (xs, ys)) = SL lenR lenL (ys, xs)

-- | \(\mathcal{O}(n)\). Applies a function to every element of a symmetric list.
--
-- On lists: 'Prelude.map' from "Prelude" is \(\mathcal{O}(n)\).
--
-- > toList (map f (fromList xs)) == Prelude.map f xs
map :: (a -> b) -> SymmetricList a -> SymmetricList b 
map = fmap

-- | \(\mathcal{O}(1)\) amortized, \(\mathcal{O}(n)\) worst case. Splits a symmetric list into its first
-- element and the rest, or returns 'Nothing' if it is empty.
--
-- On lists: 'Data.List.uncons' from "Data.List" is \(\mathcal{O}(1)\).
--
-- > fmap (fmap toList) (uncons (fromList xs)) == Data.List.uncons xs
uncons :: SymmetricList a -> Maybe (a, SymmetricList a) 
uncons (SL _ _ ([], [])) = Nothing
uncons (SL _ lenR ([x], ys)) = Just (x, balance (SL 0 lenR ([], ys)))
uncons (SL lenL lenR (x:xs, ys)) = Just (x, SL (lenL-1) lenR (xs,ys))
uncons (SL _ _ ([], [y])) = Just (y, nil)

-- | \(\mathcal{O}(1)\) amortized, \(\mathcal{O}(n)\) worst case. Splits a symmetric list into all its
-- elements but the last one and the last element, or returns 'Nothing' if it
-- is empty.
--
-- __Faster than lists:__ 'Data.List.unsnoc' from "Data.List" is \(\mathcal{O}(n)\).
--
-- > fmap (\(sl, x) -> (toList sl, x)) (unsnoc (fromList xs)) == Data.List.unsnoc xs
unsnoc :: SymmetricList a -> Maybe (SymmetricList a, a)
unsnoc (SL _ _ ([], [])) = Nothing 
unsnoc (SL lenL _ (xs, [y])) = Just (balance (SL lenL 0 (xs, [])), y)
unsnoc (SL lenL lenR (xs, y:ys)) = Just (SL lenL (lenR-1) (xs, ys), y)
unsnoc (SL _ _ ([x], [])) = Just (nil, x)

-- | \(\mathcal{O}(\min(m, n))\), where m and n are the lengths of the symmetric lists.
-- Appends two symmetric lists, traversing only the shorter one.
--
-- __Faster than lists:__ '(Prelude.++)' from "Prelude" is \(\mathcal{O}(m)\), since it traverses the whole first list.
--
-- > toList (fromList xs ++ fromList ys) == xs Prelude.++ ys
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
    show (SL _ _ l) = show l
    -- show sl = show $ toList sl 
    -- show (SL len (xs, ys)) = ("<- ") ++ (show xs) ++ (" | ") ++ (show $ reverse ys) ++ (" ->") 

instance Eq a => Eq (SymmetricList a) where
    (==) (SL _ _ (l, r)) (SL _ _ (l', r')) = l == l' && r == r'
    
instance Ord a => Ord (SymmetricList a) where
    (<=) (SL _ _ (l, r)) (SL _ _ (l', r')) = l <= l' && Lst.reverse r <= Lst.reverse r'

instance Functor SymmetricList where
    fmap f (SL lenL lenR (xs, ys)) = SL lenL lenR ([f x | x <- xs], [f y | y <- ys]) 

-- | Folds over the elements from front to back.
--
-- 'length' is \(\mathcal{O}(1)\), since the lengths of both lists are stored.
--
-- __Faster than lists:__ 'Prelude.length' from "Prelude" is \(\mathcal{O}(n)\).
--
-- > length (fromList xs) == Prelude.length xs
instance Foldable SymmetricList where
    foldr f i (SL _ _ (xs, ys)) = foldr f (foldr f i (Lst.reverse ys)) xs
    length (SL lenL lenR _) = lenL + lenR

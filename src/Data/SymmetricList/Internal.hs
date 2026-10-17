module Data.SymmetricList.Internal (
    SymmetricList(..),
    balance,
    revAppend,
) where

import qualified Data.List as Lst
import Control.DeepSeq

{--
    SymmetricList (xs, ys) = xs ++ reverse ys
    Invariants over SymmetricLists:
    null xs = single ys || null ys
    null ys = single xs || null xs
--}

data SymmetricList a = SL !Int !Int ([a], [a])

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

-- | \(\mathcal{O}(n)\), where n is the length of the first list. Reverses the first list onto the second.
--
-- > revAppend xs ys == Prelude.reverse xs Prelude.++ ys
revAppend :: [a] -> [a] -> [a]
revAppend []     acc = acc
revAppend (z:zs) acc = revAppend zs (z:acc)

{-- Instances --}

instance Show a => Show (SymmetricList a) where
    -- show (SL _ _ l) = show l
    show (SL _ _ (xs, ys)) = show (xs Lst.++ Lst.reverse ys)
    -- show (SL len (xs, ys)) = ("<- ") ++ (show xs) ++ (" | ") ++ (show $ reverse ys) ++ (" ->")

instance Eq a => Eq (SymmetricList a) where
    (==) sl@(SL lenL lenR (xs, ys)) sl'@(SL lenL' lenR' (xs', ys'))
        | lenL + lenR /= lenL' + lenR' = False
        | lenL > lenL' = sl' == sl
        | Just mid' <- Lst.stripPrefix xs xs'
        , Just mid  <- Lst.stripPrefix ys' ys = Lst.reverse mid == mid'
        | otherwise = False

instance Ord a => Ord (SymmetricList a) where
    compare (SL _ _ (xs, ys)) (SL _ _ (xs', ys')) = go xs xs'
        where
            go (a:as) (b:bs) = compare a b <> go as bs
            go as     bs     = compare (as Lst.++ Lst.reverse ys) (bs Lst.++ Lst.reverse ys')

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

instance NFData a => NFData (SymmetricList a) where
    rnf (SL _ _ (l, r)) = rnf l `seq` rnf r

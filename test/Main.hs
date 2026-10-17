module Main (main) where

import Test.Hspec 
import Test.QuickCheck
import Data.SymmetricList (SymmetricList)
import qualified Data.SymmetricList as SymmetricList
import qualified Data.List as L
import Control.Exception (evaluate)

data Seed a = Cons a | Snoc a | Reverse | Uncons | Unsnoc | Tail  deriving (Show)

instance Arbitrary a => Arbitrary (Seed a) where
    arbitrary = frequency [(3, Cons <$> arbitrary), (3, Snoc <$> arbitrary), (2, pure Reverse), (1, pure Unsnoc), (1, pure Uncons), (1, pure Tail)]

apply :: SymmetricList a -> Seed a -> SymmetricList a 
apply s (Cons x) = SymmetricList.cons x s
apply s (Snoc x) = SymmetricList.snoc x s
apply s Reverse = SymmetricList.reverse s
apply s Unsnoc = maybe s fst (SymmetricList.unsnoc s)
apply s Uncons = maybe s snd (SymmetricList.uncons s)
apply s (Tail) 
    | null s = s
    | otherwise = SymmetricList.tail s 
    
instance Arbitrary a => Arbitrary (SymmetricList a) where
    arbitrary = foldl apply SymmetricList.nil <$> listOf arbitrary 
    shrink s = SymmetricList.fromList <$> shrink (SymmetricList.toList s)

fromSeeds :: [Seed a] -> SymmetricList a
fromSeeds = foldl apply SymmetricList.nil

prop_length_seed :: [Seed Int] -> Property
prop_length_seed seeds =
    let s = fromSeeds seeds
    in  length s === length (SymmetricList.toList s)

prop_fromList_toList :: [Int] -> Property
prop_fromList_toList xs =
    True ==> SymmetricList.toList (SymmetricList.fromList xs) == xs

prop_cons :: Int -> [Seed Int] -> Property
prop_cons x seeds = 
    True ==> SymmetricList.toList (SymmetricList.cons x s) == x:SymmetricList.toList s
    where
        s = fromSeeds seeds

prop_snoc :: Int -> [Seed Int] -> Property
prop_snoc x seeds = 
    True ==> SymmetricList.toList (SymmetricList.snoc x s) == SymmetricList.toList s ++ [x]
    where
        s = fromSeeds seeds

prop_head :: [Seed Int] -> Property
prop_head seeds = not (null xs) ==> SymmetricList.head s == L.head xs
    where
        s  = fromSeeds seeds
        xs = SymmetricList.toList s

prop_last :: [Seed Int] -> Property
prop_last seeds = not (null xs) ==> SymmetricList.last s == L.last xs
    where
        s  = fromSeeds seeds
        xs = SymmetricList.toList s

prop_tail :: [Seed Int] -> Property
prop_tail seeds = not (null xs) ==> SymmetricList.toList (SymmetricList.tail s) == L.tail xs
    where
        s  = fromSeeds seeds
        xs = SymmetricList.toList s

prop_null :: [Seed Int] -> Property
prop_null seeds = True ==> SymmetricList.null s == L.null (SymmetricList.toList s)
    where
        s = fromSeeds seeds

prop_dropWhile :: Fun Int Bool -> [Seed Int] -> Property
prop_dropWhile f seeds = True ==> SymmetricList.toList (SymmetricList.dropWhile (applyFun f) s) == L.dropWhile (applyFun f) (SymmetricList.toList s)
    where
        s = fromSeeds seeds

prop_reverse :: [Seed Int] -> Property 
prop_reverse seeds = True ==> SymmetricList.toList (SymmetricList.reverse s) == L.reverse (SymmetricList.toList s)
    where
        s = fromSeeds seeds

prop_length :: [Seed Int] -> Property
prop_length seeds = True ==> length (SymmetricList.fromList xs) == length xs
    where
        xs = SymmetricList.toList (fromSeeds seeds)

prop_map :: Fun Int Int -> [Seed Int] -> Property
prop_map f seeds = True ==> SymmetricList.toList (SymmetricList.map (applyFun f) s) == map (applyFun f) (SymmetricList.toList s)
    where
        s = fromSeeds seeds

prop_uncons :: [Seed Int] -> Property
prop_uncons seeds = not (null xs) ==> (y == x && SymmetricList.toList sl == ys)
    where 
        s  = fromSeeds seeds
        xs = SymmetricList.toList s
        Just (x, sl) = SymmetricList.uncons s
        Just (y, ys) = L.uncons xs

prop_append :: [Seed Int] -> [Seed Int] -> Property
prop_append seeds seeds' = True ==> SymmetricList.toList (SymmetricList.append s s') == SymmetricList.toList s L.++ SymmetricList.toList s'
    where
        s  = fromSeeds seeds
        s' = fromSeeds seeds'

prop_semigroup :: [Seed Int] -> [Seed Int] -> Property
prop_semigroup seeds seeds' = True ==> s <> s' == SymmetricList.append s s'
    where
        s  = fromSeeds seeds
        s' = fromSeeds seeds'

prop_semigroup_assoc :: [Seed Int] -> [Seed Int] -> [Seed Int] -> Property
prop_semigroup_assoc seeds seeds' seeds'' = True ==> (s <> s') <> s'' == s <> (s' <> s'')
    where
        s   = fromSeeds seeds
        s'  = fromSeeds seeds'
        s'' = fromSeeds seeds''

prop_unsnoc :: [Seed Int] -> Property
prop_unsnoc seeds = not (null xs) ==> (x == y && SymmetricList.toList sl == ys) 
    where 
        s  = fromSeeds seeds
        xs = SymmetricList.toList s
        Just (sl, x) = SymmetricList.unsnoc s
        Just (ys, y) = L.unsnoc xs

prop_singleton :: Int -> Property
prop_singleton x = True ==> (SymmetricList.toList . SymmetricList.singleton) x == L.singleton x

prop_eq :: [Seed Bool] -> [Seed Bool] -> Property
prop_eq seeds seeds' = (s == s') === (SymmetricList.toList s == SymmetricList.toList s')
    where
        s  = fromSeeds seeds
        s' = fromSeeds seeds'

prop_eq_representation :: [Seed Int] -> Property
prop_eq_representation seeds = s === SymmetricList.fromList (SymmetricList.toList s)
    where
        s = fromSeeds seeds

prop_compare :: [Seed Bool] -> [Seed Bool] -> Property
prop_compare seeds seeds' = compare s s' === compare (SymmetricList.toList s) (SymmetricList.toList s')
    where
        s  = fromSeeds seeds
        s' = fromSeeds seeds'

main :: IO ()
main = hspec $ do
    describe "fromList" $ do
        it "round-trips a list through toList" $ do
            property prop_fromList_toList
        
    describe "cons" $ do 
        it "behaves like (:) on lists" $ do
            property prop_cons

    describe "snoc" $ do

        it "behaves like (++ [x]) on lists" $ do
            property prop_snoc

    describe "head" $ do
        it "trhows an error when the list is empty" $ do
            SymmetricList.head SymmetricList.nil `shouldThrow` anyErrorCall

        it "take out the element we just added from the top" $ do
            SymmetricList.head (SymmetricList.cons 1 SymmetricList.nil) `shouldBe` L.head (1:[])

        it "take out the element we just added from the bottom" $ do
            SymmetricList.head (SymmetricList.snoc 1 SymmetricList.nil) `shouldBe` L.head (1:[])
        
        it "behaves like head on lists" $ do
            property prop_head

    describe "last" $ do
        it "throws an error when the list is empty" $ do
            SymmetricList.last SymmetricList.nil `shouldThrow` anyErrorCall

        it "give the last element we just added from the front" $ do
            SymmetricList.last (SymmetricList.cons 1 SymmetricList.nil) `shouldBe` L.last (1:[])

        it "give the last element we just added from the back" $ do
            SymmetricList.last (SymmetricList.snoc 1 SymmetricList.nil) `shouldBe` L.last (1:[])

        it "behaves like last on lists" $ do
            property prop_last 


    describe "tail" $ do
        it "throw error when the list is empty" $ do
            evaluate (SymmetricList.tail SymmetricList.nil) `shouldThrow` anyErrorCall

        it "give nil when we just added from the front" $ do
            SymmetricList.toList (SymmetricList.tail (SymmetricList.cons 1 SymmetricList.nil)) `shouldBe` []

        it "give nil when we just added from the back" $ do
            SymmetricList.toList (SymmetricList.tail (SymmetricList.snoc 1 SymmetricList.nil)) `shouldBe` []

        it "give the tail of the list" $ do
            property prop_tail

    describe "null" $ do
        it "behaves like null on lists" $ do
            property prop_null

    describe "dropWhile" $ do
        it "give nil when the list is empty" $ do
            ((SymmetricList.toList (SymmetricList.dropWhile (\x -> True) SymmetricList.nil)) :: [Int]) `shouldBe` ((L.dropWhile (\x -> True) []) :: [Int])

        it "returns an empty list when the predicate matches every element" $ do
            SymmetricList.toList (SymmetricList.dropWhile (\x -> True) $ SymmetricList.fromList [1 .. 3]) `shouldBe` L.dropWhile (\x -> True) [1 .. 3]

        it "returns the original list when the predicate matches no elements" $ do
            SymmetricList.toList (SymmetricList.dropWhile (\x -> False) $ SymmetricList.fromList [1 .. 3]) `shouldBe` L.dropWhile (\x -> False) [1 .. 3]

        it "behaves like dropWhile on lists" $ do 
            property prop_dropWhile

    describe "reverse" $ do
        it "behaves like reverse on lists" $ do
            property prop_reverse

    describe "length" $ do
        it "behaves like length on lists using fromList" $ do
            property prop_length
        
        it "behaves like length on lists using Seed" $ do
            property prop_length_seed

    describe "map" $ do
        it "behaves like map on lists" $ do
            property prop_map

    describe "uncons" $ do
        it "behaves like uncons on lists" $ do
            property prop_uncons
        
        it "give Nothing when the list is empty" $ do
            SymmetricList.uncons (SymmetricList.fromList ([]:: [Int])) `shouldBe` Nothing

    describe "unsnoc" $ do
        it "behaves like unsnoc on lists" $ do
            property prop_unsnoc

        it "give Nothing when the list is empty" $ do
            SymmetricList.unsnoc (SymmetricList.fromList ([]:: [Int])) `shouldBe` Nothing

    describe "append" $ do
        it "behaves like (++) on lists" $ do
            property prop_append

    describe "(<>)" $ do
        it "behaves like append" $ do
            property prop_semigroup

        it "is associative" $ do
            property prop_semigroup_assoc


    describe "singleton" $ do
        it "behaves like singleton on lists" $ do
            property prop_singleton

    describe "(==)" $ do
        it "behaves like (==) on the represented lists" $ do
            property prop_eq

        it "ignores the internal representation" $ do
            property prop_eq_representation

        it "considers equal lists with the same elements split differently" $ do
            SymmetricList.snoc 3 (SymmetricList.fromList [1, 2]) `shouldBe` SymmetricList.cons 1 (SymmetricList.fromList [2, 3 :: Int])

    describe "compare" $ do
        it "behaves like compare on the represented lists" $ do
            property prop_compare

module Main (main) where

import Test.Hspec 
import Test.QuickCheck
import qualified Data.SymmetricList as SL
import qualified Data.List as L
import Control.Exception (evaluate)

data Seed a = Cons a | Snoc a | Reverse | Uncons | Unsnoc | Tail  deriving (Show)

instance Arbitrary a => Arbitrary (Seed a) where
    arbitrary = frequency [(3, Cons <$> arbitrary), (3, Snoc <$> arbitrary), (2, pure Reverse), (1, pure Unsnoc), (1, pure Uncons), (1, pure Tail)]

apply :: SL.SymmetricList a -> Seed a -> SL.SymmetricList a 
apply s (Cons x) = SL.cons x s
apply s (Snoc x) = SL.snoc x s
apply s Reverse = SL.reverse s
apply s Unsnoc = maybe s fst (SL.unsnoc s)
apply s Uncons = maybe s snd (SL.uncons s)
apply s (Tail) 
    | null s = s
    | otherwise = SL.tail s 
    
instance Arbitrary a => Arbitrary (SL.SymmetricList a) where
    arbitrary = foldl apply SL.nil <$> listOf arbitrary 
    shrink s = SL.fromList <$> shrink (SL.toList s)

fromSeeds :: [Seed a] -> SL.SymmetricList a
fromSeeds = foldl apply SL.nil

prop_length_seed :: [Seed Int] -> Property
prop_length_seed seeds =
    let s = fromSeeds seeds
    in  length s === length (SL.toList s)

prop_fromList_toList :: [Int] -> Property
prop_fromList_toList xs =
    True ==> SL.toList (SL.fromList xs) == xs

prop_cons :: Int -> [Seed Int] -> Property
prop_cons x seeds = 
    True ==> SL.toList (SL.cons x s) == x:SL.toList s
    where
        s = fromSeeds seeds

prop_snoc :: Int -> [Seed Int] -> Property
prop_snoc x seeds = 
    True ==> SL.toList (SL.snoc x s) == SL.toList s ++ [x]
    where
        s = fromSeeds seeds

prop_head :: [Seed Int] -> Property
prop_head seeds = not (null xs) ==> SL.head s == L.head xs
    where
        s  = fromSeeds seeds
        xs = SL.toList s

prop_last :: [Int] -> Property
prop_last xs = not (null xs) ==> SL.last (SL.fromList xs) == L.last xs

prop_tail :: [Int] -> Property
prop_tail xs = not (null xs) ==> SL.toList (SL.tail (SL.fromList xs)) == L.tail xs

prop_null :: [Int] -> Property
prop_null xs = True ==> SL.null (SL.fromList xs) == L.null xs 

prop_dropWhile :: Fun Int Bool -> [Int] -> Property
prop_dropWhile f xs = True ==> SL.toList (SL.dropWhile (applyFun f) (SL.fromList xs)) == L.dropWhile (applyFun f) xs

prop_reverse :: [Int] -> Property 
prop_reverse xs = True ==> SL.toList (SL.reverse (SL.fromList xs)) == L.reverse xs

prop_length :: [Int] -> Property
prop_length xs = True ==> length (SL.fromList xs) == length xs

prop_map :: Fun Int Int -> [Int] -> Property
prop_map f xs = True ==> SL.toList (SL.map (applyFun f) (SL.fromList xs)) == map (applyFun f) xs

prop_uncons :: [Int] -> Property
prop_uncons xs = not (null xs) ==> (y == x && SL.toList sl == ys)
    where 
        Just (x, sl) = SL.uncons (SL.fromList xs)
        Just (y, ys) = L.uncons xs

prop_conc :: [Int] -> [Int] -> Property
prop_conc xs ys = True ==> SL.toList ((SL.fromList xs) SL.++ (SL.fromList ys)) == xs L.++ ys 

prop_unsnoc :: [Int] -> Property
prop_unsnoc xs = not (null xs) ==> (x == y && SL.toList sl == ys) 
    where 
        Just (sl, x) = SL.unsnoc (SL.fromList xs)
        Just (ys, y) = L.unsnoc xs

prop_singleton :: Int -> Property
prop_singleton x = True ==> (SL.toList . SL.singleton) x == L.singleton x 

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
            SL.head SL.nil `shouldThrow` anyErrorCall

        it "take out the element we just added from the top" $ do
            SL.head (SL.cons 1 SL.nil) `shouldBe` L.head (1:[])

        it "take out the element we just added from the bottom" $ do
            SL.head (SL.snoc 1 SL.nil) `shouldBe` L.head (1:[])
        
        it "behaves like head on lists" $ do
            property prop_head

    describe "last" $ do
        it "throws an error when the list is empty" $ do
            SL.last SL.nil `shouldThrow` anyErrorCall

        it "give the last element we just added from the front" $ do
            SL.last (SL.cons 1 SL.nil) `shouldBe` L.last (1:[])

        it "give the last element we just added from the back" $ do
            SL.last (SL.snoc 1 SL.nil) `shouldBe` L.last (1:[])

        it "behaves like last on lists" $ do
            property prop_last 


    describe "tail" $ do
        it "throw error when the list is empty" $ do
            evaluate (SL.tail SL.nil) `shouldThrow` anyErrorCall

        it "give nil when we just added from the front" $ do
            SL.toList (SL.tail (SL.cons 1 SL.nil)) `shouldBe` []

        it "give nil when we just added from the back" $ do
            SL.toList (SL.tail (SL.snoc 1 SL.nil)) `shouldBe` []

        it "give the tail of the list" $ do
            property prop_tail

    describe "null" $ do
        it "behaves like null on lists" $ do
            property prop_null

    describe "dropWhile" $ do
        it "give nil when the list is empty" $ do
            ((SL.toList (SL.dropWhile (\x -> True) SL.nil)) :: [Int]) `shouldBe` ((L.dropWhile (\x -> True) []) :: [Int])

        it "returns an empty list when the predicate matches every element" $ do
            SL.toList (SL.dropWhile (\x -> True) $ SL.fromList [1 .. 3]) `shouldBe` L.dropWhile (\x -> True) [1 .. 3]

        it "returns the original list when the predicate matches no elements" $ do
            SL.toList (SL.dropWhile (\x -> False) $ SL.fromList [1 .. 3]) `shouldBe` L.dropWhile (\x -> False) [1 .. 3]

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
            SL.uncons (SL.fromList ([]:: [Int])) `shouldBe` Nothing

    describe "unsnoc" $ do
        it "behaves like unsnoc on lists" $ do
            property prop_unsnoc

        it "give Nothing when the list is empty" $ do
            SL.unsnoc (SL.fromList ([]:: [Int])) `shouldBe` Nothing

    describe "(++)" $ do
        it "behaves like (++) on lists" $ do
            property prop_conc

    describe "singleton" $ do
        it "behaves like singleton on lists" $ do
            property prop_singleton
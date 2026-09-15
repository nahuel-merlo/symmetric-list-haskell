module Main (main) where

import Test.Hspec 
import qualified Data.SymmetricList as SL
import qualified Data.List as L
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "fromList" $ do
        it "creates an empty list from []" $ do
            SL.toList (SL.fromList ([] :: [Int])) `shouldBe` []
        
        it "preserve the elements" $ do 
            SL.toList (SL.fromList [1 .. 3]) `shouldBe` [1 .. 3]

    describe "toList" $ do
        it "creates from nil" $ do
            ((SL.toList SL.nil) :: [Int]) `shouldBe` ([] :: [Int])
        

    describe "cons" $ do 
        it "behaves like (:) on empty lists" $ do
            SL.toList (SL.cons 1 $ SL.nil) `shouldBe` 1:[]
        it "behaves like (:) on lists" $ do
            SL.toList (SL.cons 1 $ SL.fromList [2 .. 3]) `shouldBe` 1:[2 .. 3]

    describe "snoc" $ do
        it "behaves like (++ [x]) on empty lists" $ do
            SL.toList (SL.snoc 1 SL.nil) `shouldBe` [] ++ [1]

        it "behaves like (++ [x]) on lists" $ do
            SL.toList (SL.snoc 3 $ SL.fromList [1 .. 2]) `shouldBe` ([1 .. 2] ++ [3])

    describe "head" $ do
        it "trhows an error when the list is empty" $ do
            SL.head SL.nil `shouldThrow` anyErrorCall

        it "behaves like head on lists" $ do
            SL.head (SL.fromList [1 .. 3]) `shouldBe` L.head [1 .. 3]

        it "take out the element we just added from the top" $ do
            SL.head (SL.cons 1 SL.nil) `shouldBe` L.head (1:[])

        it "take out the element we just added from the bottom" $ do
            SL.head (SL.snoc 1 SL.nil) `shouldBe` L.head (1:[])

    describe "last" $ do
        it "throws an error when the list is empty" $ do
            SL.last SL.nil `shouldThrow` anyErrorCall

        it "behaves like last on lists" $ do
            SL.last (SL.fromList [1 .. 3]) `shouldBe` last [1 .. 3]

        it "give the last element we just added from the front" $ do
            SL.last (SL.cons 1 SL.nil) `shouldBe` L.last (1:[])

        it "give the last element we just added from the back" $ do
            SL.last (SL.snoc 1 SL.nil) `shouldBe` L.last (1:[])

    describe "tail" $ do
        it "throw error when the list is empty" $ do
            evaluate (SL.tail SL.nil) `shouldThrow` anyErrorCall

        it "give nil when we just added from the front" $ do
            SL.toList (SL.tail (SL.cons 1 SL.nil)) `shouldBe` []

        it "give nil when we just added from the back" $ do
            SL.toList (SL.tail (SL.snoc 1 SL.nil)) `shouldBe` []

        it "give the tail of the list" $ do
            SL.toList (SL.tail (SL.fromList [1 .. 3])) `shouldBe` L.tail [1 .. 3]

    describe "null" $ do
        it "behaves like null on lists" $ do
            SL.null SL.nil `shouldBe` L.null []

        it "gives False when the list has one element" $ do
            SL.null (SL.cons 1 SL.nil) `shouldBe` L.null (1:[])
        
        it "gives False when the list has n elemnts" $ do
            SL.null (SL.fromList [1 .. 3]) `shouldBe` L.null [1 .. 3]

    describe "dropWhile" $ do
        it "give nil when the list is empty" $ do
            ((SL.toList (SL.dropWhile (\x -> True) SL.nil)) :: [Int]) `shouldBe` ((L.dropWhile (\x -> True) []) :: [Int])

        it "returns an empty list when the predicate matches every element" $ do
            SL.toList (SL.dropWhile (\x -> True) $ SL.fromList [1 .. 3]) `shouldBe` L.dropWhile (\x -> True) [1 .. 3]

        it "returns the original list when the predicate matches no elements" $ do
            SL.toList (SL.dropWhile (\x -> False) $ SL.fromList [1 .. 3]) `shouldBe` L.dropWhile (\x -> False) [1 .. 3]

        it "behaves like dropWhile on lists" $ do 
            SL.toList (SL.dropWhile (==2) $ SL.fromList [1 .. 3]) `shouldBe` L.dropWhile (==2) [1 .. 3]
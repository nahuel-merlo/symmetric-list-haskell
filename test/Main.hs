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
        it "add an element to an empty list" $ do
            SL.toList (SL.cons 1 $ SL.nil) `shouldBe` 1:[]
        it "add an element" $ do
            SL.toList (SL.cons 1 $ SL.fromList [2 .. 3]) `shouldBe` 1:[2 .. 3]

    describe "snoc" $ do
        it "add an element to an empty list" $ do
            SL.toList (SL.snoc 1 SL.nil) `shouldBe` [] ++ [1]

        it "add an element to the back" $ do
            SL.toList (SL.snoc 3 $ SL.fromList [1 .. 2]) `shouldBe` ([1 .. 2] ++ [3])

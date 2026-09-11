module Main (main) where

import Test.Hspec
import Data.SymmetricList

main :: IO ()
main = hspec $ do
    describe "fromList" $ do
        it "creates an empty list from []" $ do
            toList (fromList ([] :: [Int])) `shouldBe` []
        
        it "preserve the elements" $ do 
            toList (fromList [1 .. 1000]) `shouldBe` [1 .. 1000]

    describe "cons" $ do 
        it "add an element to an empty list" $ do
            toList (cons 1 $ fromList []) `shouldBe` [1]
        it "add an element" $ do
            toList (cons 1 $ fromList [2 .. 1000]) `shouldBe` 1:[2 .. 1000]
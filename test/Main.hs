module Main (main) where

import Data.SymmetricList

main :: IO ()
main = do
    test "fromList empty" $ toList (fromList ([] :: [Int])) == []
    test "fromList" $ toList (fromList [1 .. 1000]) == [1 .. 1000] 
    test "const" $ toList (cons 1 $ fromList [2 .. 1000]) == 1:[2 .. 1000]
    test "snoc" $ toList (snoc 1000 $ fromList [1 .. 999]) == [1 .. 1000]

test :: String -> Bool -> IO ()
test name condition
    | condition = putStrLn ("✓ " ++ name)
    | otherwise = putStrLn ("✗ " ++ name)
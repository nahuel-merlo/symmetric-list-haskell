module Main (main) where

import Test.Tasty.Bench
import qualified Data.SymmetricList as SL
import qualified Data.List as L

main :: IO ()
main = defaultMain
  [ env (pure (SL.fromList [1 .. n :: Int], [1 .. n :: Int])) $ \ ~(sl, xs) ->
      bgroup ("n=" ++ show n) 
        [ bgroup "last"
            [ bench "symmetricList" $ whnf SL.last sl
            , bcompare (baseline n "last") $
              bench "list" $ whnf last xs
            ]
            , bgroup "reverse" 
            [ bench "symmetricList" $ whnf SL.reverse sl
            , bcompare (baseline n "reverse") $ 
              bench "list" $ whnf reverse xs
            ]
        ]
  | n <- [1000, 10000, 100000]
  ]

baseline :: Int -> String -> String
baseline n node = ("$NF == \"symmetricList\" && $(NF-1) == \"" ++ node ++ "\" && $(NF-2) == \"n="++ show n ++"\"")
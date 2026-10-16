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
            , bgroup "snoc"
            [ bench "symmetricList" $ whnf (uncurry SL.snoc) (n, sl)
            , bcompare (baseline n "snoc") $
              bench "list" $ nf (uncurry (++)) (xs, [n])
            ]
            , bgroup "init"
            [ bench "symmetricList" $ whnf SL.init sl
            , bcompare (baseline n "init") $ 
              bench "list" $ nf init xs
            ]
            , bgroup "unsnoc"
              [ bench "symmetricList" $ whnf SL.unsnoc sl
              , bcompare (baseline n "unsnoc") $
                bench "list" $ nf L.unsnoc xs
              ]
        ]
  | n <- [1000, 10000, 100000]
  ]

baseline :: Int -> String -> String
baseline n node = ("$NF == \"symmetricList\" && $(NF-1) == \"" ++ node ++ "\" && $(NF-2) == \"n="++ show n ++"\"")
module Main (main) where

import Blackjack

main :: IO ()
--main = putStrLn $ unlines $ fmap show $ take 100 $ M.toList myTable
--main = dumpCSV
main = do
    computeAndStore
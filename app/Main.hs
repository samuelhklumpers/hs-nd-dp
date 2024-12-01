module Main (main) where

import Blackjack
import qualified Data.Map.Lazy as M

main :: IO ()
--main = putStrLn $ unlines $ fmap show $ take 100 $ M.toList myTable
main = computeAndStore
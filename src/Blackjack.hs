{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveAnyClass, TupleSections,
ScopedTypeVariables, TypeApplications, FlexibleContexts, FlexibleInstances,
MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

{-|
This module defines a variant of Blackjack and formulates the optimal strategy as a dynamic programming problem.
-}
module Blackjack ( module Blackjack ) where


import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Traversable (forM)
import Data.List (groupBy, sortOn, intercalate)
import Data.Function (on)
import Data.Bifunctor (first, second)
import Data.Serialize (encode, Serialize, decode)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import Data.Either (fromRight, fromLeft)
import Probability (Mean, MaxMean, Variance, weightedAvg, avg, Mixture, mean, LogP1, HasMean)
import Data.Proxy (Proxy (..))
import Data.Maybe (fromJust, mapMaybe, fromMaybe)
import Control.Monad.State.Class (MonadState, modify)
import GHC.Exts (fromString)
import Control.Monad (forM_)

import DP


-- * Definition

{-|
The type of hands, holding cards with values summing up to `normals` with an optional additional `ace`.

(This works, because a second ace always has a value of 1).
-}
data Hand = Hand { normals :: !Int , ace :: !Bool } deriving (Eq, Ord, Show, Generic)
{-| 
The type of blackjack states.
-}
data Blackjack = Blackjack {
    -- | The player's hand.
    you :: !Hand,
    -- | The dealers's hand.
    dealer :: !Hand,
    -- | `True` if the player is standing.
    stand :: !Bool,
    -- | The penalty counter for bribing, the actual penalty is accessed via `bribeM`.
    bribeM_ :: !Int,
    -- | The counter for bribing.
    bribe :: !Int,
    -- | The payout multiplier counter, the actual multiplier is accessed via `multiplier`.
    multiplier_ :: !Int ,
    -- | The fraction of wealth invested.
    bet :: !Double } deriving (Eq, Ord, Show, Generic)
data Action = Bet !Double | Draw | Win | Lose | Dealer | Hit | Stand | Bribe deriving (Eq, Ord, Show, Generic)

instance Hashable Hand
instance Hashable Action
instance Hashable Blackjack

unBet :: Action -> Double
unBet (Bet x) = x
unBet _ = error "unBet: x != Bet y"

isBet :: Action -> Bool
isBet (Bet _) = True
isBet _ = False

draw :: [Hand]
draw = Hand 0 True : (normalHand <$> ([2..10] ++ [10, 10, 10, 10]))

normalHand :: Int -> Hand
normalHand x = Hand x False

blackjack0 :: Blackjack
blackjack0 = Blackjack (Hand 0 False) (Hand 0 False) False 0 0 0 1

multiplier :: Blackjack -> Double
multiplier = (2+) . (*0.1) . fromIntegral . multiplier_

bribeM :: Blackjack -> Double
bribeM = (0.75 ^) . bribeM_

deriving instance Serialize Hand
deriving instance Serialize Blackjack
deriving instance Serialize Action

instance Semigroup Hand where
    (Hand x y) <> (Hand z w)
      | y && w    = Hand (x + z + 1) True
      | otherwise = Hand (x + z) (y || w)

instance Monoid Hand where
    mempty = Hand 0 False


-- * Problem statement
blackjackP :: (Monad m, Fractional s, Mixture s, Ord s) => DPProblem Blackjack s Action m
blackjackP = DPProblem srch blackjackVal $ \ (s, _, ea) -> do
    return (s, fromLeft Nothing ea)
    where
    srch b 
      | valueHand (you b) == 0 = uniSearch 1 Bet 0 1 b
      | otherwise = enumSearch blackjackAct b

valueHand :: Hand -> Int
valueHand (Hand n a)
    | a && n < 11 = n + 11
    | a = n + 1
    | otherwise = n

blackjackAct :: Fractional s => Blackjack -> Either s [Action]
blackjackAct b
  | multiplier_ b > 0 = Left 0
  | y > 21  = lose
  | d > 21  = win
  | d == 21 = if y == 21 then draw' else lose
  | y == 21 = win
  | stand b = if y < d then lose else
              if d < 17 then Right [Dealer] else
              if y == d then draw' else win
  | otherwise = if bribeM_ b < 3 then Right [Hit, Stand, Bribe] else Right [Hit, Stand]
  where
  lose = Right [Lose]
  win = Right [Win]
  draw' = Right [Draw]
  y = valueHand $ you b
  d = valueHand (dealer b)

blackjackVal :: (Monad m, Fractional stat, Mixture stat) => Blackjack -> (Blackjack -> m stat) -> Action -> m stat
blackjackVal b v (Bet f) = do
    essBet <- forM (liftA2 (<>) draw draw) $ \ drawYou ->
        forM (liftA2 (<>) draw draw) $ \ drawDealer ->
        v (b { you = drawYou , dealer = drawDealer , bet = f })
    let eBet = avg $ concat essBet
    return eBet
blackjackVal b v Draw   = do
    let e = realToFrac (bet b * (bribeM b - 1))
    e' <- v (blackjack0 { multiplier_ = multiplier_ b + 1 })
    return $ e + e'
blackjackVal b v Win    = do
    let e = realToFrac (bet b * (multiplier b * bribeM b - 1))
    e' <- v (blackjack0 { multiplier_ = multiplier_ b + 1 })
    return $ e + e'
blackjackVal b _ Lose    = do
    return $ realToFrac (-bet b)
blackjackVal b v Dealer = do
    esHit <- forM draw $ \ drawDealer ->
        v (b { dealer = dealer b <> drawDealer })
    return $ avg esHit
blackjackVal b v Hit    = do
    let bribe' = max 0 (bribe b - 1)
    let pBribe = fromIntegral (bribe b) / 4

    eHitBribe <- if valueHand (you b) > 10
        then v (b { you = Hand 21 False })
        else v (b { you = you b <> normalHand 10 , bribe = bribe' })

    esHit <- forM draw $ \ drawYou ->
        v (b { you = you b <> drawYou , bribe = bribe' })

    let eHitFair = avg esHit
    let eHit = weightedAvg [(pBribe, eHitBribe), (1 - pBribe, eHitFair)]
    return eHit
blackjackVal b v Stand  = do
    v (b { stand = True , bribe = 0 })
blackjackVal b v Bribe  = do
    v (b { bribe = 1 + bribe b , bribeM_ = 1 + bribeM_ b })


-- * Processing
fmtHand :: Hand -> String
fmtHand (Hand n a) = show n ++ (if a then "A" else "")

fmtAdvice :: (Int, Action) -> String
fmtAdvice (n, a) = show n ++ [head $ show a]

graph :: M.Map k a -> M.Map k (k, a)
graph = M.mapWithKey (,)

dumpCSV :: M.Map (Hand, Hand, Int, Int) [(Int, Action)] -> IO ()
dumpCSV table = do
    sheets' <- sheets
    forM_ sheets' $ \ (i, j, sheet) -> do
        BS.writeFile ("tables_test/" ++ show i ++ show j ++ "kelly.csv") $ fromString sheet

    where
    sheets :: IO [(Int, Int, String)]
    sheets = sequence [sheet i j | j <- [0..3], i <- [0..j]]
        where
        sheet i j = do
            let t1 = M.filterWithKey (\ (h, _, i', j') _ -> i == i' && j == j' && h /= Hand 0 False) table
            let t2 = M.toList $ concatMap fmtAdvice <$> M.mapKeys (\ (x, y, _, _) -> (x, y)) t1
            let hands' = [Hand x False | x <- [4..20]] ++ [Hand x True | x <- [1..20]]
            let hands = fmtHand <$> hands'
            let t3 = fmap (\ h -> mapMaybe (\ ((y, d), x) -> if y == h then Just (d, x) else Nothing) t2) hands'
            let t4 = (\ xs -> fmap (fromMaybe "-" . flip lookup xs) hands') <$> t3

            let header = "You\\Deal" : hands
            let body = header : zipWith (:) hands t4

            return (i, j, unlines $ intercalate "," <$> body)

-- some stored tables
compactKellyEnumStored :: IO (M.Map (Hand, Hand, Int, Int) [(Int, Action)])
compactKellyEnumStored = fromRight (error "bad read: compactKellyEnumStored") . decode <$> BS.readFile "tables_test/holocure_blackjack_kelly_red.bin"

kellyEnumStored :: IO (M.Map Blackjack (Double, Maybe Action))
kellyEnumStored = M.fromList . fromRight (error "bad read: kellyEnumStored") . decode <$> BS.readFile "tables_test/holocure_blackjack_kelly.bin"

kellyUniStored :: IO (M.Map Blackjack (Double, Maybe Action))
kellyUniStored = M.fromList . fromRight (error "bad read: kellyUniStored") . decode <$> BS.readFile "tables_test/holocure_blackjack_kelly_uni.bin"

computeAndStore :: IO ()
computeAndStore = do
    table <- blackjackTableRed :: (IO (HM.HashMap Blackjack (MaxMean (LogP1 (Mean Double)), Maybe Action)))
    print $ length table
    --let compTable = compact $ summary $ M.fromList $ HM.toList table
    _ <- BS.writeFile "tables_test/holocure_blackjack_kelly_uni.bin" $ encode $ HM.toList $ first mean <$> table
    return ()

compact :: HasMean v Double => M.Map (Hand, Hand, Int, Int) [(Blackjack, (v, Action))] -> M.Map (Hand, Hand, Int, Int) [(Int, Action)]
compact table = M.filter (not . null) $ fmap (groupRuns . fmap (second (first mean))) table
    where
    groupRuns :: [(Blackjack, (Double, Action))] -> [(Int, Action)]
    groupRuns = fmap pick . groupBy ((==) `on` snd . snd) . sortOn fst . fmap (first multiplier_)

    pick :: [(Int, (Double, Action))] -> (Int, Action)
    pick xs@(x : _) = (minimum (fst <$> xs), snd $ snd x)
    pick [] = error "crude:pick: This is bad."

summary :: M.Map Blackjack (s, Maybe Action) -> M.Map (Hand, Hand, Int, Int) [(Blackjack, (s, Action))]
summary table = M.mapKeysWith (++) (\ b -> (you b , dealer b , bribe b , bribeM_ b)) . fmap check . graph $ table
    where
    v = (\ m -> (m,) $ unBet $ fromJust $ snd $ table M.! blackjack0 { multiplier_ = m } ) <$> [0..19]

    check :: (Blackjack, (s, Maybe Action)) -> [(Blackjack, (s, Action))]
    check (_, (_, Nothing)) = []
    check (b, (s, Just a))  =
        [(b, (s, a)) | multiplier_ b < 20 && (a `elem` [Hit,Stand,Bribe] && lookup (multiplier_ b) v == Just (bet b) || isBet a)]

blackjackTableRed :: (Ord s, Fractional s, Mixture s) => IO (HM.HashMap Blackjack (s, Maybe Action))
blackjackTableRed = do
    !t <- blackjackTable
    execDP (reducedP t blackjackP) blackjack0

blackjackTable :: (Ord s, Fractional s, Mixture s) => IO (HM.HashMap Blackjack (s, Maybe Action))
blackjackTable = execDP blackjackP blackjack0
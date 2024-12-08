{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}


{-|
This module defines a variant of Blackjack and formulates the optimal strategy as a `DPProblem`.
-}
module Blackjack ( computeAndStore , dumpCSV' ) where

import Control.Monad (forM_)
import Data.Bifunctor (first, second)
import Data.Bits (shiftL, (.|.), xor)
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import Data.Function (on)
import Data.Hashable ( Hashable(hash) )
import qualified Data.HashMap.Strict as HM
import Data.List (groupBy, sortOn, intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, mapMaybe, fromMaybe)
import Data.Serialize (encode, Serialize, decode)
import Data.Traversable (forM)
import GHC.Exts (fromString)
import GHC.Float (castDoubleToWord64)
import GHC.Generics (Generic)

import DP (DPProblem(..), enumSearch, uniSearch, reducedDP)
import Probability (Mean, MaxMean, weightedAvg, avg, Mixture, mean, Log1P, HasMean)

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
    -- | The fraction of wealth betted.
    bet :: !Double } deriving (Eq, Ord, Show, Generic)

multiplier :: Blackjack -> Double
multiplier = (2+) . (*0.1) . fromIntegral . multiplier_

bribeM :: Blackjack -> Double
bribeM = (0.75 ^) . bribeM_

instance Hashable Blackjack where
    hash (Blackjack (Hand y ya) (Hand d da) s b_ b m b') =
        (    y
        .|. fromEnum ya `shiftL` 5
        .|.           d `shiftL` 6
        .|. fromEnum da `shiftL` 11
        .|.  fromEnum s `shiftL` 12
        .|.          b_ `shiftL` 13
        .|.           b `shiftL` 15
        .|.           m `shiftL` 17)
        `xor` fromIntegral (castDoubleToWord64 b')

deriving instance Serialize Blackjack

{-|
The type of hands, holding cards with values summing up to `normals` with an optional additional `ace`.
(This works, because a second ace always has a value of 1).
-}
data Hand = Hand { normals :: !Int , ace :: !Bool } deriving (Eq, Ord, Show, Generic)

instance Hashable Hand
deriving instance Serialize Hand

instance Semigroup Hand where
    (Hand x y) <> (Hand z w)
      | y && w    = Hand (x + z + 1) True
      | otherwise = Hand (x + z) (y || w)

instance Monoid Hand where
    mempty = Hand 0 False

{-|
The total space of actions that can be taken in a game of blackjack.

Of these, the actions a player can take are `Bet`, `Hit`, `Stand`, and `Bribe`.
The others are internal actions to allow for better (and more readable) memoization. 
-}
data Action = Bet !Double  -- ^ Bet a fraction of your wealth
            | Draw         -- ^ Occurs when the game is drawn
            | Win          -- ^ Occurs when the game is won
            | Lose         -- ^ Occurs when the game is lost
            | Dealer       -- ^ Occurs when the dealer has to hit
            | Hit          -- ^ Hit
            | Stand        -- ^ Stand
            | Bribe        -- ^ Bribe the dealer up to 3 times, trading 25% of your remaining potential winnings,
                           -- for an additional 25% to hit a good card (see `blackjackVal`), decreasing by 25% for each hit.
    deriving (Eq, Ord, Show, Generic)

deriving instance Serialize Action

unBet :: Action -> Double
unBet (Bet x) = x
unBet _ = error "unBet: x != Bet y"

isBet :: Action -> Bool
isBet (Bet _) = True
isBet _ = False

normalHand :: Int -> Hand
normalHand x = Hand x False

valueHand :: Hand -> Int
valueHand (Hand n a)
    | a && n < 11 = n + 11
    | a = n + 1
    | otherwise = n

drawDistribution :: [Hand]
drawDistribution = Hand 0 True : (normalHand <$> ([2..10] ++ [10, 10, 10, 10]))


-- | The initial state of a game of blackjack.
blackjack0 :: Blackjack
blackjack0 = Blackjack (Hand 0 False) (Hand 0 False) False 0 0 0 1

{-|
The problem statement for optimal blackjack play, geared towards objective functions for which it makes to not go all in.
(That is, long-term strategies).

Exhaustively searches whether to `Hit`, `Stand`, or `Bribe`, but uses a golden section search to find a good betting fraction.
-}
blackjackP :: (Monad m, Fractional s, Mixture s, Ord s) => DPProblem Blackjack s Action m
blackjackP = DPProblem srch blackjackVal
    where
    srch b
      | valueHand (you b) == 0 = uniSearch 12 Bet 0 1 b
      | otherwise = enumSearch blackjackAct b
{-
{-|
The problem statement for optimal blackjack play, geared towards optimizing the expected value of a single round.
-}

blackjackP' :: (Monad m, Fractional s, Mixture s, Ord s) => DPProblem Blackjack s Action m
blackjackP' = DPProblem (enumSearch blackjackAct) blackjackVal
-}

{-|
The set of available actions for each state.

Some notes:
Ideally, we model the case in which we never lose, but we cannot for obvious reasons.
Fortunately the game does not go on infinitely and at some point the increase in the multiplier becomes neglegible (at the scale you are expected to lose).

The next most ideal model would solve some (piecewise) linear equation to obtain an estimate for the reward in late round.
This doesn't work either, because the matrix is huge (something like n=10584) and dense.
(Which is still tractable probably, but it also needs some clever searching to get the strategy to settle on something first).

Something similar happens for draws, since this doesn't increment the multiplier, and therefore refers back to the previous state;
which would also require solving a big linear equation.

Instead, we truncate the game after round 30.
This does influence the behaviour and results in the last rounds, but this is drowned out by the small chance of getting there from earlier rounds (hopefully).

The set of actions is given as:
- if your hand is empty, bet and draw two cards for you and the dealer each
- if the value of your hand goes over 21, you lose
- if the value of the dealer's hand goes over 21, you win
- if the dealer gets a blackjack
    - and you don't, you lose
    - and you do, you draw
- if you get a blackjack, you win
- if you are standing
    - and the dealer has a better hand, you lose
    - and the dealer's hand has value less than 17, the dealer hits
    - if the your and the dealer's hand are of the same value, you draw
    - otherwise, you win
- you can hit or stand
- if you bribed less than 3 times, you can also bribe
-}
blackjackAct :: Fractional s => Blackjack -> Either s [Action]
blackjackAct b
  | multiplier_ b > 30 = Left 0
  | y == 0  = Right [Bet 1] -- NOTE: leave this here so if you remove the `uniSearch`, you get back the normal expected value. 
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

-- | Calculate the objective value of performing an action in the given state
blackjackVal :: (Monad m, Fractional stat, Mixture stat) => Blackjack -> (Blackjack -> m stat) -> Action -> m stat
blackjackVal b v (Bet f) = do
    essBet <- forM (liftA2 (<>) drawDistribution drawDistribution) $ \ drawYou ->
        forM (liftA2 (<>) drawDistribution drawDistribution) $ \ drawDealer ->
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
    esHit <- forM drawDistribution $ \ drawDealer ->
        v (b { dealer = dealer b <> drawDealer })
    return $ avg esHit
blackjackVal b v Hit    = do
    let bribe' = max 0 (bribe b - 1)
    let pBribe = fromIntegral (bribe b) / 4

    eHitBribe <- if valueHand (you b) > 10
        then v (b { you = Hand 21 False })
        else v (b { you = you b <> normalHand 10 , bribe = bribe' })

    esHit <- forM drawDistribution $ \ drawYou ->
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

dumpCSV' :: IO ()
dumpCSV' = kellyUniCompactStored >>= dumpCSV

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

{-
compactKellyEnumStored :: IO (M.Map (Hand, Hand, Int, Int) [(Int, Action)])
compactKellyEnumStored = fromRight (error "bad read: compactKellyEnumStored") . decode <$> BS.readFile "tables_test/holocure_blackjack_kelly_red.bin"

kellyEnumStored :: IO (M.Map Blackjack (Double, Maybe Action))
kellyEnumStored = M.fromList . fromRight (error "bad read: kellyEnumStored") . decode <$> BS.readFile "tables_test/holocure_blackjack_kelly.bin"

kellyUniStored :: IO (M.Map Blackjack (Double, Maybe Action))
kellyUniStored = M.fromList . fromRight (error "bad read: kellyUniStored") . decode <$> BS.readFile "tables_test/holocure_blackjack_kelly_uni.bin"
-}

kellyUniCompactStored :: IO (M.Map (Hand, Hand, Int, Int) [(Int, Action)])
kellyUniCompactStored = M.fromList . fromRight (error "bad read: kellyUniCompactStored") . decode <$> BS.readFile "tables_test/holocure_blackjack_kelly_uni_compact.bin"

computeAndStore :: IO ()
computeAndStore = do
    table <- blackjackTableRed :: (IO (HM.HashMap Blackjack (MaxMean (Log1P (Mean Double)), Maybe Action)))
    print $ length table
    print $ table HM.!? blackjack0
    let compTable = compact $ summary $ M.fromList $ HM.toList table
    _ <- BS.writeFile "tables_test/holocure_blackjack_kelly_uni_compact.bin" $ encode $ M.toList compTable
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
blackjackTableRed = reducedDP blackjackP blackjack0
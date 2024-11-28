{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Blackjack ( module Blackjack ) where

import DP
import Lib
import qualified Data.Map.Lazy as M
import Control.Monad.Memo (startRunMemo)
import Data.Traversable (forM)
import Data.List (groupBy, sortOn)
import Data.Function (on)
import Data.Bifunctor (first)
import Data.Serialize (encode, Serialize, decode)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import Data.Either (fromRight)

data Action' = Bet' | Draw' | Win' | Dealer' | Hit' | Stand' | Bribe' deriving (Eq, Show, Generic)

deriving instance Serialize Action'

type Stats' = Double
type Stats'' = (Stats', Maybe Action')

-- TODO generalize over the statistics and maximized quantity

cf' :: IO (M.Map (Hand, Hand, Int, Int) [(Int, Action')])
cf' = fromRight (error "bad read: cf") . decode <$> BS.readFile "holocure_blackjack_crude_expect_2.bin"

computeAndStore :: IO ()
computeAndStore = do
    _ <- BS.writeFile "holocure_blackjack_crude_expect_2.bin" $ encode blackjackTableCompact
    return ()

blackjackTableCompact :: M.Map (Hand, Hand, Int, Int) [(Int, Action')]
blackjackTableCompact = M.filter (not . null) $ fmap go blackjackTable'
    where
    go :: [(Blackjack, (Stats', Action'))] -> [(Int, Action')]
    go = fmap pick . groupBy ((==) `on` snd . snd) . sortOn fst . fmap (first multiplier_)

    pick :: [(Int, (Stats', Action'))] -> (Int, Action')
    pick xs@(x : _) = (minimum (fst <$> xs), snd $ snd x)
    pick [] = error "crude:pick: This is bad."

blackjackTable' :: M.Map (Hand, Hand, Int, Int) [(Blackjack, (Stats', Action'))]
blackjackTable' = (M.mapKeysWith (++) (\ b -> (you b , dealer b , bribe b , bribeM_ b)) . fmap go . graph) blackjackTable
    where
    go :: (Blackjack, (Stats', Maybe Action')) -> [(Blackjack, (Stats', Action'))]
    go (_, (_, Nothing)) = []
    go (b, (s, Just a))  = [(b, (s, a)) | a `elem` [Bet',Hit',Stand',Bribe'] && multiplier_ b < 20]


blackjackTable :: M.Map Blackjack (Stats', Maybe Action')
blackjackTable = snd $ startRunMemo $ solveDP blackjackP blackjack0

blackjackP :: Monad m => DPProblem Blackjack Stats' Action' m
blackjackP = DPProblem blackjackAct blackjackVal (const $ return ())

type G = Either Stats' [Action']

lose :: G
lose = Left 0

win :: G
win = Right [Win']

draw' :: G
draw' = Right [Draw']

blackjackAct :: Blackjack -> G
blackjackAct b
  | multiplier_ b > 30 = Left 0
  | y == 0  = Right [Bet']
  | y > 21  = lose
  | d > 21  = win
  | d == 21 = if y == 21 then draw' else lose
  | y == 21 = win
  | stand b = if y < d then lose else
              if d < 17 then Right [Dealer'] else
              if y == d then draw' else win
  | otherwise = if bribeM_ b < 3 then Right [Hit', Stand', Bribe'] else Right [Hit', Stand']
  where
  y = valueHand $ you b
  d = valueHand (dealer b)

blackjackVal :: Monad m => Blackjack -> (Blackjack -> m Stats') -> Action' -> m Stats'
blackjackVal b v Bet'    = do
    essBet <- forM (liftA2 (<>) draw draw) $ \ drawYou ->
        forM (liftA2 (<>) draw draw) $ \ drawDealer ->
        v (b { you = drawYou , dealer = drawDealer , bet = True })
    let eBet = average $ concat essBet
    return (eBet - 1)
blackjackVal b v Draw'   = do
    let e = bribeM b
    e' <- v (blackjack0 { multiplier_ = multiplier_ b + 1 })
    return $ e + e'
blackjackVal b v Win'    = do
    let e = multiplier b * bribeM b
    e' <- v (blackjack0 { multiplier_ = multiplier_ b + 1 })
    return $ e + e'
blackjackVal b v Dealer' = do
    esHit <- forM draw $ \ drawDealer ->
        v (b { dealer = dealer b <> drawDealer })
    return $ average esHit
blackjackVal b v Hit'    = do
    let bribe' = max 0 (bribe b - 1)
    let pBribe = fromIntegral (bribe b) / 4

    eHitBribe <- if valueHand (you b) > 10
        then v (b { you = Hand 21 False })
        else v (b { you = you b <> normalHand 10 , bribe = bribe' })

    esHit <- forM draw $ \ drawYou ->
        v (b { you = you b <> drawYou , bribe = bribe' })

    let eHitFair = average esHit
    return $ pBribe * eHitBribe + (1 - pBribe) * eHitFair
blackjackVal b v Stand'  = do
    v (b { stand = True , bribe = 0 })
blackjackVal b v Bribe'  = do
    v (b { bribe = 1 + bribe b , bribeM_ = 1 + bribeM_ b })

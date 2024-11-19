{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( module Lib ) where

import qualified Data.Map as M
import Control.Monad.Trans.State.Lazy
    ( execState, get, modify, State )
import Control.Monad ( forM )
import Data.List ( sortOn )
import Data.Serialize ( Serialize, encode, decode ) 
import GHC.Generics ( Generic )
import qualified Data.ByteString as BS
import Data.Either ( fromRight )

{-
The python is bad but has the right idea.

We want DP to calculate the E and optimal choices at the same time.
DP is more than memoization, we want the table back.
-}

-- #hands: 21 * 20 / 2 = 210
-- #both hands: 210^2
-- #stand: 2 * 210^2
-- #bribe: 6 * 210^2
-- 264600
-- a linear solve will not agree

data Hand = Hand { normals :: Int , ace :: Bool } deriving (Eq, Ord, Show, Generic)
data Action = NA | Bet Bool | Hit | Stand | Bribe deriving (Eq, Ord, Show, Generic)
data Blackjack = Blackjack
    { you :: Hand, dealer :: Hand, stand :: Bool , bribeM_ :: Int
    , bribe :: Int , multiplier_ :: Int , bet :: Bool } deriving (Eq, Ord, Show, Generic)
data Stats = Stats { action :: Action , expectation :: Double } deriving (Eq, Ord, Show, Generic)
data Status = Play | Win | Lose | Draw deriving (Eq, Ord, Show)

deriving instance Serialize Hand
deriving instance Serialize Blackjack
deriving instance Serialize Action
deriving instance Serialize Stats

type DP = M.Map Blackjack Stats

blackjack0 :: Blackjack
blackjack0 = Blackjack (Hand 0 False) (Hand 0 False) False 0 0 0 True

memo :: (Blackjack -> State DP Stats) -> Blackjack -> State DP Stats
memo f b = do
    m <- get
    case m M.!? b of
        Just v  -> return v
        Nothing -> do
            v <- f b
            modify $ M.insert b v
            return v

dp' :: Blackjack -> State DP Stats
dp' b = let dpM = memo dp' in do
    -- we cut off the table at 5.1 thank you
    -- just look at the bit between 2.0 and 4.0
    -- alternative is matrix but the system is too big
    
    -- _ <- traceShow b (return ())
    if multiplier_ b > 30 {-5.0-} then
        return $ Stats Stand 0
    else
        case blackjackState b of
            Win -> do
                let e = if bet b then multiplier b * bribeM b - 1 else 0
                e' <- dpM (blackjack0 { multiplier_ = multiplier_ b + 1 })
                return $ Stats NA (e + expectation e')
            Draw -> do
                -- should be the solution of some system but table too big 
                return $ Stats NA 0
            Lose -> do
                return $ Stats NA $ if bet b then (-1) else 0
            Play -> do
                if you b == mempty then do
                    essNoBet <- forM (liftA2 (<>) draw draw) $ \ drawYou ->
                           forM (liftA2 (<>) draw draw) $ \ drawDealer ->
                           dpM (b { you = drawYou , dealer = drawDealer , bet = False })
                    let eNoBet = average $ expectation <$> concat essNoBet
                    essBet <- forM (liftA2 (<>) draw draw) $ \ drawYou ->
                           forM (liftA2 (<>) draw draw) $ \ drawDealer ->
                           dpM (b { you = drawYou , dealer = drawDealer , bet = True })
                    let eBet = average $ expectation <$> concat essBet

                    if eBet > eNoBet then
                        return $ Stats (Bet True) eBet
                    else
                        return $ Stats (Bet False) eNoBet
                else if stand b then do
                    esHit <- forM draw $ \ drawDealer ->
                                 dpM (b { dealer = dealer b <> drawDealer })
                    return $ Stats NA $ average $ expectation <$> esHit
                else do
                    let bribe' = max 0 (bribe b - 1)
                    let pBribe = fromIntegral (bribe b) / 4

                    eHitBribe <- expectation <$> if valueHand (you b) > 10
                        then dpM (b { you = Hand 21 False })
                        else dpM (b { you = you b <> normalHand 10 , bribe = bribe' })

                    esHit <- forM draw $ \ drawYou ->
                                 dpM (b { you = you b <> drawYou , bribe = bribe' })

                    let eHitFair = average $ expectation <$> esHit
                    let eHit = pBribe * eHitBribe + (1 - pBribe) * eHitFair

                    -- setting bribe = 0 makes the table marginally smaller
                    eStand <- expectation <$> dpM (b { stand = True , bribe = 0 })
                    eBribe <- do
                        if bribeM_ b > 2 then
                            return $ -2
                        else
                            expectation <$> dpM (b { bribe = 1 + bribe b , bribeM_ = 1 + bribeM_ b })

                    let s = sortOn (negate . expectation)
                         [Stats Hit eHit, Stats Stand eStand, Stats Bribe eBribe]

                    return $ head s

multiplier :: Blackjack -> Double
multiplier = (2+) . (*0.1) . fromIntegral . multiplier_

bribeM :: Blackjack -> Double
bribeM = (0.75 ^) . bribeM_ 

dp :: DP
dp = execState (memo dp' blackjack0) mempty
-- this doesn't understand stability, I think

computeAndStore :: IO ()
computeAndStore = do
    _ <- BS.writeFile "holocure_blackjack_0bet.bin" $ encode dp 
    return ()

dpFromFile :: IO DP
dpFromFile = fromRight (error "bad read: dpFromFile") . decode <$> BS.readFile "holocure_blackjack_0bet.bin"

poll :: DP -> (Int, Int) -> (Int, Int) -> Int -> Int -> Int -> Stats
poll dp (y, ya) (d, da) m b mb = dp M.! blackjack0 { you = Hand y (ya > 0) , dealer = Hand d (da > 0) , multiplier_ = m , bribe = b , bribeM_ = mb }

-- 1. TODO
-- poll dpf (20, 0) (20, 0) 10 3 3
-- is a Hit but it should be Stand? (is this because draw is completely wrong? lol)
-- maybe just use a rescaled E from a higher multiplier

-- 2. TODO
-- we maximize the E of one run
-- this is bad
-- you want to maximize: the expected geometric growth rate
-- a. r = r_a^p_a * ...
-- b. kelly f^* = \mu / V for mean \mu and variance V
-- but r_a depends on f^*...

-- solution: just run it at 0.0, 0.05, 0.1 and see what happens


instance Semigroup Hand where
    (Hand x y) <> (Hand z w) 
      | y && w    = Hand (x + z + 1) True
      | otherwise = Hand (x + z) (y || w)

instance Monoid Hand where
    mempty = Hand 0 False
    

average :: Fractional a => [a] -> a
average xs = sum xs / fromIntegral (length xs)

draw :: [Hand]
draw = Hand 0 True : (normalHand <$> ([2..10] ++ [10, 10, 10, 10]))

normalHand :: Int -> Hand
normalHand x = Hand x False

draws :: Int
draws = 10 + 4

blackjackState :: Blackjack -> Status
blackjackState b
  | y > 21  = Lose
  | d > 21  = Win
  | d == 21 = if y == 21 then Draw else Lose
  | y == 21 = Win -- the order of the clauses matters now
  | stand b = if y < d then Lose else
              if d < 17 then Play else
              if y == d then Draw else Win
  | otherwise = Play
  where
  y = valueHand $ you b
  d = valueHand (dealer b)

valueHand :: Hand -> Int
valueHand (Hand n a)
    | a && n < 11 = n + 11
    | a = n + 1
    | otherwise = n
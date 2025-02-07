{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

{-
module RI.ReactorIdle (
    gameBest,
    upgradeShortName,
    gameBest',
    plantHeat,
    plantCellCost,
    tps,
    Game(..),
    Spec(..),
    Plant'(..),
    Build'(..),
    Plant,
    Build,
    Plants(..),
    Upgrade(..),
    UpgradeStats(..),
    Cell(..),
    Gen(..),
    Pump(..) ) where
-}

module RI.ReactorIdle ( module RI.ReactorIdle ) where

import qualified Data.Map as M
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Text.Printf (printf)
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Function
import Control.Monad
import ListT
import Control.Monad.State
import Debug.Trace


boundedEnum :: (Enum a, Bounded a) => [a]
boundedEnum = enumFrom minBound

pow :: (Floating a, Real b) => a -> b -> a
pow b e = b ** realToFrac e

data Cell = Thermo | Fusion | Thorium | Protactium
    deriving (Eq, Ord, Show, Generic, Hashable)

cellHeat :: (Real a, Floating b) => Cell -> a -> b
cellHeat c n = cellBaseHeat c * pow 1.25 n

cellBaseHeat :: Fractional a => Cell -> a
cellBaseHeat c = case c of
    Thermo  -> 50e6
    Fusion  -> 2.5e9
    Thorium -> 150e9
    Protactium -> 9e12

cellLife :: (Real a, Floating b) => Build' a -> Cell -> b
cellLife b cT = 800 * pow 2 (M.findWithDefault 0 (CellLife cT) (runBuild b))

cellHeatCostBase :: Fractional a => Cell -> a
cellHeatCostBase c = case c of
    Thermo  -> 100e9
    Fusion  -> 100e12
    Thorium -> 10e15
    Protactium -> 10e18 -- ?

cellLifeCostBase :: Fractional a => Cell -> a
cellLifeCostBase c = case c of
    Thermo  -> 500e9
    Fusion  -> 500e12
    Thorium -> 50e15
    Protactium -> 50e18 -- ?

cellCost :: Fractional a => Cell -> a
cellCost c = case c of
    Thermo  -> 20e9
    Fusion  -> 800e9
    Thorium -> 72e12
    Protactium -> 5.04e15 -- ?


data Gen = Gen2 | Gen3 | Gen4 | Gen5
    deriving (Eq, Ord, Show, Generic, Hashable)

genHeatBase :: Fractional a => Gen -> a
genHeatBase g = case g of
    Gen2 -> 9
    Gen3 -> 32
    Gen4 -> 96
    Gen5 -> 288

genWater :: (Real a, Floating b) => Gen -> a -> b
genWater g gL = genWaterBase g * pow 1.25 gL

genWaterBase :: Fractional a => Gen -> a
genWaterBase g = case g of
    Gen2 -> 5e3
    Gen3 -> 8e3
    Gen4 -> 22e3
    Gen5 -> 44e3

genWaterMult :: Fractional a => Gen -> a
genWaterMult g = case g of
    Gen2 -> 100
    Gen3 -> 200
    Gen4 -> 400
    Gen5 -> 1200

genCost :: Fractional a => Gen -> a
genCost g = case g of
    Gen2 -> 2.5e6
    Gen3 -> 10e12
    Gen4 -> 50e15
    Gen5 -> 12.5e15 -- ???


data Pump = Pump | GroundPump
    deriving (Eq, Ord, Show, Generic, Hashable)

pumpWater :: (Real a, Floating b) => Pump -> a -> b
pumpWater p pL = pumpWaterBase p * pow 1.5 pL

pumpWaterBase :: Fractional a => Pump -> a
pumpWaterBase p = case p of
    Pump -> 25e3
    GroundPump -> 67.5e3

isoMult :: (Real a, Fractional b) => Int -> a -> b
isoMult i n = 1 + fromIntegral i * (0.05 + 0.05 * realToFrac n)

circMult :: (Real a, Fractional b) => Bool -> a -> b
circMult False _ = 1
circMult True qL = 1.9 + 0.225 *  realToFrac qL


data Upgrade
    = CellHeat Cell | CellLife Cell
    | GenMaxHeat | GenEff | GenMaxWater
    | ElemMaxWater | PumpWater Pump
    | IsoMult | CircMult
    deriving (Eq, Ord, Show, Generic, Hashable)

type Level = Int

upgradeScale :: Fractional a => Upgrade -> a
upgradeScale u = case u of
    CellHeat _      -> 1.78
    CellLife _      -> 8
    GenMaxHeat      -> 3.7
    GenEff          -> 1.5
    GenMaxWater     -> 1.45
    ElemMaxWater    -> 2
    PumpWater p     -> if p == Pump then 1.98 else 2
    IsoMult         -> 10
    CircMult        -> 2.1 -- ?

upgradeBase :: Fractional a => Upgrade -> a
upgradeBase u = case u of
    CellHeat c      -> cellHeatCostBase c
    CellLife c      -> cellLifeCostBase c
    GenMaxHeat      -> 1e3
    GenEff          -> 4e2
    GenMaxWater     -> 20e9
    ElemMaxWater    -> 30e9
    PumpWater p     -> if p == Pump then 80e9 else 640e9
    IsoMult         -> 50e3
    CircMult        -> 1e15 -- ?

upgradeShortName :: Upgrade -> String
upgradeShortName u = case u of
    CellHeat _ -> "H"
    CellLife _ -> "L"
    GenMaxHeat -> "GMH"
    GenEff -> "GE"
    GenMaxWater -> "GMW"
    ElemMaxWater -> "WEMW"
    PumpWater _ -> "P"
    IsoMult -> "I"
    CircMult -> "C"

upgradeCost :: Upgrade -> Level -> Float
upgradeCost u n = upgradeBase u * upgradeScale u ^ n

upgradeToCost :: (Real a, Floating b) => Upgrade -> a -> b
upgradeToCost u n = upgradeBase u * (1 - s ** realToFrac n) / (1 - s)
    where
    s = upgradeScale u


data Spec = Spec { specCellType :: Cell, specCells :: Int
                 , specGenType :: Gen, specGens :: Int
                 , specPumpType :: Pump, specPumps :: Int
                 , specIsos :: Int, specCirc :: Bool }
    deriving (Eq, Ord, Generic, Hashable)

instance Semigroup Spec where
    (<>) = error "Semigroup Spec"

instance Monoid Spec where
    mempty = Spec Fusion 0 Gen2 0 Pump 0 0 False

instance Show Spec where
    show (Spec cT cN gT gN pT pN iN iC) = if iC then y ++ "C" else y
        where
        x = printf "%s%s %d:%d:%d" (show cT) (show gT) cN gN pN
        z = if pT == Pump then x ++ "P" else x
        y = if iN > 0 then z ++ ":" ++ show iN else z

specHeat :: (Real a, Floating b) => Build' a -> Spec -> b
specHeat b s = fromIntegral cN * cellHeat cellType cL * isoMult iN iL where
    upgrades = runBuild b
    cellType = specCellType s
    cN = specCells s
    cL = M.findWithDefault 0 (CellHeat cellType) upgrades
    iN = specIsos s
    iL = M.findWithDefault 0 IsoMult upgrades

specCost :: Fractional a => Spec -> a
specCost s = let c = fromIntegral (specGens s) * genCost (specGenType s) in
            if specCirc s then c + fromIntegral (specGens s) / 2 * 250e15 else c -- average...

specCellCost ::  (Real a, Floating b) => Build' a -> Spec -> b
specCellCost b s = fromIntegral (specCells s) * cellCost cT / cellLife b cT
    where
    cT = specCellType s


newtype Build' a = Build { runBuild :: M.Map Upgrade a }
    deriving (Eq, Ord, Generic, Hashable)

instance Functor Build' where
    fmap f = Build . fmap f . runBuild

instance Num a => Semigroup (Build' a) where
    x <> y = Build (runBuild x <> runBuild y)

instance Num a => Monoid (Build' a) where
    mempty = Build mempty

instance Show a => Show (Build' a) where
    show (Build b) = unwords $ (\ (u, n) -> upgradeShortName u ++ show n) <$> M.toList b

buildCostTo :: (Real a, Floating b) => Build' a -> b
buildCostTo = sum . fmap (uncurry upgradeToCost) . M.toList . runBuild

buildCostFromTo :: (Real a, Floating b) => Build' a -> Build' a -> b
buildCostFromTo bx by = buildCostTo (Build $ M.unionWith max (runBuild bx) (runBuild by)) - buildCostTo bx

type Build = Build' Level


data Plant' a = Plant { plantBuild :: Build' a , plantSpec :: Spec , plantCells :: Int }
    deriving (Eq, Ord, Generic, Hashable)

instance Semigroup (Plant' a) where
    (<>) = error "Semigroup Plant"

instance Num a => Monoid (Plant' a) where
    mempty = Plant mempty mempty 0

instance Show a => Show (Plant' a) where
    show (Plant b s n) = printf "%dx %s, %s" n (show s) (show b)

plantNetHeat :: (Real a, Floating b) => Plant' a -> b
plantNetHeat p = plantHeat p - plantCellCost p

plantHeat :: (Real a, Floating b) => Plant' a -> b
plantHeat (Plant b s n) = fromIntegral n * specHeat b s

plantCellCost :: (Real a, Floating b) => Plant' a -> b
plantCellCost (Plant b s n) = fromIntegral n * specCellCost b s

plantCostFromTo :: Plant -> Plant -> Float
plantCostFromTo (Plant b s n) (Plant b' s' n') = bC + pC
    where
    bC = buildCostFromTo b b'
    pC = max 0 $ fromIntegral n' * specCost s' - fromIntegral n * specCost s

type Plant = Plant' Level


data Plants = Island | Village | Region | City | SHC | Metro | FHC | Mainland | EHC | Continent
    deriving (Eq, Ord, Enum, Bounded, Show, Generic, Hashable)

plantBuyCost :: Plants -> Float
plantBuyCost p = case p of
    Mainland -> 30e18
    _ -> 1e100


newtype Game = Game { gamePlant :: M.Map Plants Plant }

gamePower :: Game -> Float
gamePower g = sum $ fmap plantHeat $ M.elems $ gamePlant g


data UpgradeStats = UpgradeStats { upgradeTotalCost :: Float, upgradeEff :: Float, upgradeBuy :: Float, upgradeRet :: Float, heat0 :: Float, dHeat :: Float } deriving Eq

instance Ord UpgradeStats where
    compare x y = compare (upgradeEff x) (upgradeEff y)

instance Show UpgradeStats where
    show (UpgradeStats c e b r h0 dh) = printf "% 6.2f = % 6.2f + % 6.2f -> +% 8.2e (% 6.2f%%) for % 8.2e" e b r dh (100 * dh / h0) c

type Specs = M.Map Plants [(Spec, Int)]

tps :: Int
tps = 5

data Research = RProtactium | RCirc deriving (Eq, Ord, Show)

plantPrereqs :: Plant -> [Research]
plantPrereqs (Plant b s n) = buildPrereqs b ++ specPrereqs s

specPrereqs :: Spec -> [Research]
specPrereqs s = [RCirc | specCirc s] ++ [RProtactium | specCellType s == Protactium]

buildPrereqs :: Build -> [Research]
buildPrereqs _ = []

takeWhileT :: (Monad m) => (a -> Bool) -> ListT m a -> ListT m a
takeWhileT f t = do
    unT <- lift $ uncons t
    case unT of
        Nothing     -> mempty
        Just (h, t) -> if f h then
            cons h (takeWhileT f t)
            else
            mempty

{-
researchBest' :: [Research] -> Specs -> ListT (State Game) (Game, Either (UpgradeStats, Plants, Plant) (Research, Level))
researchBest' block s = do
    g <- get

    -- 1. Find the best upgrade
    let o@(_, pn, p') = gameBest s g block
    let g' = Game $ M.insert pn p' $ gamePlant g

    -- 2. Does this upgrade use research we don't have yet?
    case dropWhile (`elem` gameResearch g) $ plantPrereqs p' of
        []         -> do
            -- 3. If not, yield the upgrade and update the state to after applying the upgrade
            put g'
            cons (g, Left o) $ researchBest' block s
        (prereq:_) -> do
            -- 4. 
            let (Just (_, f), gUp) = flip runState g $ ListT.head $ researchBest' (prereq:block) s
            let (_, pnUp, pUp) = either id (error "not implemented: double research") f

            -- check what happens if you research first
            let (lks', gRLk) = flip runState g' $ ListT.toList $ takeWhileT (either (\ (_, _, p) -> usesResearch prereq p) (const False) . snd) $ ListT.take 4 $ researchBest' block s
            let lks = fmap (either id (error "not implemented: double research") . snd) lks'

            -- patch the result of researching first onto upgrading first
            -- and vice versa
            -- compare time taken

            -- NB: not handled: grouping researches
            let rT = fastResearch g prereq
            let upRT = fastResearch gUp prereq

            let rLksUp = flip runState gRLk $ applyUpgrade pnUp pUp
            let upRLks = flip runState gUp $ forM_ lks $ \ (_, pn, p) -> applyUpgrade pn p

            undefined

gameResearch :: Game -> [Research]
gameResearch = _

researchBest :: Specs -> ListT (State Game) (Game, Either (UpgradeStats, Plants, Plant) (Research, Level))
researchBest = researchBest' []
-}

data ResearchStats = ResearchStats {  }

fastResearch :: Game -> Research -> ResearchStats
fastResearch = undefined

usesResearch :: Research -> Plant -> Bool
usesResearch r p = r `elem` plantPrereqs p

gameBest :: Specs -> Game -> (UpgradeStats, Plants, Plant)
gameBest s g = minimum $ gameBest' s g

gameBest' :: Specs -> Game -> [(UpgradeStats, Plants, Plant)]
gameBest' specs g = catMaybes $ do
    pn <- --[Island] 
        boundedEnum :: [Plants]

    let (p, acc) = maybe (mempty, plantBuyCost pn) (, 0) (gamePlant g M.!? pn)
    
    case plantBest specs g pn p acc of
        Nothing -> return Nothing
        Just (u, p') -> return $ Just (u, pn, p')

plantBest :: Specs -> Game -> Plants -> Plant -> Float -> Maybe (UpgradeStats, Plant)
plantBest specs g pn p acc = case specs M.!? pn of
    Nothing -> Nothing
    Just sns -> Just $ minimum $ do
        --traceShow pn $ return ()
        let (Plant _ s n) = p
        sn@(s', n') <- sns
        let acc' = max 0 $ fromIntegral n' * specCost s' - fromIntegral n * specCost s
        let (u, b') = buildBest g p sn (acc' + acc)

        return (u, Plant b' s' n')

buildBest :: Game -> Plant -> (Spec, Int) -> Float -> (UpgradeStats, Build)
buildBest g p@(Plant b _ _) (s', n') acc = go b
    where
    go b' = case bs' of
            [] -> (UpgradeStats 0 (1/0) 0 0 0 0, b')
            bs -> minimum bs
        where
        bs' = allBuilds g p (Plant b' s' n') acc

upgradeEffect :: (Real a, Real b) => Float -> Plant' a -> Plant' b -> Float -> Float
upgradeEffect power p q cost = upgradeEff $ mkUpgradeStats power p q cost

applyUpgrade :: (MonadState Game m) => Plants -> Plant -> m UpgradeStats
applyUpgrade pn q = do
    u <- getUpgradeStats pn q
    g <- get
    put $ g { gamePlant = M.insert pn q (gamePlant g) } 
    return u

getUpgradeStats :: (MonadState Game m) => Plants -> Plant -> m UpgradeStats
getUpgradeStats pn q = do
    g <- get
    return $ getUpgradeStats' g pn q

getUpgradeStats' :: Game -> Plants -> Plant -> UpgradeStats
getUpgradeStats' g pn q = mkUpgradeStats (gamePower g) p q c
    where
    p' = gamePlant g M.!? pn
    p = fromMaybe mempty p'
    c = plantCostFromTo p q + maybe (plantBuyCost pn) (const 0) p'

mkUpgradeStats :: (Real a, Real b) => Float -> Plant' a -> Plant' b -> Float -> UpgradeStats
mkUpgradeStats power p q cost = UpgradeStats cost eff buy ret h0 dh
    where
    eff = let v = buy + ret in if isNaN v then 1/0 else v

    buy = costH / power
    ret = costH / max 0 dh

    costH  = cost / 3600 / fromIntegral tps

    h0   = plantNetHeat p
    dh   = plantNetHeat q - h0

roundBuild :: Build' Float -> Build
roundBuild (Build b) = Build $ M.mapWithKey go b
    where
    go (CellHeat _) = floor
    go IsoMult = floor
    go _ = ceiling


allBuilds :: Game -> Plant -> Plant -> Float -> [(UpgradeStats, Build)]
allBuilds g p@(Plant b _ _) (Plant b' s' n') acc = flip evalState (1/0) $ toReverseList $ do
    let (Spec cT _ _ _ _ _ iN ciN) = s' 

    let lowerB = M.unionWith max (runBuild b) (runBuild b')
    let gmwL0 = max 30 $ M.findWithDefault 0 GenMaxWater lowerB
    let geL0 = max 60 $ M.findWithDefault 0 GenEff lowerB
    let ciL0 = M.findWithDefault 0 CircMult lowerB

    let lowerH = plantNetHeat p :: Float
    let pPH = gamePowerH g
    
    ciL <- fromFoldable $ if ciN then [ciL0 - 1..20] else [0]
    let cCI = upgradeToCost CircMult ciL / pPH
    upperT <- lift get
    guard (cCI < upperT) 

    iL <- fromFoldable $ if iN > 0 then [0..20] else [0]
    let cI = cCI + upgradeToCost IsoMult iL / pPH
    upperT <- lift get
    --traceShow (cI, upperT) $ return ()
    guard (cI < upperT) 

    cL <- fromFoldable [0..20]
    let cC = cI + upgradeToCost (CellHeat cT) cL / pPH
    upperT <- lift get
    --traceShow (cC, upperT) $ return ()
    guard (cC < upperT) 

    gmwL <- fromFoldable [gmwL0 - 1..80]
    let cGMW = cC + upgradeToCost GenMaxWater gmwL / pPH
    upperT <- lift get
    --traceShow (cGMW, upperT) $ return ()
    guard (cGMW < upperT) 

{-
    geL <- fromFoldable [geL0 - 1..90]
    let cGE = cGMW + upgradeToCost GenEff geL / pPH
    upperT <- lift get
    --traceShow (cGE, upperT) $ return ()
    guard (cGE < upperT) 
-}

    let b'' = roundBuild $ autoFillBuild' s' $ Build $ fmap fromIntegral $ lowerB
            & M.insert (CellHeat cT) cL
            & M.insert IsoMult iL
            & M.insert GenMaxWater gmwL
--            & M.insert GenEff geL
            & M.insert CircMult ciL

    let q = Plant b'' s' n'

    guard (plantNetHeat q > lowerH) 

    let cost = acc + buildCostFromTo b b''
    let e = mkUpgradeStats (gamePower g) p q cost

    lift $ modify (min $ upgradeEff e)

    return (e, b'')

gamePowerH :: Game -> Float
gamePowerH g = gamePower g * fromIntegral tps * 3600

autoFillBuild' :: Spec -> Build' Float -> Build' Float
autoFillBuild' s b = {-# SCC "Build" #-} Build $ M.unionWith max
    (M.fromList [(GenEff, geL), (ElemMaxWater, pL + 2), (PumpWater pT, pL)])
    (runBuild b)
    where
    upgrades = runBuild b
    cT = specCellType s
    pT = specPumpType s
    gT  = specGenType s

    cN = specCells s
    cL = {-# SCC "findWithDefault" #-} M.findWithDefault 0 (CellHeat cT) upgrades
    pN = specPumps s
    iN = specIsos s
    iL = M.findWithDefault 0 IsoMult upgrades
    gN = specGens s
    gL = M.findWithDefault 0 GenMaxWater upgrades
    geL0 = M.findWithDefault 0 GenEff upgrades
    qN = specCirc s
    qL = M.findWithDefault 0 CircMult upgrades

    heat   = fromIntegral cN * isoMult iN iL * cellHeat cT cL
    water  = fromIntegral gN * genWater gT gL * circMult qN qL

    heatCon0 = fromIntegral gN * genHeatBase gT * pow 1.25 geL0
    deficit0 = heat - heatCon0 - genWaterMult gT * water

    geL = max geL0 $ logBase 1.25 $ max ((deficit0 + heatCon0) / fromIntegral gN / genHeatBase gT) 1

    deficit = heat - fromIntegral gN * genHeatBase gT * pow 1.25 geL - genWaterMult gT * water
    waterExcess = max 0 (- deficit) / genWaterMult gT
    waterUsage = water - waterExcess

    pL = {-# SCC "logBase" #-} logBase 1.5 $ max (waterUsage / fromIntegral pN / pumpWaterBase pT) 1
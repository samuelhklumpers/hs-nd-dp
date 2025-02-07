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
import Data.Semigroup
import Data.Foldable


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

upgradeCostFromTo :: (Real a, Floating b) => Upgrade -> a -> a -> b
upgradeCostFromTo u n m = upgradeBase u * (s ** realToFrac m - s ** realToFrac n) / (s - 1)
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
buildCostFromTo old upd = sum $ M.mapWithKey (uncurry . upgradeCostFromTo)  $ M.intersectionWith (,) (runBuild old) new
    where
    new = M.unionWith max (runBuild old) (runBuild upd)
    --buildCostTo (Build $ ) - buildCostTo bx

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


data Game = Game
    { gamePlant :: M.Map Plants Plant
    , gameClock :: Float
    , gameCurrentResearch :: Float
    , gameResearchL :: M.Map Plants Int
    , gameResearch :: [Research] }

instance Show Game where
    show g = printf "t = % 6.2f ..." (gameClock g)

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

-- TODO research prereqs
-- researching something that has a prereq you don't have either?
-- tally up their costs, and notify that you're getting two researches

researchPrereq :: Research -> [Research]
researchPrereq = \case
    RProtactium -> [RCirc] -- TODO hack to avoid double research tehe
    RCirc       -> [RProtactium]

researchCost :: [Research] -> Research -> (Float, [Research])
researchCost xs r = (sum $ researchCost' <$> r:ys, r:ys)
    where
    ys = [y | y <- researchPrereq r, y `notElem` xs]

researchCost' :: Research -> Float
researchCost' = \case
    RProtactium -> 2.5e15
    RCirc -> 1.25e15

plantTiles :: Plants -> Int
plantTiles = \case
    Island -> 35
    Village -> 92
    Region -> 142
    City -> 279
    Metro -> 375
    Mainland -> 410
    Continent -> 450
    _ -> 0

researchTiles :: Game -> M.Map Plants Int
researchTiles = M.mapWithKey (\ k _ -> plantTiles k) . gamePlant

researchCorrection :: Game -> (Float, Float, M.Map Plants Int)
researchCorrection g = (rb', cb', floor <$> ds)
    where
    ts = M.filter (>0) $ fmap fromIntegral (researchTiles g) :: M.Map Plants Float

    cs = 1.78
    rs = 1.25
    w = 1 / log (cs / rs)

    t0 = minimum ts

    ds = (* w) . log . (/t0) <$> ts

    cb' = sum $ (1.78 **) <$> ds
    rb' = sum $ M.intersectionWith (*) ts $ (1.25 **) <$> ds



{-
plantPrereqs :: Plant -> [Research]
plantPrereqs (Plant b s n) = buildPrereqs b ++ specPrereqs s

buildPrereqs :: Build -> [Research]
buildPrereqs _ = []
-}

specPrereqs :: Spec -> [Research]
specPrereqs s = [RCirc | specCirc s] ++ [RProtactium | specCellType s == Protactium]

takeWhileT :: (Monad m) => (a -> Bool) -> ListT m a -> ListT m a
takeWhileT f t = do
    unT <- lift $ uncons t
    case unT of
        Nothing     -> mempty
        Just (h, t) -> if f h then
            cons h (takeWhileT f t)
            else
            mempty

data ResearchStats = ResearchStats { researchLevels :: M.Map Plants Int, researchTime :: Float, researchBuildTime :: Float, researcWaitTime :: Float, researchBuildCost :: Float, researchResearch :: [Research] }

instance Show ResearchStats where
    show (ResearchStats l t tb tw bc dr) = printf "Research: % 6.2f = % 6.2f + % 6.2f, for % 6.2e, %s, %s" t tb tw bc (show dr) (show l)

fastResearch :: Game -> Research -> ResearchStats
fastResearch g r = ResearchStats levels (build + wait) build wait cost dr
    where
    p = gamePower g
    (c', dr) = researchCost (gameResearch g) r
    c = c' - gameCurrentResearch g

    cs = 1.78
    rs = 1.25

    (rb', cb', offsets) = researchCorrection g

    cb = 25e3 * cb'
    rb = 8 * rb'

    a = 1 / rs * cs / (cs - 1)
    -- TODO Yeah you still have to test the lower and higher int
    n = round $ logBase (cs * rs) (a * c / rb * p / cb)
    levels = (+ n) <$> offsets

    --dlevels = M.unionWith (-) levels (gameResearchL g)
    tiles = fromIntegral <$> researchTiles g

    cost  = 25e3 * sum (max 0 <$> M.unionWith (-) ((cs ^) <$> levels) ((cs ^) <$> gameResearchL g))
    speed = 8 * sum (M.intersectionWith (*) tiles $ (rs ^) <$> levels)


    build = cost / p / fromIntegral tps / 3600
    wait  = c / speed / fromIntegral tps / 3600

{-
fastResearch :: Game -> Research -> ResearchStats
fastResearch g r = ResearchStats _ _ _ _
    where
    n = researchT0 g r
-}

doResearch' :: Game -> Research -> (ResearchStats, Game)
doResearch' g = flip runState g . doResearch

doResearch :: MonadState Game m => Research -> m ResearchStats
doResearch r = do
    g <- get

    let u = fastResearch g r

    let g' = g {
        gameResearchL = researchLevels u,
        gameResearch = researchResearch u ++ gameResearch g,
        gameClock = researchTime u + gameClock g }

    put g'
    return u

type Action = Either (UpgradeStats, Plants, Plant) (ResearchStats, Research)

researchBest' :: [Research] -> Int -> Int -> Specs -> ListT (State Game) (Game, Action)
researchBest' block d lookahead s = do
    g1 <- get

    -- 1. Find the best upgrade
    case gameBest block s g1 of
        -- TODO ah, the most efficient upgrade is not necessarily "the best" for branching your research on. Consider:
        -- Metro: buy + ret = 40, +10% total income
        -- SHC: buy + ret = 1, +0% total income

        -- TODO is there a better statistic to target for branching?

        Nothing -> mempty
        Just (_, pn, p') -> do
            -- Does this upgrade use research we don't have yet?
            case dropWhile (`elem` gameResearch g1) $ specPrereqs $ plantSpec p' of
                [] -> do
                    -- 2. If not, yield the upgrade and update the state to after applying the upgrade
                    {- _ <- applyUpgrade pn p'
                    cons (g1, Left o) $ researchBest' block s -}
                    applyUpgradeLifted pn p'
                    <> researchBest' block d lookahead s
                (prereq:_) -> do
                    --traceM $ "!" ++ show u

                    let block' = prereq : researchPrereq prereq

                    -- 3.a.1. Restart from 1. but ban @prereq@ 
                    let (u3a1, g3a1) = flip runState g1 $ ListT.head $ researchBest' (block' ++ block) (d + 1) lookahead s
                    let (_, pn3a1, p3a1) = either id (error "not implemented: double research") (snd $ fromJust u3a1)
                    -- TODO instead just keep taking until no research?
                    -- but then you have to take a couple more to make it fair? hmm
                    -- but taking more isn't necessarily better if you're blocked on research...
                    -- I guess you could try to do some "convolution"-esque approach if you're precomputing upgrades...

                    -- 3.a.2. Research @prereq@
                    let (_, g3a2) = doResearch' g3a1 prereq
                    --let (ra, g3a2) = doResearch' g3a1 prereq

                    -- 3.b.1. Research @prereq@ from 1. 
                    let (_, g3b1) = doResearch' g1 prereq
                    --let (rb, g3b1) = doResearch' g1 prereq

                    --traceM $ "Wait: " ++ show ra
                    --traceM $ "Research: " ++ show rb

                    -- 3.b.2. Apply the upgrade found in 1. 
                    let (_, g3b2) = applyUpgrade' g3b1 pn p'

                    -- 3.b.3. Carry on for @lookahead@ more steps
                    let (u3b3', g3b3) = flip runState g3b2 $ ListT.toList
                            -- $ takeWhileT (either (\ (_, _, p) -> specUsesResearch (plantSpec p) prereq) (const False) . snd)
                            -- TODO is this takeWhile necessary here?
                            -- can we drop it safely? does it not give a better estimate without it?

                            -- A: yes.
                            $ ListT.take lookahead $ researchBest' block (d + 1) lookahead s

                    let u3b3 = (\ (_, pn, p) -> (pn, p)) . either id (error "not implemented: double research") . snd <$> u3b3'

                    -- 3.b.4. Apply the upgrade found in 3.a.1.
                    let (_, g3b4) = applyUpgrade' g3b3 pn3a1 p3a1

                    -- 3.a.3. Apply the upgrade found in 1.
                    let (_, g3a3) = applyUpgrade' g3a2 pn p'

                    -- 3.a.4. Apply the upgrades found in 3.b.3.
                    let (_, g3a4) = flip runState g3a3 $ forM u3b3 (uncurry applyUpgrade)

                    {-
                    traceM $ "Depth: " ++ show d

                    traceM $ "Skip: " ++ unlines (show <$> [
                            g3a1,
                            g3a2,
                            g3a3,
                            g3a4
                        ])
                    
                    traceM $ "Research: " ++ unlines (show <$> [
                            g3b1,
                            g3b2,
                            g3b3,
                            g3b4
                        ])

                    traceM $ if gameClock g3a4 < gameClock g3b4 then "Skip" else "Take" 
                    -}

                    when (d == 0) $ do
                        let l = gameClock g3a4
                        let r = gameClock g3b4
                        traceM $ "D" ++ show d ++ ": " ++ show l ++ (if l < r then " < " else " > ") ++ show r

                    if gameClock g3a4 < gameClock g3b4 then
                        applyUpgradeLifted pn3a1 p3a1
                        {- <> doResearchLifted prereq
                        <> applyUpgradeLifted pn p'
                        <> Foldable.foldMap (uncurry applyUpgradeLifted) u3b3 -}
                        <> researchBest' block d lookahead s
                    else do
                        doResearchLifted prereq
                        <> applyUpgradeLifted pn p'
                        {- <> Foldable.foldMap (uncurry applyUpgradeLifted) u3b3
                        <> applyUpgradeLifted pn3a1 p3a1 -}
                        <> researchBest' block d lookahead s

doResearchLifted :: (MonadTrans t, MonadState Game m) => Research -> t m (Game, Action)
doResearchLifted r = lift $ do
    g <- get
    s <- doResearch r
    return (g, Right (s, r))

applyUpgradeLifted :: (MonadTrans t, MonadState Game m) => Plants -> Plant -> t m (Game, Action)
applyUpgradeLifted pn p = lift $ do
    g <- get
    s <- applyUpgrade pn p

    return (g, Left (s, pn, p))


researchBest :: Int -> Specs -> ListT (State Game) (Game, Action)
researchBest = researchBest' [] 0

{-
-- this is susceptible to double research
let (_, pn3a1, p3a1) = gameBest (prereq:block) s g1
let (_, g3a1) = applyUpgrade' g1 pn3a1 p3a1
-}

--usesResearch :: Research -> Plant -> Bool
--usesResearch r p = r `elem` plantPrereqs p

specUsesResearch :: Spec -> Research -> Bool
specUsesResearch s r = r `elem` specPrereqs s

specUsesAnyResearch :: Foldable t => Spec -> t Research -> Bool
specUsesAnyResearch s = any (specUsesResearch s)

gameBest :: [Research] -> Specs -> Game -> Maybe (UpgradeStats, Plants, Plant)
gameBest block s g = minimum' $ gameBest' block s g

gameBest' :: [Research] -> Specs -> Game -> [(UpgradeStats, Plants, Plant)]
gameBest' block specs g = catMaybes $ do
    pn <- --[Island] 
        boundedEnum :: [Plants]

    let (p, acc) = maybe (mempty, plantBuyCost pn) (, 0) (gamePlant g M.!? pn)

    case plantBest block specs g pn p acc of
        Nothing -> return Nothing
        Just (u, p') -> do
            return $ Just (u, pn, p')

plantBest :: [Research] -> Specs -> Game -> Plants -> Plant -> Float -> Maybe (UpgradeStats, Plant)
plantBest block specs g pn p acc = case specs M.!? pn of
    Nothing -> Nothing
    Just sns -> minimum' $ do
        --traceShow pn $ return ()
        let (Plant _ s n) = p
        sn@(s', n') <- sns

        guard (not $ any (specUsesResearch s') block)
        let acc' = max 0 $ fromIntegral n' * specCost s' - fromIntegral n * specCost s

        case buildBest g p sn (acc' + acc) of
            Nothing -> []
            Just (u, b') -> do
                --traceM $ "Prereqs " ++ show (specPrereqs s') ++ ", Block " ++ show block
                return (u, Plant b' s' n')

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = fmap getMin . foldMap' (Just . Min)

buildBest :: Game -> Plant -> (Spec, Int) -> Float -> Maybe (UpgradeStats, Build)
buildBest g p@(Plant b _ _) (s', n') acc = go b
    where
    go b' = case bs' of
            [] -> Just (UpgradeStats 0 (1/0) 0 0 0 0, b')
            bs -> minimum' bs
        where
        bs' = allBuilds g p (Plant b' s' n') acc

upgradeEffect :: (Real a, Real b) => Float -> Plant' a -> Plant' b -> Float -> Float
upgradeEffect power p q cost = upgradeEff $ mkUpgradeStats power p q cost

applyUpgrade' :: Game -> Plants -> Plant -> (UpgradeStats, Game)
applyUpgrade' g pn q = flip runState g $ applyUpgrade pn q

applyUpgrade :: (MonadState Game m) => Plants -> Plant -> m UpgradeStats
applyUpgrade pn q = do
    u <- getUpgradeStats pn q
    g <- get
    traceM $ show u ++ " u " ++ show (upgradeBuy u) ++ "g" ++ show (gameClock g)
    put $ g { gamePlant = M.insert pn q (gamePlant g), gameClock = upgradeBuy u + gameClock g }
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


-- TODO can we prune more builds with a lower bound on the cost for a given heat?
-- TODO can we ``defunctionalize'' this? I.e., right now we have essentially eta : Game -> Build -> Build -> Float
-- and we want to know whether eta g b b1 < eta g b b2.
-- Can we change this to eta' : Build -> X and eta'' : X -> Game -> Build -> Float for some concrete X?
-- and then hopefully from X extract conditions when x1 < x2.
allBuilds :: Game -> Plant -> Plant -> Float -> [(UpgradeStats, Build)]
allBuilds g p@(Plant b _ _) (Plant b' s' n') acc = flip evalState (1/0) $ toReverseList $ do
    let (Spec cT _ _ _ _ _ iN ciN) = s'

    let lowerB = M.unionWith max (runBuild b) (runBuild b')
    let gmwL0 = max 30 $ M.findWithDefault 0 GenMaxWater lowerB
    let ciL0 = M.findWithDefault 0 CircMult lowerB
    --let geL0 = max 60 $ M.findWithDefault 0 GenEff lowerB
    let clL0 = M.findWithDefault 0 (CellLife cT) lowerB

    let lowerH = plantNetHeat p :: Float
    let pPH = gamePowerH g

    iL <- fromFoldable $ if iN > 0 then [0..20] else [0]
    let t0 = upgradeToCost IsoMult iL / pPH

    upperT <- lift get
    guard (t0 < upperT)

    cL <- fromFoldable [0..20]
    let t1 = t0 + upgradeToCost (CellHeat cT) cL / pPH
    let hI = fromIntegral (n' * specCells s') * isoMult (specIsos s') iL  * cellHeat cT cL :: Float
    guard (hI > lowerH)

    upperT <- lift get
    guard (t1 < upperT)

    ciL <- fromFoldable $ if ciN then [max 0 (ciL0 - 1)..20] else [0]
    let t2 = t1 + upgradeToCost CircMult ciL / pPH
    upperT <- lift get
    guard (t2 < upperT)

    gmwL <- fromFoldable [max 0 (gmwL0 - 1)..80]
    let t3 = t2 + upgradeToCost GenMaxWater gmwL / pPH
    upperT <- lift get
    guard (t3 < upperT)

    clL <- fromFoldable [clL0 .. 3]
    let t4 = t3 + upgradeToCost (CellLife cT) clL / pPH
    upperT <- lift get
    guard (t4 < upperT)

{-
    geL <- fromFoldable [geL0 - 1..90]
    let cGE = cGMW + upgradeToCost GenEff geL / pPH
    upperT <- lift get
    --traceShow (cGE, upperT) $ return ()
    guard (cGE < upperT) 
-}

    let b'' = roundBuild $ autoFillBuild' s' $ Build $ fmap fromIntegral $ lowerB
            & M.insert (CellHeat cT) cL
            & (if iL > 0 then M.insert IsoMult iL else id)
            & M.insert GenMaxWater gmwL
--            & M.insert GenEff geL
            & (if ciL > 0 then M.insert CircMult ciL else id)
            & (if clL > 0 then M.insert (CellLife cT) clL else id)

    let q = Plant b'' s' n'

    --let x = buildCostFromTo b b''
    --when (x < 0) $ traceShow (b, b'', s, s') $ return ()

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
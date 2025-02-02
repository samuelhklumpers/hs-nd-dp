{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module ReactorIdle ( module ReactorIdle ) where

import qualified Data.Map as M

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import DP
import Data.List (find, sortOn)
import Control.Monad (ap, forM, forM_, replicateM)
import Text.Printf (printf)
import Data.Foldable (fold)
import Data.Maybe (mapMaybe)
import Data.Containers.ListUtils (nubOrdOn)
import Control.Monad.State.Lazy (get, StateT (runStateT), put)
import Control.Monad.Trans (lift)


tps :: Int
tps = 5

data Cell = Thermo | Fusion | Thorium | Protactium
    deriving (Eq, Ord, Show, Generic, Hashable)

cellHeatBase :: Fractional a => Cell -> a
cellHeatBase c = case c of
    Thermo  -> 100e9
    Fusion  -> 100e12
    Thorium -> 10e15
    Protactium -> 10e18 -- ?

cellLifeBase :: Fractional a => Cell -> a
cellLifeBase c = case c of
    Thermo  -> 500e9
    Fusion  -> 500e12
    Thorium -> 50e15
    Protactium -> 50e18 -- ?

baseHeat :: Fractional a => Cell -> a
baseHeat c = case c of
    Thermo  -> 50e6
    Fusion  -> 2.5e9
    Thorium -> 150e9
    Protactium -> 9e12
    
cellCost :: Cell -> Float
cellCost c = case c of
    Thermo  -> 20e9
    Fusion  -> 800e9
    Thorium -> 72e12
    Protactium -> 5.04e15 -- ?


data Gen = Gen2 | Gen3 | Gen4 | Gen5
    deriving (Eq, Ord, Show, Generic, Hashable)

data Pump = Pump | GroundPump
    deriving (Eq, Ord, Show, Generic, Hashable)

data Upgrade
    = CellHeat Cell | CellLife Cell
    | GenMaxHeat | GenEff | GenMaxWater
    | ElemMaxWater | PumpWater Pump
    | IsoMult | CircMult
    deriving (Eq, Ord, Show, Generic, Hashable)

type Level = Int

data Spec = Spec { specCellType :: Cell, specCells :: Int
                 , specGenType :: Gen, specGens :: Int
                 , specPumpType :: Pump, specPumps :: Int
                 , specIsos :: Int, specCirc :: Bool }
    deriving (Eq, Ord, Generic, Hashable)

newtype Build = Build { runBuild :: M.Map Upgrade Level }
    deriving (Eq, Ord, Generic, Hashable)

data Plant = Plant { plantBuild :: Build , plantSpec :: Spec , plantCells :: Int }
    deriving (Eq, Ord, Generic, Hashable)

instance Show Spec where
    show (Spec cT cN gT gN pT pN iN iC) = if iC then y ++ "C" else y
        where
        x = printf "%s%s %d:%d:%d" (show cT) (show gT) cN gN pN
        z = if pT == Pump then x ++ "P" else x
        y = if iN > 0 then z ++ ":" ++ show iN else z

instance Show Build where
    show (Build b) = unwords $ (\ (u, n) -> upgradeShortName u ++ show n) <$> M.toList b

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

instance Show Plant where
    show (Plant b s n) = printf "%dx %s, %s" n (show s) (show b)

buildCostTo :: Build -> Float
buildCostTo = x --(if any (< 0) (runBuild b) then traceShow b else id) x b
    where
    x = sum . fmap (uncurry upgradeToCost) . M.toList . runBuild

buildCostFromTo :: Build -> Build -> Float
buildCostFromTo bx by = buildCostTo (Build $ M.unionWith max (runBuild bx) (runBuild by)) - buildCostTo bx

upgradeCost :: Upgrade -> Level -> Float
upgradeCost u n = upgradeBase u * upgradeScale u ^ n

upgradeToCost :: Upgrade -> Level -> Float
upgradeToCost u n = upgradeBase u * (1 - s ^ n) / (1 - s)
    where
    s = upgradeScale u

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
    CellHeat c      -> cellHeatBase c
    CellLife c      -> cellLifeBase c
    GenMaxHeat      -> 1e3
    GenEff          -> 4e2
    GenMaxWater     -> 20e9
    ElemMaxWater    -> 30e9
    PumpWater p     -> if p == Pump then 80e9 else 640e9
    IsoMult         -> 50e3
    CircMult        -> 1e15 -- ?

completeCost :: Plant -> Float
completeCost = buildCostTo . autoFillBuild

completeCostFromTo :: Build -> Plant -> (Build, Float)
completeCostFromTo b = ap (,) (buildCostFromTo b) . autoFillBuild

autoFillBuild :: Plant -> Build
autoFillBuild (Plant b s _) = Build $
    M.insert GenEff geL $
    M.insert ElemMaxWater (pL + 2) $
    M.insert GenMaxWater gwL $ runBuild b
    where
    upgrades = runBuild b
    cellType = specCellType s
    pumpType = specPumpType s
    genType  = specGenType s

    cN = specCells s
    cL = M.findWithDefault 0 (CellHeat cellType) upgrades
    pN = specPumps s
    pL = M.findWithDefault 0 (PumpWater pumpType) upgrades
    iN = specIsos s
    iL = M.findWithDefault 0 IsoMult upgrades
    gN = specGens s
    --gL = M.findWithDefault 0 GenMaxWater upgrades
    qN = specCirc s
    qL = M.findWithDefault 0 CircMult upgrades

    heat = fromIntegral cN * isoMult iN iL * cellHeat cellType cL
    water = fromIntegral pN * pumpWater pumpType pL
    excess = heat - genWaterMult genType * water

    geL = ceiling $ logBase 1.25 $ max (excess / fromIntegral gN / genHeatBase genType) 1
    gwL = ceiling $ logBase 1.25 $ max (water / fromIntegral gN / genWaterBase genType / circMult qN qL) 1

genHeatBase :: Gen -> Float
genHeatBase g = case g of
    Gen2 -> 9
    Gen3 -> 32
    Gen4 -> 96
    Gen5 -> 288

--genWater :: Gen -> Level -> Float
--genWater g gL = genWaterBase g * 1.25 ^ gL

circMult :: Bool -> Level -> Float
circMult False _ = 1
circMult True qL = 1.9 + 0.225 * fromIntegral qL

genWaterBase :: Gen -> Float
genWaterBase g = case g of
    Gen2 -> 5e3
    Gen3 -> 8e3
    Gen4 -> 22e3
    Gen5 -> 44e3

genWaterMult :: Gen -> Float
genWaterMult g = case g of
    Gen2 -> 100
    Gen3 -> 200
    Gen4 -> 400
    Gen5 -> 1200

pumpWater :: Pump -> Level -> Float
pumpWater p pL = baseWater p * 1.5 ^ pL
    where
    baseWater p = case p of
        Pump -> 25e3
        GroundPump -> 67.5e3


isoMult :: Int -> Level -> Float
isoMult i n = 1 + fromIntegral i * (0.05 * (1 + fromIntegral n))

cellHeat :: Cell -> Level -> Float
cellHeat c n = baseHeat c * 1.25 ^ n

instance Semigroup Build where
    x <> y = Build (runBuild x <> runBuild y)

instance Monoid Build where
    mempty = Build mempty

-- data Research



-- NB:
-- (Discrete, i.e., actual) RI is not unimodal. 
-- Continuous (idealized) RI is convex, and is the best candidate for analysis, via gradient descent.

completeBuildP :: Monad m => Plant -> DDPProblem Build Float (Upgrade, Int) m
completeBuildP p = DDPProblem (enumSearch buildN) buildA
    where
    buildN :: Build -> Either Float [(Upgrade, Int)]
    buildN b = --seq (unsafePerformIO $ print b) $ 
            case mu of
            Nothing -> Left 0
            Just u  -> case u of
                PumpWater _ -> Right $ (u,) <$> [0..40]
                CircMult -> Left 1e99 --Right $ (u,) <$> [0..10] -- TODO properly gate research
                _ -> error "what"
        where
        mu = find (`notElem` M.keys (runBuild b)) $
             PumpWater (specPumpType $ plantSpec p) : [CircMult | specCirc (plantSpec p)]

    buildA b (u, uL) = do
        let b' = Build $ M.insert u uL (runBuild b)
        let (b'', dv) = completeCostFromTo (plantBuild p) $ p { plantBuild = b' }
        return (subtract dv, b'')

completeBuild :: Plant -> (Cell, Level) -> Level -> Level -> IO Build
completeBuild p (cT, cL) lL iL = do
        (_, _, b) <- sol
        return b
    where
    sol = solveDDP (completeBuildP p) (Build $ M.fromList
        [(CellHeat cT, cL)
        ,(CellLife cT, lL)
        ,(IsoMult, iL)])

completeBuild' :: Plant -> IO Plant
completeBuild' p = do
        b <- completeBuild p (cT, cL) lL iL
        return $ p { plantBuild = b }
    where
    cT = specCellType $ plantSpec p
    cL = M.findWithDefault 0 (CellHeat cT) (runBuild $ plantBuild p)
    iL = M.findWithDefault 0 IsoMult (runBuild $ plantBuild p)
    lL = M.findWithDefault 0 (CellLife cT) (runBuild $ plantBuild p)

specBuildAtHeat :: Float -> Spec -> Build -> (Float, Build)
specBuildAtHeat h s b = (cost (cL, iL) , Build $ M.unionWith max (runBuild b) $ M.fromList [(CellHeat cellType, cL), (IsoMult, iL)])
    where
    cellType = specCellType s
    cN = specCells s
    iN = specIsos s

    h' = h / fromIntegral cN
    ls = [(max 0 $ ceiling $ logBase 1.25 (h' / baseHeat cellType / isoMult iN iL), iL) | iL <- [0 .. if iN > 0 then 20 else 0]]
    cost (x, y) = buildCostFromTo b (Build $ M.fromList [(CellHeat cellType, x), (IsoMult, y)])
    (cL, iL) = head $ sortOn cost ls

plantNextUpgrades :: Plant -> (Spec, Int) -> [Build]
plantNextUpgrades p@(Plant b _ _) (s', n) =
    [snd $ specBuildAtHeat (h * m / fromIntegral n) s' b
    | m <- [1.05] ++ [1.118 ^ i | i <- [1 .. 30 :: Int]]]
    where
    h = plantHeat p

plantHeat :: Plant -> Float
plantHeat (Plant b s n) = fromIntegral n * specHeat b s

plantCellCost :: Plant -> Float
plantCellCost (Plant b s n) = fromIntegral n * specCellCost b s

specCellCost :: Build -> Spec -> Float
specCellCost b s = fromIntegral (specCells s) * cellCost cT / cellLife b cT
    where
    cT = specCellType s

cellLife :: Build -> Cell -> Float
cellLife b cT = 800 * 2 ^ M.findWithDefault 0 (CellLife cT) (runBuild b)

plantNetHeat :: Plant -> Float
plantNetHeat p = plantHeat p - plantCellCost p

specHeat :: Build -> Spec -> Float
specHeat b s = fromIntegral cN * cellHeat cellType cL * isoMult iN iL where
    upgrades = runBuild b
    cellType = specCellType s
    cN = specCells s
    cL = M.findWithDefault 0 (CellHeat cellType) upgrades
    iN = specIsos s
    iL = M.findWithDefault 0 IsoMult upgrades


data Plants = Island | Village | Region | City | SHC | Metro | FHC | Mainland | EHC | Continent
    deriving (Eq, Ord, Enum, Bounded, Show, Generic, Hashable)

boundedEnum :: (Enum a, Bounded a) => [a]
boundedEnum = enumFrom minBound

newtype Game = Game { runGame :: M.Map Plants Plant }

plantBuyCost :: Plants -> Float
plantBuyCost p = case p of
    Mainland -> 30e18
    _ -> 1e100

nextUpgrades :: Game -> [(Float, Plants, Plant)]
nextUpgrades g = [
    (0, pn, Plant b s n)
    | (pn, p) <- M.toList (runGame g)
    , sn@(s, n) <- M.findWithDefault [] pn specs
    , b <- plantNextUpgrades p sn
    ] ++ [
    (plantBuyCost pn, pn, Plant b s n)
    | pn <- boundedEnum :: [Plants], pn `M.notMember` runGame g
    , sn@(s, n) <- M.findWithDefault [] pn specs
    , b <- plantNextUpgrades (Plant mempty s n) sn
    ]

data UpgradeStats = UpgradeStats { upgradeTotalCost :: Float, upgradeEff :: Float, upgradeBuy :: Float, upgradeRet :: Float, heat0 :: Float, dHeat :: Float } deriving Eq

instance Ord UpgradeStats where
    compare x y = compare (upgradeEff x) (upgradeEff y)

instance Show UpgradeStats where
    show (UpgradeStats c e b r h0 dh) = printf "% 6.2f = % 6.2f + % 6.2f -> +% 8.2e (% 6.2f%%) for % 8.2e" e b r dh (100 * dh / h0) c


bestUpgrades :: Game -> IO [(UpgradeStats, Plants, Plant)]
bestUpgrades g = do
    let nu = nextUpgrades g
    let power = gamePower g

    nu' <- forM nu $ \ (pbc, pn, p) -> do
        p' <- completeBuild' p
        let stats = upgradeEfficiency power pbc (runGame g M.!? pn) p'
        return (stats, pn, p')

    let nu'' = sortOn (\ (hrs, _, _) -> hrs) nu'

    return $ nubOrdOn (\ (_, pn, _) -> pn) nu''


gamePower :: Game -> Float
gamePower g = sum $ fmap plantHeat $ M.elems $ runGame g

upgradeEfficiency :: Float -> Float -> Maybe Plant -> Plant -> UpgradeStats
upgradeEfficiency power pbc p q = UpgradeStats cost (buy + ret) buy ret h0 dh
    where
    buy = costH / power
    ret = costH / max 0 dh

    costH  = cost / 3600 / fromIntegral tps

    cost = plantCostFromTo p q + pbc
    h0   = maybe 0 plantNetHeat p
    dh   = plantNetHeat q - h0



plantCostFromTo :: Maybe Plant -> Plant -> Float
plantCostFromTo Nothing (Plant b' s' n') = buildCostTo b' + fromIntegral n' * specCost s'
plantCostFromTo (Just (Plant b s n)) (Plant b' s' n') = bC + pC
    where
    bC = buildCostFromTo b b'
    pC = max 0 $ fromIntegral n' * specCost s' - fromIntegral n * specCost s

specCost :: Spec -> Float
specCost s = let c = fromIntegral (specGens s) * genCost (specGenType s) in
    if specCirc s then c + fromIntegral (specGens s) / 2 * 250e15 else c -- average...

genCost :: Gen -> Float
genCost g = case g of
    Gen2 -> 2.5e6
    Gen3 -> 10e12
    Gen4 -> 50e15
    Gen5 -> 12.5e15 -- ???



-- specs
thermo111 :: Spec
thermo111 = Spec Thermo 1 Gen3 1 Pump 1 0 False

fusion111 :: Spec
fusion111 = Spec Fusion 1 Gen3 1 Pump 1 0 False

thermo122 :: Spec
thermo122 = Spec Thermo 1 Gen3 2 Pump 2 0 False

fusion122 :: Spec
fusion122 = Spec Fusion 1 Gen3 2 GroundPump 2 0 False

-- island
fusionG3_122 :: Spec
fusionG3_122 = Spec Fusion 1 Gen3 2 Pump 2 1 False

fusionG3_111 :: Spec
fusionG3_111 = Spec Fusion 1 Gen3 1 Pump 1 0 False

fusionG4_122 :: Spec
fusionG4_122 = Spec Fusion 1 Gen4 2 Pump 2 0 False

thoriumIsl :: Spec
thoriumIsl = Spec Thorium 1 Gen4 2 Pump 2 0 False
--

thoriumSHC :: Spec
thoriumSHC = Spec Thorium 1 Gen3 50 Pump 64 4 False

thoriumG4_SHC :: Spec
thoriumG4_SHC = Spec Thorium 1 Gen4 50 Pump 64 4 False

thorium1148 :: Spec
thorium1148 = Spec Thorium 1 Gen4 4 GroundPump 8 1 False

thorium1482 :: Spec
thorium1482 = Spec Thorium 1 Gen4 4 GroundPump 8 2 False

thorium1261 :: Spec
thorium1261 = Spec Thorium 1 Gen4 2 GroundPump 6 1 False

thorium1261C :: Spec
thorium1261C = Spec Thorium 1 Gen4 2 GroundPump 6 1 True

thorium1_4_12_1C :: Spec
thorium1_4_12_1C = Spec Thorium 1 Gen4 4 GroundPump 12 1 True

thoriumFHC :: Spec
thoriumFHC = Spec Thorium 1 Gen3 44 GroundPump 64 4 False

prot1_4_12_1C :: Spec
prot1_4_12_1C = Spec Protactium 1 Gen4 4 GroundPump 12 1 True

prot1261C :: Spec
prot1261C = Spec Protactium 1 Gen4 2 GroundPump 6 1 True


specs :: M.Map Plants [(Spec, Int)]
specs = M.fromList [
        (Island, [
            (fusionG3_122, 4),
            (fusionG3_111, 8),
            (fusionG4_122, 4),
            (thoriumIsl, 4)
        ]),
        (Village, [
            (fusion122, 11)
        ]),
        (Region, [
            --(fusion122, 18),
            (thorium1261, 9)
        ]),
        (City, [
            --(fusion122, 42),
            (thorium1482, 13),
            (thorium1261, 20),
            (thorium1_4_12_1C, 10),
            (thorium1261C, 19),
            (prot1261C, 19)
        ]),
        (SHC, [
            (thoriumSHC, 1),
            (thoriumG4_SHC, 1)
        ]),
        (Metro, [
            (thorium1148, 20),
            (thorium1482, 19),
            (thorium1261, 30),
            (thorium1261C, 27),
            (prot1261C, 26)
        ]),
        (FHC, [
            (thoriumFHC, 1),
            (thoriumG4_SHC, 1)
        ]),
        (Mainland, [
            (thorium1482, 22),
            (thorium1261, 33),
            (prot1_4_12_1C, 15),
            -- probably v
            (thorium1261C, 29),
            (prot1261C, 29)
        ])
    ]

-- plants
plantIsland :: Plant
plantIsland = Plant
    (Build $ M.fromList
        [(CellHeat Fusion, 0)
        ,(IsoMult, 4)
        ,(CellLife Fusion, 2)
        ,(GenEff, 68)
        ,(GenMaxWater, 31)
        ,(PumpWater Pump, 14)
        ,(ElemMaxWater, 16)]
    )
    fusionG3_122
    4

plantVillage :: Plant
plantVillage = Plant
    (Build $ M.fromList
        [(CellHeat Fusion, 9)
        ,(CellLife Fusion, 1)
        ,(GenEff, 74)
        ,(GenMaxWater, 39)
        ,(PumpWater GroundPump, 16)
        ,(ElemMaxWater, 18)])
    fusion122
    11

plantRegion :: Plant
plantRegion = Plant
    (Build $ M.fromList
        [(CellHeat Thorium, 7)
        ,(CellLife Thorium, 1)
        ,(IsoMult, 13)
        ,(GenEff, 82)
        ,(GenMaxWater, 50)
        ,(PumpWater GroundPump, 22)
        ,(ElemMaxWater, 24)])
    thorium1261
    9

{-
plantRegion :: Plant
plantRegion = Plant
    (Build $ M.fromList
        [(CellHeat Fusion, 10)
        ,(GenEff, 74)
        ,(GenMaxWater, 41)
        ,(PumpWater GroundPump, 17)
        ,(ElemMaxWater, 19)])
    fusion122
    18
-}

plantCity :: Plant
plantCity = Plant
    (Build $ M.fromList
        [(CellHeat Thorium, 9)
        ,(CellLife Thorium, 1)
        ,(IsoMult, 11)
        ,(GenEff, 87)
        ,(GenMaxWater, 50)
        ,(PumpWater GroundPump, 23)
        ,(ElemMaxWater, 25)])
    thorium1482
    13

{-
plantCity :: Plant
plantCity = Plant
    (Build $ M.fromList
        [(CellHeat Fusion, 14)
        ,(GenEff, 74)
        ,(GenMaxWater, 44)
        ,(PumpWater GroundPump, 19)
        ,(ElemMaxWater, 20)])
    fusion122 -- thorium1482
    42
-}


plantSHC :: Plant
plantSHC = Plant
    (Build $ M.fromList
        [(CellHeat Thorium, 3)
        ,(CellLife Thorium, 1)
        ,(IsoMult, 11)
        ,(GenEff, 78)
        ,(GenMaxWater, 42)
        ,(PumpWater Pump, 20)
        ,(ElemMaxWater, 20)])
    thoriumSHC
    1

plantMetro :: Plant
plantMetro = Plant
    (Build $ M.fromList
        [(CellHeat Thorium, 9)
        ,(CellLife Thorium, 1)
        ,(IsoMult, 12)
        ,(GenEff, 85)
        ,(GenMaxWater, 52)
        ,(PumpWater GroundPump, 23)
        ,(ElemMaxWater, 25)])
    thorium1261
    30

{-
plantMetro :: Plant
plantMetro = Plant
    (Build $ M.fromList
        [(CellHeat Thorium, 9)
        ,(IsoMult, 9)
        ,(GenEff, 85)
        ,(GenMaxWater, 49)
        ,(PumpWater GroundPump, 22)
        ,(ElemMaxWater, 24)])
    thorium1148
    20

plantMetro :: Plant
plantMetro = Plant
    (Build $ M.fromList
        [(CellHeat Thorium, 7)
        ,(IsoMult, 9)
        ,(GenEff, 79)
        ,(GenMaxWater, 48)
        ,(PumpWater GroundPump, 21)
        ,(ElemMaxWater, 22)])
    thorium1148
    20
-}

plantFHC :: Plant
plantFHC = Plant
    (Build $ M.fromList
        [(CellHeat Thorium, 4)
        ,(CellLife Thorium, 1)
        ,(IsoMult, 12)
        ,(GenEff, 78)
        ,(GenMaxWater, 44)
        ,(PumpWater GroundPump, 18)
        ,(ElemMaxWater, 20)])
    thoriumFHC
    1

--
game :: Game
game = Game $ M.fromList [
        (Island, plantIsland),
        (Village, plantVillage),
        (Region, plantRegion),
        (City, plantCity),
        (SHC, plantSHC),
        (Metro, plantMetro),
        (FHC, plantFHC)
    ]


buildDiff :: Build -> Build -> M.Map Upgrade (Int, Int)
buildDiff (Build a) (Build b) = M.mapWithKey f c
    where
    c = M.union a b
    f k _ = (M.findWithDefault 0 k a, M.findWithDefault 0 k b)


showBest :: IO ()
showBest = do
    best <- bestUpgrades game
    forM_ best $ \ (stats, pn, p') -> do
        let p = runGame game M.!? pn
        formatUpgrade p p' pn stats


formatUpgrade :: Maybe Plant -> Plant -> Plants -> UpgradeStats -> IO ()
formatUpgrade p p' pn stats = do
    let b = maybe mempty plantBuild p

    let diff' = buildDiff b (plantBuild p')
    let diff = unwords $ mapMaybe (\ (u, (a, b)) -> if a /= b then Just $ upgradeShortName u ++ show a ++ "->" ++ show b else Nothing) $ M.toList diff'
    
    let s1 = maybe "New" (show . plantSpec) p
    if fmap plantSpec p == Just (plantSpec p') then
        putStrLn $ printf "%-10s: %-50s, %-50s, %s" (show pn) (show stats) (show (plantSpec p')) diff
    else
        putStrLn $ printf "%-10s: %-50s, %-50s, %s" (show pn) (show stats) (s1 ++ " -> " ++ show (plantSpec p')) diff



data PlantStats = PlantStats { plantPowerT :: Float, plantCellCostT :: Float, plantPowerNetT :: Float, plantPowerH :: Float, plantBat :: Float }

instance Show PlantStats where
    show stats = printf "power/t = % 8.2e (% 8.2e), power/h = % 8.2e, bat = % 8.2e"
        (plantPowerT stats) (plantPowerNetT stats) (plantPowerH stats) (plantBat stats)

instance Semigroup PlantStats where
    (PlantStats x y z w v) <> (PlantStats x' y' z' w' v') = PlantStats (x + x') (y + y') (z + z') (w + w') (v + v')

instance Monoid PlantStats where
    mempty = PlantStats 0 0 0 0 0

plantStats :: Plant -> PlantStats
plantStats p = PlantStats h h' hn hh bat
    where
    hn = h - h'
    h  = plantHeat p
    h' = plantCellCost p
    hh = hn * 3600 * fromIntegral tps
    bat = hh * 10

showPower :: IO ()
showPower = do
    let stats = plantStats <$> runGame game
    let total = fold $ M.elems stats

    forM_ (M.toList stats) $ \ (pn, stats) -> do
        let frac = plantPowerNetT stats / plantPowerNetT total

        putStrLn $ printf "%-10s(%6.1f%%): %s" (show pn) (100 * frac) (show stats)
        -- show pn ++ printf "(%.1f%%): " (100 * frac) ++ show stats
    putStrLn $ "Total: " ++ show total

{-
stepGame :: StateT Game IO (Game, UpgradeStats, Plants, Plant, Plant)
stepGame = do
    g <- get

    nu <- lift $ bestUpgrades g
    let (stats, pn, p') = head nu
    let p = runGame g M.! pn

    let g' = Game $ M.insert pn p' $ runGame g

    put g'
    return (g', stats, pn, p, p')
-}

stepGame :: StateT Game IO (Game, UpgradeStats, Plants, Maybe Plant, Plant)
stepGame = do
    g <- get

    nu <- lift $ bestUpgrades g
    let (stats, pn, p') = head nu
    let p = runGame g M.!? pn

    let g' = Game $ M.insert pn p' $ runGame g

    put g'
    lift $ formatUpgrade p p' pn stats

    return (g', stats, pn, p, p')

stepsGame :: Int -> StateT Game IO [(Game, UpgradeStats, Plants, Maybe Plant, Plant)]
stepsGame n = replicateM n stepGame


showSteps :: IO ()
showSteps = do
    _ <- runStateT (stepsGame 20) game
    return ()

{-
showSteps :: IO ()
showSteps = do
    (xs, _) <- runStateT (stepsGame 10) game
    forM_ xs $ \ (_, u, pn, p, p') -> do
        formatUpgrade p p' pn u
-}


main :: IO ()
main = do
    putStrLn "Power:"
    showPower
    putStrLn ""

    putStrLn "Next upgrades:"
    showBest
    putStrLn ""

    putStrLn "Next steps:"
    showSteps
    putStrLn ""

-- when plantBuild plantMetro M.! CellHeat Thorium == 4., start researching for Gen4

-- TODO directly search for the best efficiency
-- -> from the current build exhaustively search + branch&bound

-- TODO research
-- -> if a local search hits an unresearched item, branch, and push the research to the queue
-- -> generate sequences of upgrades with R+ and without the research R-
-- -> check if T(R+; R- `union` R+) < T(R-; R- `union` R+)

-- TODO unfold a big upgrade into viable sub-upgrades

-- DEFER speed up completeBuild by using gradient descent
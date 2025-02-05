module RI.Data where

import RI.ReactorIdle
import qualified Data.Map as M
import Control.Monad.State
import Data.Foldable
import Text.Printf
import Data.Maybe
import Control.Monad

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
            (thorium1261, 20)--,
            --(thorium1_4_12_1C, 10),
            --(thorium1261C, 19),
            --(prot1261C, 19)
        ]),
        (SHC, [
            (thoriumSHC, 1),
            (thoriumG4_SHC, 1)
        ]),
        (Metro, [
            (thorium1148, 20),
            (thorium1482, 19),
            (thorium1261, 30)--,
            --(thorium1261C, 27),
            --(prot1261C, 26)
        ]),
        (FHC, [
            (thoriumFHC, 1),
            (thoriumG4_SHC, 1)
        ]),
        (Mainland, [
            (thorium1482, 22),
            (thorium1261, 33)--,
            --(prot1_4_12_1C, 15),
            -- probably v
            --(thorium1261C, 29),
            --(prot1261C, 29)
        ])
    ]

-- plants
plantIsland :: Plant
plantIsland = Plant
    (Build $ M.fromList
        [(CellHeat Fusion, 1)
        ,(IsoMult, 6)
        ,(CellLife Fusion, 2)
        ,(GenEff, 68)
        ,(GenMaxWater, 32)
        ,(PumpWater Pump, 15)
        ,(ElemMaxWater, 17)]
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
        [(CellHeat Thorium, 10)
        ,(CellLife Thorium, 1)
        ,(IsoMult, 14)
        ,(GenEff, 87)
        ,(GenMaxWater, 52)
        ,(PumpWater GroundPump, 24)
        ,(ElemMaxWater, 26)])
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
        [(CellHeat Thorium, 12)
        ,(CellLife Thorium, 2)
        ,(IsoMult, 14)
        ,(GenEff, 87)
        ,(GenMaxWater, 54)
        ,(PumpWater GroundPump, 25)
        ,(ElemMaxWater, 27)])
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

plantMainland :: Plant
plantMainland = Plant
    (Build $ M.fromList
        [(CellHeat Thorium, 10)
        ,(GenMaxWater, 52)
        ,(ElemMaxWater, 26)
        ,(PumpWater GroundPump, 24)
        ,(IsoMult, 14)]
    )
    thorium1482
    22

--
game :: Game
game = Game $ M.fromList [
        (Island, plantIsland),
        (Village, plantVillage),
        (Region, plantRegion),
        (City, plantCity),
        (SHC, plantSHC),
        (Metro, plantMetro),
        (FHC, plantFHC),
        (Mainland, plantMainland)
    ]



main :: IO ()
main = do
    putStrLn "Power:"
    showPower
    putStrLn ""

    putStrLn "Next upgrades:"
    forM_ (gameBest' specs game) $ \ (u, pn, q) -> do
        let p = gamePlant game M.!? pn
        formatUpgrade p q pn u
    putStrLn ""

    putStrLn "Next steps:"
    showSteps
    putStrLn ""

showPower :: IO ()
showPower = do
    let stats = plantStats <$> gamePlant game
    let total = fold $ M.elems stats

    forM_ (M.toList stats) $ \ (pn, stats) -> do
        let frac = plantPowerNetT stats / plantPowerNetT total

        putStrLn $ printf "%-10s(%6.1f%%): %s" (show pn) (100 * frac) (show stats)
        -- show pn ++ printf "(%.1f%%): " (100 * frac) ++ show stats
    putStrLn $ "Total: " ++ show total

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

stepGame :: StateT Game IO (Game, UpgradeStats, Plants, Maybe Plant, Plant)
stepGame = do
    g <- get

    let (stats, pn, p') = gameBest specs g
    let p = gamePlant g M.!? pn

    let g' = Game $ M.insert pn p' $ gamePlant g

    put g'
    lift $ formatUpgrade p p' pn stats

    return (g', stats, pn, p, p')

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

buildDiff :: Build -> Build -> M.Map Upgrade (Int, Int)
buildDiff (Build a) (Build b) = M.mapWithKey f c
    where
    c = M.union a b
    f k _ = (M.findWithDefault 0 k a, M.findWithDefault 0 k b)

stepsGame :: Int -> StateT Game IO [(Game, UpgradeStats, Plants, Maybe Plant, Plant)]
stepsGame n = replicateM n stepGame

showSteps :: IO ()
showSteps = do
    _ <- runStateT (stepsGame 20) game
    return ()


{-



showBest :: IO ()
showBest = do
    best <- bestUpgrades game
    forM_ best $ \ (stats, pn, p') -> do
        let p = gamePlant game M.!? pn
        formatUpgrade p p' pn stats

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


{-
showSteps :: IO ()
showSteps = do
    (xs, _) <- runStateT (stepsGame 10) game
    forM_ xs $ \ (_, u, pn, p, p') -> do
        formatUpgrade p p' pn u
-}
-}
{-
-- when plantBuild plantMetro M.! CellHeat Thorium == 4., start researching for Gen4

-- TODO directly search for the best efficiency
-- -> from the current build exhaustively search + branch&bound

-- TODO research
-- -> if a local search hits an unresearched item, branch, and push the research to the queue
-- -> generate sequences of upgrades with R+ and without the research R-
-- -> check if T(R+; R- `union` R+) < T(R-; R- `union` R+)

-- TODO unfold a big upgrade into viable sub-upgrades

-- DEFER speed up completeBuild by using gradient descent
-}

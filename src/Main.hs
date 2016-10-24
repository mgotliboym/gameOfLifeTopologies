{-# LANGUAGE ScopedTypeVariables #-}

--{-# LANGUAGE TypeFamilies #-}

module Main where
import Control.Comonad
import Control.Concurrent
import System.Console.ANSI
import Text.Printf
--import Debug.Trace
--import Control.Comonad.Env
import Data.List
import Data.Maybe

import Class
import InfPlane
import Torus
import Klein
import BoardCreation

--Projective Plane
data PlaneZipProj a = PlaneZipProj (ZpMobius (ZpMobius a))
instance Functor PlaneZipProj where
  fmap f (PlaneZipProj zs) = PlaneZipProj $ fmap (fmap f) zs

--io stuff
gameOfLifeRule :: (PlaneZip p z1 z2) => p z1 z2 Bool -> Bool
gameOfLifeRule p =
  let numNeighbors = sum $ (map fromEnum) $ concat $ getTrueSquare p
    in if (extract p) then numNeighbors==3 || numNeighbors==4 --one more than standard, since it counts the live cell itself
       else numNeighbors==3

makeAnyLifeRule :: (PlaneZip p z1 z2) => [Int] -> [Int] -> p z1 z2 Bool -> Bool
makeAnyLifeRule birth survival p =
  let numNeighbors = sum $ (map fromEnum) $ concat $ getTrueSquare p
  in if extract p then
       (numNeighbors-1) `elem` survival
     else
       numNeighbors `elem` birth

{-prettyShow :: (Show a) => ZpMobius a -> String
prettyShow (ZpMobius n as x bs) = show n ++ (unlines $ map show as) ++ "\n" ++ show x ++ "\n" ++ (unlines $ map show bs) -}

showGame :: (PlaneZip p z1 z2) => p z1 z2 Bool -> String
showGame p =
  let xs = getSquare 80 p
    in unlines $ map (\(n,unnumberedLines) -> (printf "%03d" n)++unnumberedLines++(printf "%03d" n)) $ zip [(1::Int)..] $ map (map (\x -> if x then 'X' else ' ')) xs


main :: IO ()
main = do --putStrLn $ show $ fst $ survivesOrAnnihilatesExtraInfo $ emptyExBoard  $ torusFromList $ vertWallBoard 5 2045
  let results = map (\x -> (survivesOrAnnihilatesExtraInfo . emptyExBoard . torusFromList $ vertWallBoard 5 x, x) ) [7,11..147]--[253,249]--[4..2050] --257]
  putStrLn $ unlines $ map (show . \((a,_),c) -> (a,c) ) results
{-main :: IO ()
main = do
  let results = map (\x -> (survivesOrAnnihilatesExtraInfo . emptyExBoard . torusFromList $ vertWallBoard 5 x, x) ) [113]--[253,249]--[4..2050] --257]
  putStrLn $ unlines $ map (show . \((a,_),c) -> (a,c) ) results
  let maxColsClusters = foldl (\(_,board) (dat,_) -> getMaxInfo $ countAliveClusters $ countAliveCols $ (dat,board)) (emptyExBoard $ snd $ fst $ head results) (map fst results)
      maxFuse = foldr (\(dat,_) m -> max m (fuseLength dat)) 0 (map fst results)
      maxCycle = foldr (\(dat,_) m -> max m (timeToCycle dat)) 0 (map fst results)
      minFuse = foldr (\(dat,_) m -> min m (fuseLength dat)) 999999 (map fst results)
      maxPeriod = foldr (\(dat,_) m -> max m (timeToCycle dat) - (fuseLength dat)) 0 (map fst results)
      allClusterSizes = foldr (\(dat,_) gs -> gs `union` seenClusterSizes dat) [] (map fst results)
      mostClusterSizes = foldr (\(dat,_) gs -> if (length $ seenClusterSizes dat) > (length gs) then seenClusterSizes dat else gs) [] (map fst results)
  putStrLn "Summary Results:"
  putStrLn $ "maxColsClusters: " ++ show maxColsClusters
  putStrLn $ "maxFuse: " ++ show maxFuse
  putStrLn $ "maxCycle: " ++ show maxCycle
  putStrLn $ "minFuse: " ++ show minFuse
  putStrLn $ "maxPeriod: " ++ show maxPeriod
  putStrLn $ "allClusterSizes: " ++ show allClusterSizes
  putStrLn $ "mostClusterSizes: " ++ show mostClusterSizes
-}
  --main' 300000 startBoardTorusW
  --where
runGame :: (PlaneZip p z1 z2) => Int -> p z1 z2 Bool -> IO ()
runGame speed board = main' 1 speed board
main' :: (PlaneZip p z1 z2) => Int -> Int -> p z1 z2 Bool -> IO ()
main' n speed board = do
  --clearScreen
  putStrLn $ show n
  putStrLn $ showGame board
  threadDelay speed 
  main' (n+1) speed $ extend gameOfLifeRule board

data BoardResult = Survived | Annihilated deriving (Show)
data ExtraStats = ExtraStats {
  aliveColumns :: Int, --assumes vert wall board on a torus/klein
  maxAliveColumns :: Int, --assumes vert wall board on a torus/klein
  aliveClusters :: Int, --assumes vert wall board on a torus/klein
  maxAliveClusters :: Int, --assumes vert wall board on a torus/klein
  timeToCycle :: Int, --starts at 1, the timeToCycle-th generation is the first repeated one
  survivedAnnihilated :: BoardResult,
  fuseLength :: Int, --starts at 1, the fuseLength-th generation is the one which is repeated after timeToCycle-th generations.
  --After the fuseLength-th generation, there will be (timetoCycle-fuseLength) unique generations in every cycle
  seenClusterSizes :: [Int]
  } deriving (Show)
type ExBoard p = (ExtraStats, p Bool)

emptyExBoard :: p Bool -> ExBoard p
emptyExBoard board = (ExtraStats{aliveColumns=0,maxAliveColumns=0,
                                 aliveClusters=0,maxAliveClusters=0,
                                 timeToCycle=0,survivedAnnihilated=Survived,
                                 fuseLength=0,seenClusterSizes=[]}, board)

updateBoard :: (PlaneZip p z1 z2) => ExBoard (p z1 z2) -> ExBoard (p z1 z2)
updateBoard (dat, board) =
  let board' = (dat, extend gameOfLifeRule board)
  in board' `seq` getMaxInfo $ countAliveClusters $ countAliveCols board'

countAliveCols :: (PlaneZip p z1 z2) => ExBoard (p z1 z2) -> ExBoard (p z1 z2)
countAliveCols (dat, board) =
  let total = sum $ map fromEnum $ head $ getFullBoard board
  in (dat{aliveColumns=total},board)

countAliveClusters :: (PlaneZip p z1 z2) => ExBoard (p z1 z2) -> ExBoard (p z1 z2)
countAliveClusters (dat, board) =
  let clusters = group $ map fromEnum $ head $ getFullBoard board
      clusters' = if length clusters >=2 && (head (head clusters) == 1 && head (head clusters) == head (last clusters)) --first cluster and last cluster are alive, so they're connected on torus
                  then (last clusters++(head clusters)) : (tail clusters)
                  else clusters
      total = sum $ map head $ clusters' --counts the clusters
      sizes = map length $ filter ((==1) . head) clusters' --gets the size of each cluster of live cells
      sizes' = union (seenClusterSizes dat) sizes --dedups sizes, and removes elements already in seen, and merges
  in (dat{aliveClusters=total,seenClusterSizes=sizes'},board)

--must call after count to take the max with an updated count, as in updateBaord
getMaxInfo :: ExBoard a -> ExBoard a
getMaxInfo (dat, board) =
  (dat{maxAliveColumns=max (aliveColumns dat) (maxAliveColumns dat),
       maxAliveClusters=max (aliveClusters dat) (maxAliveClusters dat)} , board)

survivesOrAnnihilates :: forall p z1 z2. PlaneZip p z1 z2 => p z1 z2 Bool -> Bool
survivesOrAnnihilates board =
  let emptyBoard = map (map $ const False) $ getFullBoard board
      
      f :: [[[Bool]]] -> [p z1 z2 Bool] -> Bool
      f boardsSoFar (nextBoard:rest) =
        let boolBoard = getFullBoard nextBoard in
        if boolBoard == emptyBoard then
          False --trace ("iterations until result" ++ (show $ length boardsSoFar)) False
          else if boolBoard `elem` boardsSoFar then
            True --trace ("iterations until result" ++ (show $ length boardsSoFar)) True
          else f (boolBoard:boardsSoFar) rest
      f _ [] = undefined --it's an infinite list
  in f [] $ iterate (extend gameOfLifeRule) board

survivesOrAnnihilatesExtraInfo :: forall p z1 z2. PlaneZip p z1 z2 => ExBoard (p z1 z2) -> ExBoard (p z1 z2)
survivesOrAnnihilatesExtraInfo exboard@(_, board) =
  let emptyBoard = map (map $ const False) $ getFullBoard board
      f :: [[[Bool]]] -> [ExBoard (p z1 z2)] -> ExBoard (p z1 z2)
      f boardsSoFar ((dat',nextBoard):rest) =
        let boolBoard = getFullBoard nextBoard in
        if boolBoard == emptyBoard then
          --trace ("iterations until result" ++ (show $ length boardsSoFar)) False
          (dat'{survivedAnnihilated=Annihilated, timeToCycle=length boardsSoFar,
                seenClusterSizes=sort $ seenClusterSizes dat'},nextBoard)
          else if boolBoard `elem` boardsSoFar then
            (dat'{survivedAnnihilated=Survived, timeToCycle=length boardsSoFar,
                 fuseLength=length boardsSoFar - (fromJust $ elemIndex boolBoard boardsSoFar) - 1,
                 seenClusterSizes=sort $ seenClusterSizes dat'},nextBoard)
          else f (boolBoard:boardsSoFar) rest
      f _ [] = undefined --it's an infinite list
  in f [] $ iterate updateBoard exboard

survivesOrAnnihilatesExtraInfoTest :: forall p z1 z2. PlaneZip p z1 z2 => ExBoard (p z1 z2) -> ExBoard (p z1 z2)
survivesOrAnnihilatesExtraInfoTest exboard@(_, board) =
  let emptyBoard = map (map $ const False) $ getFullBoard board
      f :: ExBoard (p z1 z2) -> ExBoard (p z1 z2)
      f full@(dat',nextBoard) =
        let boolBoard = getFullBoard nextBoard in
        if boolBoard == emptyBoard then
          --trace ("iterations until result" ++ (show $ length boardsSoFar)) False
          (dat'{survivedAnnihilated=Annihilated, timeToCycle=0,
                seenClusterSizes=sort $ seenClusterSizes dat'},nextBoard)
        else full `seq` f $ updateBoard full      
  in f exboard

data Interval a = Interval a a deriving (Show, Eq)--closed interval

{-getNumComponents :: PlaneZip p z1 z2 => ExBoard (p z1 z2) -> ExBoard (p z1 z2)
getNumComponents (dat,board) =
  let aliveCoordinates :: Integral a => [[a]] --board to coordinates of live cells in each row
      aliveCoordinates = map (\row -> map snd $ filter (\(b,_) -> b) $ zip row [1..]) $ getFullBoard board 
      aliveCooGrouped :: Integral a => [[Interval a]] -- list of start/end coordinates of clusters of live cells in each row (no torus/klein consideration)
      aliveCooGrouped = map (tail . reverse . foldl (\full@(Interval a b:result) x -> if x==b+1 then Interval a x:result else Interval x x:full) [Interval (-1) (-1)]) aliveCoordinates
      --now treat alive coo grouped as a graph, where there may be edges up/down, and each tuple is a vertex
      aliveCooGrouped' :: Integral a => [[(Interval a,a)]] --snd in each pair is number of component, also head is nonempty, has at least one live cell-block
      aliveCooGrouped'= (\x -> zip (head x) [1..] : map (`zip` repeat (-1)) (tail x)) $ dropWhile (== []) aliveCooGrouped
      aliveCooGroupedTorus :: Integral a => ListZipTorus (ListZipTorus (Interval a, a))
      aliveCooGroupedTorus = ListZipTorus [] (ListZipTorus [] (head $ head aliveCooGrouped') (tail $ head aliveCooGrouped')) 
                                             (map (\(r:row) -> ListZipTorus [] r row) $ tail aliveCooGrouped')
      labelComponets :: Integral a => ListZipTorus (ListZipTorus (Interval a, a)) -> ListZipTorus (ListZipTorus (Interval a, a))
    in emptyExBoard board-}


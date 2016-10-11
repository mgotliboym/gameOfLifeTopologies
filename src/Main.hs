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
  let xs = getSquare 40 p
    in unlines $ map (\(n,unnumberedLines) -> (printf "%03d" n)++unnumberedLines++(printf "%03d" n)) $ zip [(1::Int)..] $ map (map (\x -> if x then 'X' else ' ')) xs
    
main :: IO ()
main = do
  let results = map (\x -> (survivesOrAnnihilatesExtraInfo . emptyExBoard . torusFromList $ vertWallBoard 5 x, x) ) [181]--[4..2050] --257]
  putStrLn $ unlines $ map (show . \((a,_),c) -> (a,c) ) results
  let maxColsGroups = foldl (\(_,board) (dat,_) -> getMaxInfo $ countAliveGroups $ countAliveCols $ (dat,board)) (emptyExBoard $ snd $ fst $ head results) (map fst results)
      maxFuse = foldr (\(dat,_) m -> max m (fuseLength dat)) 0 (map fst results)
      maxCycle = foldr (\(dat,_) m -> max m (timeToCycle dat)) 0 (map fst results)
      minFuse = foldr (\(dat,_) m -> min m (fuseLength dat)) 999999 (map fst results)
      maxPeriod = foldr (\(dat,_) m -> max m (timeToCycle dat) - (fuseLength dat)) 0 (map fst results)
      allGroupSizes = foldr (\(dat,_) gs -> gs `union` seenGroupSizes dat) [] (map fst results)
      mostGroupSizes = foldr (\(dat,_) gs -> if (length $ seenGroupSizes dat) > (length gs) then seenGroupSizes dat else gs) [] (map fst results)
  putStrLn "Summary Results:"
  putStrLn $ "maxColsGroups: " ++ show maxColsGroups
  putStrLn $ "maxFuse: " ++ show maxFuse
  putStrLn $ "maxCycle: " ++ show maxCycle
  putStrLn $ "minFuse: " ++ show minFuse
  putStrLn $ "maxPeriod: " ++ show maxPeriod
  putStrLn $ "allGroupSizes: " ++ show allGroupSizes
  putStrLn $ "mostGroupSizes: " ++ show mostGroupSizes
  
  --main' 300000 startBoardTorusW
  --where
runGame :: (PlaneZip p z1 z2) => Int -> p z1 z2 Bool -> IO ()
runGame speed board = main' 1 speed board
main' :: (PlaneZip p z1 z2) => Int -> Int -> p z1 z2 Bool -> IO ()
main' n speed board = do
  clearScreen
  putStrLn $ show n
  putStrLn $ showGame board
  threadDelay speed 
  main' (n+1) speed $ extend gameOfLifeRule board

data BoardResult = Survived | Annihilated deriving (Show)
data ExtraStats = ExtraStats {
  aliveColumns :: Int, --assumes vert wall board on a torus/klein
  maxAliveColumns :: Int, --assumes vert wall board on a torus/klein
  aliveGroups :: Int, --assumes vert wall board on a torus/klein
  maxAliveGroups :: Int, --assumes vert wall board on a torus/klein
  timeToCycle :: Int, --starts at 1, the timeToCycle-th generation is the first repeated one
  survivedAnnihilated :: BoardResult,
  fuseLength :: Int, --starts at 1, the fuseLength-th generation is the one which is repeated after timeToCycle-th generations.
  --After the fuseLength-th generation, there will be (timetoCycle-fuseLength) unique generations in every cycle
  seenGroupSizes :: [Int]
  } deriving (Show)
type ExBoard p = (ExtraStats, p Bool)

emptyExBoard :: p Bool -> ExBoard p
emptyExBoard board = (ExtraStats{aliveColumns=0,maxAliveColumns=0,
                                 aliveGroups=0,maxAliveGroups=0,
                                 timeToCycle=0,survivedAnnihilated=Survived,
                                 fuseLength=0,seenGroupSizes=[]}, board)

updateBoard :: (PlaneZip p z1 z2) => ExBoard (p z1 z2) -> ExBoard (p z1 z2)
updateBoard (dat, board) =
  let board' = (dat, extend gameOfLifeRule board)
  in getMaxInfo $ countAliveGroups $ countAliveCols board'

countAliveCols :: (PlaneZip p z1 z2) => ExBoard (p z1 z2) -> ExBoard (p z1 z2)
countAliveCols (dat, board) =
  let total = sum $ map fromEnum $ head $ getFullBoard board
  in (dat{aliveColumns=total},board)

countAliveGroups :: (PlaneZip p z1 z2) => ExBoard (p z1 z2) -> ExBoard (p z1 z2)
countAliveGroups (dat, board) =
  let groups = group $ map fromEnum $ head $ getFullBoard board
      groups' = if length groups >=2 && (head (head groups) == 1 && head (head groups) == head (last groups)) --first group and last group are alive, so they're connected on torus
                  then (last groups++(head groups)) : (tail groups)
                  else groups
      total = sum $ map head $ groups' --counts the groups
      sizes = map length $ filter ((==1) . head) groups' --gets the size of each group of live cells
      sizes' = union (seenGroupSizes dat) sizes --dedups sizes, and removes elements already in seen, and merges
  in (dat{aliveGroups=total,seenGroupSizes=sizes'},board)

--must call after count to take the max with an updated count, as in updateBaord
getMaxInfo :: ExBoard a -> ExBoard a
getMaxInfo (dat, board) =
  (dat{maxAliveColumns=max (aliveColumns dat) (maxAliveColumns dat),
       maxAliveGroups=max (aliveGroups dat) (maxAliveGroups dat)} , board)

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
          (dat'{survivedAnnihilated=Annihilated, timeToCycle=length boardsSoFar + 1,
                seenGroupSizes=sort $ seenGroupSizes dat'},nextBoard)
          else if boolBoard `elem` boardsSoFar then
            (dat'{survivedAnnihilated=Survived, timeToCycle=length boardsSoFar + 1,
                 fuseLength=length boardsSoFar - (fromJust $ elemIndex boolBoard boardsSoFar),
                 seenGroupSizes=sort $ seenGroupSizes dat'},nextBoard)
          else f (boolBoard:boardsSoFar) rest
      f _ [] = undefined --it's an infinite list
  in f [] $ iterate updateBoard exboard

data Interval a = Interval a a deriving (Show, Eq)--closed interval

{-getNumComponents :: PlaneZip p z1 z2 => ExBoard (p z1 z2) -> ExBoard (p z1 z2)
getNumComponents (dat,board) =
  let aliveCoordinates :: Integral a => [[a]] --board to coordinates of live cells in each row
      aliveCoordinates = map (\row -> map snd $ filter (\(b,_) -> b) $ zip row [1..]) $ getFullBoard board 
      aliveCooGrouped :: Integral a => [[Interval a]] -- list of start/end coordinates of groups of live cells in each row (no torus/klein consideration)
      aliveCooGrouped = map (tail . reverse . foldl (\full@(Interval a b:result) x -> if x==b+1 then Interval a x:result else Interval x x:full) [Interval (-1) (-1)]) aliveCoordinates
      --now treat alive coo grouped as a graph, where there may be edges up/down, and each tuple is a vertex
      aliveCooGrouped' :: Integral a => [[(Interval a,a)]] --snd in each pair is number of component, also head is nonempty, has at least one live cell-block
      aliveCooGrouped'= (\x -> zip (head x) [1..] : map (`zip` repeat (-1)) (tail x)) $ dropWhile (== []) aliveCooGrouped
      aliveCooGroupedTorus :: Integral a => ListZipTorus (ListZipTorus (Interval a, a))
      aliveCooGroupedTorus = ListZipTorus [] (ListZipTorus [] (head $ head aliveCooGrouped') (tail $ head aliveCooGrouped')) 
                                             (map (\(r:row) -> ListZipTorus [] r row) $ tail aliveCooGrouped')
      labelComponets :: Integral a => ListZipTorus (ListZipTorus (Interval a, a)) -> ListZipTorus (ListZipTorus (Interval a, a))
    in emptyExBoard board-}

{-
annihilated n, timetoannihilation
3 4 5 6 7 8
14 15 16
30 31 32
61 62 63 64
121 125 126 127 128-}

-- 3, 4, 5, 6, 7, 8, 14, 15, 16, 30, 31, 32, 61, 62, 63, 64, 121, 125, 126, 127, 128

--watch: main' 400000 $ kleinFromList $ diagWallBoard 32
--putStrLn $ unlines $ map (\x -> show (fst . survivesOrAnnihilatesExtraInfo . emptyExBoard . torusFromList . vertWallBoard $ x, x) ) [4..257]

{-

X        X



X        X
X        X

-----------

_        _
X        X


X        X
 X      X 
-}


{-
timeToCycle: n where it occurs. Ignore prev not with.
65535: 114,115,116
16383: 138,139,140
8191:  90, 91, 92
4095:  98, 99, 100
2047:  74,75,76,106,107,108
1023:
255: (50,51,52 - prev not with) 82,83,84
127: 42,43,44,(121 not with but annihilated) 122,123,124,130,131,132
65: 126,127,128 (annihilated)
31: (26,27,28 - prev not with),34,35,36
15: 18,19,20
7:  10,11,12
3:  -}


{- n=timetocycle=2^k+1, fuse length, max alive columns, max alive groups
mapM_ print $ zip (map (fst . survivesOrAnnihilatesExtraInfo . emptyExBoard . torusFromList . vertWallBoard 5 . (+1) . (2^)) [3..12]) [3..12]
col 2:
2^k+1 -> 3*2^{k-1} - 2
n     -> 3/2*(n-1)-2

col 3: n -> n-5
col 4: 2^k+1 -> 3*2^{k-2}
col 5: 2^{k-2}

always have exactly groups of sizes 1,2,3,6

9,    10,    4,    6,  2
17,   22,   12,   12,  4
33,   46,   28,   24,  8
65,   94,   60,   48,  .
129,  190,  124,  96,  .
257,  382,  252,  192, .
513,  766,  508,  384, .
1023, 1534, 1020, 768, .
-}


--161: (ExtraStats {aliveColumns = 24, maxAliveColumns = 118, aliveGroups = 24, maxAliveGroups = 48, timeToCycle = 85408, survivedAnnihilated = Survived, fuseLength = 69032, seenGroupSizes = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,26,32,36]

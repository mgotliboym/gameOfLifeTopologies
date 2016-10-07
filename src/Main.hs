--{-# LANGUAGE TypeFamilies #-}

module Main where
import Control.Comonad
import Control.Concurrent
import System.Console.ANSI
import Text.Printf
--import Debug.Trace
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
gameOfLifeRule :: (PlaneZip p) => p Bool -> Bool
gameOfLifeRule p =
  let numNeighbors = sum $ (map fromEnum) $ concat $ getTrueSquare p
    in if (getSelf p) then numNeighbors==3 || numNeighbors==4 --one more than standard, since it counts the live cell itself
       else numNeighbors==3

makeAnyLifeRule :: (PlaneZip p) => [Int] -> [Int] -> p Bool -> Bool
makeAnyLifeRule birth survival p =
  let numNeighbors = sum $ (map fromEnum) $ concat $ getTrueSquare p
  in if getSelf p then
       (numNeighbors-1) `elem` survival
     else
       numNeighbors `elem` birth

prettyShow :: (Show a) => ZpMobius a -> String
prettyShow (ZpMobius n as x bs) = show n ++ (unlines $ map show as) ++ "\n" ++ show x ++ "\n" ++ (unlines $ map show bs)

showGame :: (PlaneZip p) => p Bool -> String
showGame p =
  let xs = getSquare 40 p
    in unlines $ map (\(n,unnumberedLines) -> (printf "%03d" n)++unnumberedLines++(printf "%03d" n)) $ zip [(1::Int)..] $ map (map (\x -> if x then 'X' else ' ')) xs
    
main :: IO ()
main = do
  main' 300000 startBoardTorusW
  --where
main' :: (PlaneZip p) => Int -> p Bool -> IO ()
main' speed board = do
  clearScreen
  putStrLn $ showGame board
  threadDelay speed 
  main' speed $ extend gameOfLifeRule board


{-prob = last $ iterateN 18 (extend gameOfLifeRule) $ kleinFromList $ padBoardCenter 11 11 glider
prob' = last $ iterateN 10 boundedLeftMvKlein prob
prob'' = last $ iterateN 5 (\(PlaneZipKlein p) -> PlaneZipKlein $ leftMv p) prob'
p = last $ iterateN 6 (\(PlaneZipKlein p) -> PlaneZipKlein $ leftMv p) prob'-}

--watch: main' 400000 $ kleinFromList $ diagWallBoard 32

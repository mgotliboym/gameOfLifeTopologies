{-# LANGUAGE MultiParamTypeClasses, KindSignatures, FlexibleInstances #-}

module Torus where

import Control.Comonad
import Class
import BoardCreation

data ListZipTorus a = ListZipTorus [a] a [a] deriving (Show,Eq)
instance ListZip ListZipTorus where
  left (ListZipTorus as _ _) = as
  focus (ListZipTorus _ x _) = x
  right (ListZipTorus _ _ bs) = bs
  build = ListZipTorus
  leftMv = leftMvTorus
  rightMv = rightMvTorus

leftMvTorus :: ListZipTorus a -> ListZipTorus a
leftMvTorus (ListZipTorus [] c []) = ListZipTorus [] c []
leftMvTorus (ListZipTorus (a:as) c bs) = ListZipTorus as a (c:bs)
leftMvTorus (ListZipTorus [] c bs) =
  let (r:rbs) = reverse (c:bs)
      in ListZipTorus (rbs) r [] --rbs r [c]
rightMvTorus :: ListZipTorus a -> ListZipTorus a
rightMvTorus (ListZipTorus [] c []) = ListZipTorus [] c []
rightMvTorus (ListZipTorus as c (b:bs)) = ListZipTorus (c:as) b bs
rightMvTorus (ListZipTorus as c []) =
  let (r:ras) = reverse (c:as)
      in ListZipTorus [] r (ras) --[c] r ras

iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = take n $ tail $ iterate f x

instance Functor ListZipTorus where
  fmap = listZipFmap 
instance Comonad ListZipTorus where
  extract = listZipExtract
  duplicate a = ListZipTorus
      (iterateN (length $ left a) leftMv a) a
      (iterateN (length $ right a) rightMv a)

data PlaneZipTorus z1 z2 a = PlaneZipTorus (z1 (z2 a)) deriving (Show)

innerDuplicateN :: ListZipTorus (ListZipTorus a) -> ListZipTorus (ListZipTorus (ListZipTorus a))
innerDuplicateN a = build  (iterateN (length $ left $ focus a) (fmap leftMv) a) a
                           (iterateN (length $ right $ focus a) (fmap rightMv) a) 

instance Functor (PlaneZipTorus ListZipTorus ListZipTorus) where
  fmap f (PlaneZipTorus zs) = PlaneZipTorus $ fmap (fmap f) zs --listzip fmap
instance Comonad (PlaneZipTorus ListZipTorus ListZipTorus) where
  extract (PlaneZipTorus zs) = extract $ extract zs --listzipinf extract
  --duplicate :: PlaneZipTorus a -> PlaneZipTorus (PlaneZipTorus a)
  duplicate (PlaneZipTorus p) =
    PlaneZipTorus $ (fmap $ fmap $ PlaneZipTorus) $ innerDuplicateN $ innerDuplicateN p

instance PlaneZip PlaneZipTorus ListZipTorus ListZipTorus where
  unwrap (PlaneZipTorus zs) = zs
  getTrueSquare (PlaneZipTorus zs) =
    let (top:mid:bottom:[]) = getRow zs
      in getRow top : getRow mid : getRow bottom : []
    where getRow xs = (focus $ leftMv xs) : (focus xs) : (focus $ rightMv xs) : []
  
torusFromList :: [[Bool]] -> PlaneZipTorus ListZipTorus ListZipTorus Bool
torusFromList start =
  let midH = length start `div` 2
      midW = (length $ head start) `div` 2
      torus = map (\row -> let (as,(x:bs)) = splitAt midW row in ListZipTorus (reverse as) x bs) start
      (as', x':bs') = splitAt midH torus
  in PlaneZipTorus $ ListZipTorus (reverse as') x' bs'


startBoardTorus :: PlaneZipTorus ListZipTorus ListZipTorus Bool
startBoardTorus =
  PlaneZipTorus $ ListZipTorus
    ((ListZipTorus (replicate 5 False) True (replicate 5 False)) :
       replicate 4 (ListZipTorus (replicate 5 False) False (replicate 5 False)))
    (ListZipTorus (replicate 5 False) False (True : replicate 4 False))
    ((ListZipTorus (True : replicate 4 False) True (True : replicate 4 False)) :
       replicate 4 (ListZipTorus (replicate 5 False) False (replicate 5 False)))

startBoardTorusW :: PlaneZipTorus ListZipTorus ListZipTorus Bool
startBoardTorusW =
  PlaneZipTorus $ ListZipTorus
    ((ListZipTorus (replicate 5 False ++ True:replicate 4 False) False (replicate 10 False)) : --row above middle
       replicate 9 (ListZipTorus (replicate 5 False ++ True:replicate 4 False) False (replicate 10 False))) --top 9 rows
    (ListZipTorus (replicate 5 False ++ True:replicate 4 False) False (False : replicate 9 False)) --middle
    ((ListZipTorus (False : replicate 4 False ++ True:replicate 4 False) False (False : replicate 9 False)) : --row below middle
       replicate 9 (ListZipTorus (replicate 5 False ++ True:replicate 4 False) False (replicate 10 False))) --bottom middle rows

startBoardGliderWallCorner :: PlaneZipTorus ListZipTorus ListZipTorus Bool
startBoardGliderWallCorner =
  torusFromList $ padBoardCenter 15 15 $ padBoardTRCorner 6 6 glider `orBoards` diagWallBoard 15

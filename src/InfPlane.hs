{-# LANGUAGE MultiParamTypeClasses, KindSignatures, FlexibleInstances #-}

module InfPlane where

import Control.Comonad
import Class

data ListZipInf a = ListZipInf ([a]) a ([a]) deriving (Show,Eq)
instance ListZip ListZipInf where
  left (ListZipInf as _ _) = as
  focus (ListZipInf _ x _) = x
  right (ListZipInf _ _ bs) = bs
  build = ListZipInf
  leftMv = leftMvInf
  rightMv = rightMvInf

leftMvInf :: ListZipInf a -> ListZipInf a
leftMvInf (ListZipInf (a:as) c bs) = ListZipInf as a (c:bs)
leftMvInf (ListZipInf [] _ _) = undefined
rightMvInf :: ListZipInf a -> ListZipInf a
rightMvInf (ListZipInf as c (b:bs)) = ListZipInf (c:as) b bs
rightMvInf (ListZipInf _ _ []) = undefined

instance Functor ListZipInf where
  fmap = listZipFmap 
instance Comonad ListZipInf where
  extract = listZipExtract
  duplicate a = ListZipInf (tail (iterate leftMv a)) a
                           (tail (iterate rightMv a))
  --extend f a = fmap f (duplicate a)

  
--data PlaneZipInf a = PlaneZipInf (ListZipInf (ListZipInf a)) deriving (Show)
data PlaneZipInf z1 z2 a = PlaneZipInf (z1 (z2 a)) deriving Eq
--instance Functor (PlaneZipInf ((z :: * -> *) (z1 :: * -> *))) where
--instance Functor PlaneZipInf where
instance Functor (PlaneZipInf ListZipInf ListZipInf) where
  fmap f (PlaneZipInf zs) = PlaneZipInf $ fmap (fmap f) zs --listzip fmap
instance Comonad (PlaneZipInf ListZipInf ListZipInf) where
  extract (PlaneZipInf zs) = focus $ focus zs --listzipinf extract
  --duplicate :: PlaneZipInf a -> PlaneZipInf (PlaneZipInf a)
  duplicate (PlaneZipInf p) =
    PlaneZipInf $ (fmap $ fmap $ PlaneZipInf) $ innerDuplicate $ innerDuplicate p --note this is a ListZip fmap.
  --Could simplify using PlaneZip fmap: (fmap PlaneZipInf) $ PlaneZipInf $ innerDuplicate $ innerDuplicate p

innerDuplicate :: ListZipInf (ListZipInf a) -> ListZipInf (ListZipInf (ListZipInf a))
innerDuplicate a = build  (tail (iterate (fmap leftMv) a)) a
                          (tail (iterate (fmap rightMv) a)) 


instance PlaneZip PlaneZipInf ListZipInf ListZipInf where
  unwrap (PlaneZipInf zs) = zs
  getTrueSquare board = getSquare 1 board --(PlaneZipInf zs) = zipTake2 1 zs

infPlaneFromList :: [[Bool]] -> PlaneZipInf ListZipInf ListZipInf Bool
infPlaneFromList start =
  let midH = length start `div` 2
      midW = (length $ head start) `div` 2
      plane = map (\row -> let (as,(x:bs)) = splitAt midW row in ListZipInf (reverse as ++ repeat False) x (bs++repeat False)) start
      (as', x':bs') = splitAt midH plane
      emptyRow = ListZipInf (repeat False) False (repeat False)
  in PlaneZipInf $ ListZipInf (reverse as' ++ repeat emptyRow) x' (bs' ++ repeat emptyRow)


--getTrueSquare' PlaneZipInf 
--getTrueSquare' (PlaneZipInf zs) = zipTake2 1 zs


{-

{ -30 -20 -10 0 10
  -3  -2  -1  0 1
  -30 -20 -10 0 10}
{ -20 -10 0 10 20
  -2  -1  0 1  2
  -20 -10 0 10 20}
{ -10 0 10 20 30
  -1  0 1  2 3
  -10 0 10 20 30}


{ -3   -2   -1   0 1
  -30  -20  -10  0 10
  -300 -200 -100 0 100}
{ -2   -1   0 1   2
  -20  -10  0 10  20
  -200 -100 0 100 200}
{ -1   0 1   2   3
  -10  0 10  20  30
  -100 0 100 200 300}
-}

startBoardInf :: PlaneZipInf ListZipInf ListZipInf Bool
startBoardInf =
  PlaneZipInf $ ListZipInf
    ((ListZipInf (repeat False) True (repeat False)) :
       repeat (ListZipInf (repeat False) False (repeat False)))
    (ListZipInf (repeat False) False (True : repeat False))
    ((ListZipInf (True : repeat False) True (True : repeat False)) :
       repeat (ListZipInf (repeat False) False (repeat False)))

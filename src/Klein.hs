module Klein where

import Control.Comonad
import Class
import Torus
import Debug.Trace

---klein bottle
data ZpMobius a = ZpMobius {
  index :: Int, -- start at 1
  leftMob :: [a],
  focMob :: a,
  rightMob :: [a] } deriving (Show, Eq)
instance Functor ZpMobius where
  fmap f z@(ZpMobius n _ _ _) = (listZipFmap f z){index=n} --ZpMobius n (map f as) (f x) (map f bs)
instance ListZip ZpMobius where
  left = leftMob
  right = rightMob
  focus = focMob
  build = ZpMobius (-1)
  leftMv (ZpMobius n (a:as) x bs) = ZpMobius n as a (x:bs)
  leftMv z@(ZpMobius _ [] _ _) = z
  rightMv (ZpMobius n as x (b:bs)) = ZpMobius n (x:as) b bs
  rightMv z@(ZpMobius _ _ _ []) = z

data PlaneZipKlein a = PlaneZipKlein (ListZipTorus (ZpMobius a)) deriving (Show,Eq)

--mapZp :: (a -> b) -> ZpMobius a -> ZpMobius b
--mapZp f (ZpMobius n as x bs) = ZpMobius n (map f as) (f x) (map f bs)
lastZp :: ZpMobius a -> a
lastZp z@(ZpMobius _ _ _ (_:bs)) = last bs -- last $ right z
lastZp (ZpMobius _ _ x []) = x
headZp :: ZpMobius a -> a
headZp z@(ZpMobius _ (_:as) _ _) = last as --last $ left z
headZp (ZpMobius _ [] x _) = x

instance Functor PlaneZipKlein where
  fmap f (PlaneZipKlein zs) = PlaneZipKlein $ fmap (fmap f) zs --listzip fmap, first inf then mob

boundedLeftMvKlein :: PlaneZipKlein p -> PlaneZipKlein p
boundedLeftMvKlein (PlaneZipKlein zs) =
  PlaneZipKlein $ fmap leftMv zs
boundedRightMvKlein :: PlaneZipKlein p -> PlaneZipKlein p
boundedRightMvKlein (PlaneZipKlein zs) =
  PlaneZipKlein $ fmap rightMv zs
getLeftElemKlein :: PlaneZipKlein a -> a
getLeftElemKlein p = getLRElemKlein True p
getRightElemKlein :: PlaneZipKlein a -> a
getRightElemKlein p = getLRElemKlein False p
getLRElemKlein :: Bool -> PlaneZipKlein a -> a --True is left
getLRElemKlein lr (PlaneZipKlein zs) =
  if lr then
    case (focus zs) of
    ZpMobius _ (a:_) _ _ -> a
    ZpMobius n [] _ _    -> lastZp $ newRow n -- trace ("row index: " ++ (show $ newIndex n)) $ newRow n -- $ trace ("row: " ++ (show $ newRow n)) $ newRow n
  else
    case (focus zs) of
    ZpMobius _ _ _ (b:_) -> b
    ZpMobius n _ _ []    -> headZp $ newRow n -- trace ("row index: " ++ (show $ newIndex n)) $ newRow n 
  where newIndex n = (length $ left zs) + 1 + (length $ right zs) - n
        newRow n =
          if newIndex n < length (left zs) then
            left zs !! --(trace ("left, len: " ++ (show $ length $ left zs) ++ ": want index: " ++ (show $ ((length $ left zs) - newIndex n - 1) )) $
                        ((length $ left zs) - newIndex n - 1) --) --recall left is a reversed list
            else if length (left zs) == newIndex n then -- middle
                   focus zs
                   else right zs !! --(trace ("right, len: " ++ (show $ length $ right zs) ++ " want index: " ++ (show $ newIndex n - (length $ left zs) - 2)) $ 
                                     (newIndex n - (length $ left zs) - 1)-- )

instance Comonad PlaneZipKlein where
  extract (PlaneZipKlein zs) = focus $ extract zs --listzipTorus extract
  duplicate p' =
    PlaneZipKlein $ dupKleinTorus $ dupKlein p'
    where 
      dupKleinTorus :: ZpMobius (PlaneZipKlein a) -> ListZipTorus (ZpMobius (PlaneZipKlein a))
      dupKleinTorus z@(ZpMobius _ _ (PlaneZipKlein x) _) = ListZipTorus
        (iterateN (length $ left x) 
          (reFocusNum . fmap (\(PlaneZipKlein p) -> PlaneZipKlein $ leftMv p) ) $ z) --this is mobius fmap
        z
        (iterateN (length $ right x)
          (reFocusNum . fmap (\(PlaneZipKlein p) -> PlaneZipKlein $ rightMv p) ) $ z) --also mobius fmap
        where
          reFocusNum :: ZpMobius (PlaneZipKlein a) -> ZpMobius (PlaneZipKlein a)
          reFocusNum z'@(ZpMobius _ _ (PlaneZipKlein p) _) = z'{index = index $ focus p}
  
dupKlein :: PlaneZipKlein a -> ZpMobius (PlaneZipKlein a)
dupKlein p@(PlaneZipKlein zs) =
  let (ZpMobius n as _ bs) = focus zs in
    ZpMobius n (iterateN (length as) boundedLeftMvKlein p) p
             (iterateN (length bs) boundedRightMvKlein p)
        
instance PlaneZip PlaneZipKlein where
  getTrueSquare p@(PlaneZipKlein zs) =
    getZpRow (PlaneZipKlein $ leftMv zs) : getZpRow p : getZpRow (PlaneZipKlein $ rightMv zs) : []
    where getZpRow p' =
            getLeftElemKlein p' : extract p' : getRightElemKlein p' : []
  getSquare n (PlaneZipKlein zs) =
    map (zipTake n) $ zipTake n zs
  getSelf p = extract p

kleinFromList :: [[Bool]] -> PlaneZipKlein Bool
kleinFromList start =
  let midH = length start `div` 2
      midW = (length $ head start) `div` 2
      torus = map (\(n,row) -> let (as,(x:bs)) = splitAt midW row in ZpMobius n (reverse as) x bs) $ zip [1..] start
      (as', x':bs') = splitAt midH torus
  in PlaneZipKlein $ ListZipTorus (reverse as') x' bs'
     

startBoardKleinG :: PlaneZipKlein Bool
startBoardKleinG =
  let top3 = take 3 $ iterate (\z -> z{index=index z + 1}) (ZpMobius 1 (replicate 10 False) False (replicate 10 False))
      top = (ZpMobius 4 (replicate 10 False) True (replicate 10 False))
      mid = (ZpMobius 5 (replicate 10 False) False (True:replicate 9 False))
      bot = (ZpMobius 6 (True:replicate 9 False) True (True:replicate 9 False))
      bottom4 = take 4 $ iterate (\z -> z{index=index z + 1}) (ZpMobius 7 (replicate 10 False) False (replicate 10 False))
    in PlaneZipKlein $ ListZipTorus (top:reverse top3) mid (bot:bottom4)

startBoardKlein' :: PlaneZipKlein Int
startBoardKlein' =
  let board = take 6 $ iterate (\(ZpMobius n as x bs) -> ZpMobius (n+1) (map (*10) as) (10*x) (map (*10) bs)) (ZpMobius 1 (reverse $ take 3 [1..]) 4 (take 3 [5..]))
      (xs,y:ys) = splitAt 3 board
    in PlaneZipKlein $ ListZipTorus (reverse xs) y ys


vertWallBoardKlein :: Int -> PlaneZipKlein Bool
vertWallBoardKlein n =
  if n `mod` 2 == 1 then
    kleinFromList $ replicate (min 20 n) $ replicate (n `div` 2) False ++ True:replicate (n `div` 2) False
  else
    kleinFromList $ replicate (min 20 n) $ replicate ((n `div` 2)-1) False ++ True:replicate (n `div` 2) False

{-15 16
30 31 32
64 65 -}

--{-# LANGUAGE TypeFamilies #-}

module Main where
import Control.Comonad
import Control.Concurrent
import System.Console.ANSI
import Text.Printf
--import Debug.Trace

class (Functor z) => ListZip z where
  left :: z a -> [a]
  focus :: z a -> a
  right :: z a -> [a]
  build :: [a] -> a -> [a] -> z a
  leftMv :: z a -> z a
  rightMv :: z a -> z a

listZipFmap :: (ListZip z) => (a -> b) -> z a -> z b
listZipFmap f z = build (map f $ left z) (f $ focus z) (map f $ right z)
listZipExtract :: (ListZip z) => z a -> a
listZipExtract z = focus z

data ListZipInf a = ListZipInf ([a]) a ([a]) deriving (Show)
instance ListZip ListZipInf where
  left (ListZipInf as _ _) = as
  focus (ListZipInf _ x _) = x
  right (ListZipInf _ _ bs) = bs
  build = ListZipInf
  leftMv = leftMvInf
  rightMv = rightMvInf

leftMvInf :: ListZipInf a -> ListZipInf a
leftMvInf (ListZipInf (a:as) c bs) = ListZipInf as a (c:bs)
rightMvInf :: ListZipInf a -> ListZipInf a
rightMvInf (ListZipInf as c (b:bs)) = ListZipInf (c:as) b bs

instance Functor ListZipInf where
  fmap = listZipFmap 
instance Comonad ListZipInf where
  extract = listZipExtract
  duplicate a = ListZipInf (tail (iterate leftMv a)) a
                           (tail (iterate rightMv a))
  --extend f a = fmap f (duplicate a)

  
data PlaneZipInf a = PlaneZipInf (ListZipInf (ListZipInf a)) deriving (Show)
instance Functor PlaneZipInf where
  fmap f (PlaneZipInf zs) = PlaneZipInf $ fmap (fmap f) zs --listzip fmap
instance Comonad PlaneZipInf where
  extract (PlaneZipInf zs) = extract $ extract zs --listzipinf extract
  --duplicate :: PlaneZipInf a -> PlaneZipInf (PlaneZipInf a)
  duplicate (PlaneZipInf p) =
    PlaneZipInf $ (fmap $ fmap $ PlaneZipInf) $ innerDuplicate $ innerDuplicate p --note this is a ListZip fmap.
  --Could simplify using PlaneZip fmap: (fmap PlaneZipInf) $ PlaneZipInf $ innerDuplicate $ innerDuplicate p

innerDuplicate :: ListZipInf (ListZipInf a) -> ListZipInf (ListZipInf (ListZipInf a))
innerDuplicate a = build  (tail (iterate (fmap leftMv) a)) a
                          (tail (iterate (fmap rightMv) a)) 

zipTake :: ListZip z => Int -> z a -> [a]
zipTake n z = (reverse $ take n $ left z) ++ focus z : (take n $ right z)

zipTake2 :: ListZip z => Int -> z (z a) -> [[a]]
zipTake2 n z = zipTake n $ fmap (zipTake n) z

showN :: (ListZip z, Show (z a), Show a) => Int -> z (z a) -> String
showN n z = show $ zipTake2 n z
                         
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

--torus
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
  let (r:rbs) = reverse bs
      in ListZipTorus rbs r [c]
rightMvTorus :: ListZipTorus a -> ListZipTorus a
rightMvTorus (ListZipTorus [] c []) = ListZipTorus [] c []
rightMvTorus (ListZipTorus as c (b:bs)) = ListZipTorus (c:as) b bs
rightMvTorus (ListZipTorus as c []) =
  let (r:ras) = reverse as
      in ListZipTorus [c] r ras

iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = take n $ tail $ iterate f x

instance Functor ListZipTorus where
  fmap = listZipFmap 
instance Comonad ListZipTorus where
  extract = listZipExtract
  duplicate a = ListZipTorus
      (iterateN (length $ left a) leftMv a) a
      (iterateN (length $ right a) rightMv a)

data PlaneZipTorus a = PlaneZipTorus (ListZipTorus (ListZipTorus a)) deriving (Show)

innerDuplicateN :: ListZipTorus (ListZipTorus a) -> ListZipTorus (ListZipTorus (ListZipTorus a))
innerDuplicateN a = build  (iterateN (length $ left $ focus a) (fmap leftMv) a) a
                           (iterateN (length $ right $ focus a) (fmap rightMv) a) 

instance Functor PlaneZipTorus where
  fmap f (PlaneZipTorus zs) = PlaneZipTorus $ fmap (fmap f) zs --listzip fmap
instance Comonad PlaneZipTorus where
  extract (PlaneZipTorus zs) = extract $ extract zs --listzipinf extract
  --duplicate :: PlaneZipTorus a -> PlaneZipTorus (PlaneZipTorus a)
  duplicate (PlaneZipTorus p) =
    PlaneZipTorus $ (fmap $ fmap $ PlaneZipTorus) $ innerDuplicateN $ innerDuplicateN p

class (Comonad p) => PlaneZip p where
  getTrueSquare :: p a -> [[a]]
  getSquare :: Int -> p a -> [[a]]
  getSelf :: p a -> a
instance PlaneZip PlaneZipInf where
  getTrueSquare (PlaneZipInf zs) = zipTake2 1 zs
  getSquare n (PlaneZipInf zs) = zipTake2 n zs
  getSelf (PlaneZipInf zs) = focus $ focus $ zs
instance PlaneZip PlaneZipTorus where
  getTrueSquare (PlaneZipTorus zs) =
    let (top:mid:bottom:[]) = getRow zs
      in getRow top : getRow mid : getRow bottom : []
    where getRow xs = (focus $ leftMv xs) : (focus xs) : (focus $ rightMv xs) : []
  getSquare n (PlaneZipTorus zs) = zipTake2 n zs
  getSelf (PlaneZipTorus zs) = focus $ focus $ zs


---klein bottle
type Zp a = (Int, ([a], a, [a])) -- start int at 1
data PlaneZipKlein a = PlaneZipKlein (ListZipTorus (Zp a)) deriving (Show,Eq)

mapZp :: (a -> b) -> Zp a -> Zp b
mapZp f (n,(as, x, bs)) = (n,(map f as, f x, map f bs))
lastZp :: Zp a -> a
lastZp (_,(_,_,_:bs)) = last bs
lastZp (_,(_,x,[])) = x
headZp :: Zp a -> a
headZp (_,(_:as,_,_)) = last as
headZp (_,([],x,_)) = x

instance Functor PlaneZipKlein where
  fmap f (PlaneZipKlein zs) = PlaneZipKlein $ fmap (mapZp f) zs --listzip fmap

leftZp :: Zp a -> Zp a
leftZp (n,(a:as,x,bs)) = (n,(as,a,x:bs))
leftZp (n,([], x, bs)) = (n,([], x, bs))

rightZp :: Zp a -> Zp a
rightZp (n,(as,x,b:bs)) = (n,(x:as,b,bs))
rightZp (n,(as, x, [])) = (n,(as, x, []))

boundedLeftMvKlein :: PlaneZipKlein p -> PlaneZipKlein p
boundedLeftMvKlein (PlaneZipKlein zs) =
  PlaneZipKlein $ fmap leftZp zs
boundedRightMvKlein :: PlaneZipKlein p -> PlaneZipKlein p
boundedRightMvKlein (PlaneZipKlein zs) =
  PlaneZipKlein $ fmap rightZp zs
getLeftElemKlein :: PlaneZipKlein a -> a
getLeftElemKlein p = getLRElemKlein True p
getRightElemKlein :: PlaneZipKlein a -> a
getRightElemKlein p = getLRElemKlein False p
getLRElemKlein :: Bool -> PlaneZipKlein a -> a --True is left
getLRElemKlein lr (PlaneZipKlein zs) =
  if lr then
    case (focus zs) of
    (_,(a:_,_,_)) -> a
    (n,([],_,_))  -> lastZp $ newRow n
  else
    case (focus zs) of
    (_,(_,_,b:_)) -> b
    (n,(_,_,[]))  -> headZp $ newRow n
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
  extract (PlaneZipKlein zs) = (\(_,(_,x,_))->x) $ extract zs --listzipTorus extract
  duplicate p' =
    PlaneZipKlein $ dupKleinTorus $ dupKlein p'
    where 
      dupKleinTorus :: Zp (PlaneZipKlein a) -> ListZipTorus (Zp (PlaneZipKlein a))
      dupKleinTorus z@(_,(_,PlaneZipKlein x,_)) = ListZipTorus
        (iterateN (length $ left x) 
          (reFocusNum . mapZp (\(PlaneZipKlein p) -> PlaneZipKlein $ leftMv p) ) $ z)
        z
        (iterateN (length $ right x)
          (reFocusNum . mapZp (\(PlaneZipKlein p) -> PlaneZipKlein $ rightMv p) ) $ z)
        where
          reFocusNum :: Zp (PlaneZipKlein a) -> Zp (PlaneZipKlein a)
          reFocusNum (_,(as,PlaneZipKlein x',bs)) = (fst $ focus x',(as,PlaneZipKlein x',bs))
  
dupKlein :: PlaneZipKlein a -> Zp (PlaneZipKlein a)
dupKlein p@(PlaneZipKlein zs) =
  let (n,(as,_,bs)) = focus zs in
  (n, (iterateN (length as) boundedLeftMvKlein p,p,
   iterateN (length bs) boundedRightMvKlein p))
        
instance PlaneZip PlaneZipKlein where
  getTrueSquare p@(PlaneZipKlein zs) =
    getZpRow (PlaneZipKlein $ leftMv zs) : getZpRow p : getZpRow (PlaneZipKlein $ rightMv zs) : []
    where getZpRow p' =
            getLeftElemKlein p' : extract p' : getRightElemKlein p' : []
  getSquare n (PlaneZipKlein zs) =
    map (\(_,(as,x,bs)) -> (reverse $ take n as) ++ x : (take n bs)) $ zipTake n zs
  getSelf p = extract p


--Projective Plane
data PlaneZipProj a = PlaneZipProj (Zp (Zp a))
instance Functor PlaneZipProj where
  fmap f (PlaneZipProj zs) = PlaneZipProj $ mapZp (mapZp f) zs


--io stuff
gameOfLifeRule :: (PlaneZip p) => p Bool -> Bool
gameOfLifeRule p =
  let numNeighbors = sum $ (map fromEnum) $ concat $ getTrueSquare p
    in if (getSelf p) then numNeighbors==3 || numNeighbors==4 --one more than standard, since it counts the live cell itself
       else numNeighbors==3

prettyShow :: (Show a) => Zp a -> String
prettyShow (n,(as,x,bs)) = show n ++ (unlines $ map show as) ++ "\n" ++ show x ++ "\n" ++ (unlines $ map show bs)

showGame :: (PlaneZip p) => p Bool -> String
showGame p =
  let xs = getSquare 20 p
    in unlines $ map (\(n,unnumberedLines) -> (printf "%03d" n)++unnumberedLines++(printf "%03d" n)) $ zip [(1::Int)..] $ map (map (\x -> if x then 'X' else ' ')) xs

startBoardInf :: PlaneZipInf Bool
startBoardInf =
  PlaneZipInf $ ListZipInf
    ((ListZipInf (repeat False) True (repeat False)) :
       repeat (ListZipInf (repeat False) False (repeat False)))
    (ListZipInf (repeat False) False (True : repeat False))
    ((ListZipInf (True : repeat False) True (True : repeat False)) :
       repeat (ListZipInf (repeat False) False (repeat False)))
    
startBoardTorus :: PlaneZipTorus Bool
startBoardTorus =
  PlaneZipTorus $ ListZipTorus
    ((ListZipTorus (replicate 5 False) True (replicate 5 False)) :
       replicate 4 (ListZipTorus (replicate 5 False) False (replicate 5 False)))
    (ListZipTorus (replicate 5 False) False (True : replicate 4 False))
    ((ListZipTorus (True : replicate 4 False) True (True : replicate 4 False)) :
       replicate 4 (ListZipTorus (replicate 5 False) False (replicate 5 False)))

startBoardKleinG :: PlaneZipKlein Bool
startBoardKleinG =
  let top3 = take 3 $ iterate (\(n,(as,x,bs))->(n+1,(as,x,bs))) (1,((replicate 10 False), False, (replicate 10 False)))
      top = (4,((replicate 10 False), True, (replicate 10 False)))
      mid = (5,((replicate 10 False), False, True:(replicate 9 False)))
      bot = (6,(True:(replicate 9 False), True, True:(replicate 9 False)))
      bottom4 = take 4 $ iterate (\(n,(as,x,bs))->(n+1,(as,x,bs))) (7,((replicate 10 False), False, (replicate 10 False)))
    in PlaneZipKlein $ ListZipTorus (top:top3) mid (bot:bottom4)

startBoardKlein' :: PlaneZipKlein Int
startBoardKlein' =
  let board = take 6 $ iterate (\(n,(as,x,bs))->(n+1,(map (*10) as,10*x,map (*10) bs))) (1,(reverse $ take 3 [1..], 4, take 3 [5..]))
      (xs,y:ys) = splitAt 3 board
    in PlaneZipKlein $ ListZipTorus (reverse xs) y ys
    

main :: IO ()
main = do
  main' startBoardKleinG
  where
    main' :: (PlaneZip p) => p Bool -> IO ()
    main' board = do
      clearScreen
      putStrLn $ showGame board
      threadDelay 1000000
      main' $ extend gameOfLifeRule board

module BoardCreation where

--import Class

padBoardCenter :: Int -> Int -> [[Bool]] -> [[Bool]]
padBoardCenter height width start =
  let (above,extraH) = (height - length start) `divMod` 2
      (side,extraW) = (width - length (head start)) `divMod` 2
      start' = map (\row -> (replicate side False) ++ row ++ (replicate (side+extraW) False)) start
      emptyRow = replicate width False
  in replicate above emptyRow ++ start' ++ replicate (above+extraH) emptyRow

padBoardTRCorner :: Int -> Int -> [[Bool]] -> [[Bool]]
padBoardTRCorner height width start =
  let start' = map (\row -> (replicate (width - (length $ head start)) False) ++ row) start
  in start' ++ replicate (height - length start) (replicate width False)

padBoardBLCorner :: Int -> Int -> [[Bool]] -> [[Bool]]
padBoardBLCorner height width start =
  let start' = map (\row -> row ++ (replicate (width - (length $ head start)) False)) start
  in replicate (height - length start) (replicate width False) ++ start'

{-orBoards :: [[Bool]] -> [[Bool]] -> [[Bool]]
orBoards = map (map (\(x,y) -> x || y)) `dot` (map $ \(a,b) -> zip a b) `dot` zip
  where infixr 5 `dot`
        dot :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
        dot = ((.).(.))
-}
orBoards :: [[Bool]] -> [[Bool]] -> [[Bool]]
orBoards = zipWith (zipWith (||))

diagWallBoard :: Int -> [[Bool]]
diagWallBoard n = map (\x -> replicate x False ++ True:replicate (n-1-x) False) [0,1..(n-1)]

diagWallBoardSlopeHalf :: Int -> [[Bool]]
diagWallBoardSlopeHalf n = map (\x -> replicate (2*x) False ++ True:True:replicate (n-2-(2*x)) False) [0,1..((n-1) `div` 2)]

vertWallBoard :: Int -> [[Bool]]
vertWallBoard n =
  if n `mod` 2 == 1 then
    replicate n $ replicate (n `div` 2) False ++ True:replicate (n `div` 2) False
  else
    replicate n $ replicate ((n `div` 2)-1) False ++ True:replicate (n `div` 2) False

setCenterFalse :: [[Bool]] -> [[Bool]]
setCenterFalse board =
  let (as, (b:bs)) = splitAt (length board `div` 2) board
      (as', (_:bs')) = splitAt (length b `div` 2) b
    in as ++ (as' ++ (False:bs')):bs

glider :: [[Bool]]
glider = [[False, True, False],[False, False, True],[True,True,True]]

upGlider :: [[Bool]]
upGlider = reverse glider

--pt :: Int -> [Int]
--pt 0=[1]; pt n=zipWith (+) (0:pt (n-1)) (pt (n-1)++[0])

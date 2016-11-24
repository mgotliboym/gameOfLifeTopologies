module BoardCreation where

--------------------------------------------------------------------
-- board operations
--------------------------------------------------------------------

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

appendBoardsRight :: [[Bool]] -> [[Bool]] -> [[Bool]]
appendBoardsRight = zipWith (++)

repeatBoardRight :: [[Bool]] -> Int -> [[Bool]]
repeatBoardRight board 1 = board
repeatBoardRight _ n | n<=0 = undefined
repeatBoardRight board n = appendBoardsRight board $ repeatBoardRight board $ n-1

--------------------------------------------------------------------
--Specific types of boards
--------------------------------------------------------------------

deadBoard :: Int -> Int -> [[Bool]]
deadBoard height width =
  replicate height $ replicate width False

diagWallBoard :: Int -> [[Bool]]
diagWallBoard n = map (\x -> replicate x False ++ True:replicate (n-1-x) False) [0,1..(n-1)]

--diagWallBoardSlopeHalf :: Int -> [[Bool]]
--diagWallBoardSlopeHalf n = map (\x -> replicate (2*x) False ++ True:True:replicate (n-2-(2*x)) False) [0,1..((n-1) `div` 2)]

vertWallBoard :: Int -> Int -> [[Bool]]
vertWallBoard height width =
  replicate height $
    let leftLen = 
           if width `mod` 2 == 1 then
             width `div` 2
           else
             (width `div` 2)-1
    in replicate leftLen False ++ True:replicate (width `div` 2) False

horizWallBoard :: Int -> Int -> [[Bool]]
horizWallBoard height width =
  let emptyRow = replicate width False
      leftLen = 
        if height `mod` 2 == 1 then
          height `div` 2
        else
          (height `div` 2)-1
  in replicate leftLen emptyRow ++ (replicate width True):replicate (height `div` 2) emptyRow

vertWallBoardSquare :: Int -> [[Bool]]
vertWallBoardSquare n = vertWallBoard n n
horizWallBoardSquare :: Int -> [[Bool]]
horizWallBoardSquare n = horizWallBoard n n


setCenterFalse :: [[Bool]] -> [[Bool]]
setCenterFalse board = --doesn't work for even boards
  let (as, (b:bs)) = splitAt (length board `div` 2) board
      (as', (_:bs')) = splitAt (length b `div` 2) b
    in as ++ (as' ++ (False:bs')):bs

--------------------------------------------------------------------
-- Specific boards
--------------------------------------------------------------------

glider :: [[Bool]]
glider = [[False, True, False],[False, False, True],[True,True,True]]

upGlider :: [[Bool]]
upGlider = reverse glider


rPentomino :: [[Bool]]
rPentomino = [[False,True,True],[True,True,False],[False,True,False]]

--pt :: Int -> [Int]
--pt 0=[1]; pt n=zipWith (+) (0:pt (n-1)) (pt (n-1)++[0])

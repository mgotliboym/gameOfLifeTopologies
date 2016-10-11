{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, KindSignatures, FlexibleInstances #-}

module Class where

import Control.Comonad

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

zipTake :: ListZip z => Int -> z a -> [a]
zipTake n z = (reverse $ take n $ left z) ++ focus z : (take n $ right z)

{-zipTake2 :: ListZip z => Int -> z (z a) -> [[a]]
zipTake2 n z = zipTake n $ fmap (zipTake n) z

showN :: (ListZip z, Show (z a), Show a) => Int -> z (z a) -> String
showN n z = show $ zipTake2 n z -}

class (Comonad (p z1 z2), ListZip z1, ListZip z2) =>
  PlaneZip (p :: (* -> *) -> (* -> *) -> * -> *)
           (z1:: * -> *) (z2:: * -> *) | p -> z1 z2 where
    unwrap :: p z1 z2 a -> z1 (z2 a)
    getTrueSquare :: p z1 z2 a -> [[a]]
  
getSquare :: PlaneZip p z1 z2 => Int -> p z1 z2 a -> [[a]]
getSquare n board =
  map (zipTake n) $ zipTake n $ unwrap board
getHeight :: PlaneZip p z1 z2 => p z1 z2 a -> Int
getHeight board =
  let unwrapped = unwrap board
  in length (left unwrapped) + 1 + length (right unwrapped)
getWidth :: PlaneZip p z1 z2 => p z1 z2 a -> Int
getWidth board =
  let unwrappedInner = focus $ unwrap board
  in length (left unwrappedInner) + 1 + length (right unwrappedInner)
     
getFullBoard :: (PlaneZip p z1 z2) => p z1 z2 a -> [[a]]
getFullBoard board = getSquare (max (getHeight board) (getWidth board)) board

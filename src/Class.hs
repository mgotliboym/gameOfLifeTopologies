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

zipTake2 :: ListZip z => Int -> z (z a) -> [[a]]
zipTake2 n z = zipTake n $ fmap (zipTake n) z

showN :: (ListZip z, Show (z a), Show a) => Int -> z (z a) -> String
showN n z = show $ zipTake2 n z


class (Comonad p) => PlaneZip p where
  getTrueSquare :: p a -> [[a]]
  getSquare :: Int -> p a -> [[a]]
  getSelf :: p a -> a

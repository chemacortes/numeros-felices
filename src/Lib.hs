module Lib
  ( digitosDe
  , digitosDe2
  , digitosDe3
  , sumaCuadradosDigitos
  , esFeliz
  , sonFelices
  )
where

import           Data.Tuple                     ( swap )


digitosDe :: Integer -> [Integer]
digitosDe = digitosDe3

digitosDe1 :: Integer -> [Integer]
digitosDe1 = reverse . digitosDe1'
 where
  digitosDe1' x | x < 10    = [x]
                | otherwise = m : digitosDe1' d
    where (d, m) = x `divMod` 10

digitosDe2 :: Integer -> [Integer]
digitosDe2 = digitosDe2' []
 where
  digitosDe2' xs x | x < 10    = x : xs
                   | otherwise = digitosDe2' (m : xs) d
    where (d, m) = x `divMod` 10

digitosDe3 :: Integer -> [Integer]
digitosDe3 x = [ read [c] | c <- show x ]


sumaCuadradosDigitos :: Integer -> Integer
sumaCuadradosDigitos = sum . map (^ 2) . digitosDe


esFeliz :: Integer -> Bool
esFeliz = esFelizAc []
 where
  esFelizAc xs x | x `elem` xs = False
                 | s == 1      = True
                 | otherwise   = esFelizAc (x : xs) s
    where s = sumaCuadradosDigitos x


sonFelices :: [Integer] -> ([Integer], [Integer])
sonFelices = sonFelicesAc ([], []) []
 where
  sonFelicesAc (felices, infelices) _ [] = (felices, infelices)
  sonFelicesAc (felices, infelices) acc (x : xs)
    | x `elem` felices = sonFelicesAc (felices ++ acc, infelices) [] xs
    | x `elem` infelices || x `elem` acc = sonFelicesAc
      (felices, infelices ++ acc)
      []
      xs
    | s == 1 = sonFelicesAc (felices ++ (x : acc), infelices) [] xs
    | otherwise = sonFelicesAc (felices, infelices) (x : acc) (s : xs)
    where s = sumaCuadradosDigitos x

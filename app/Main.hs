module Main where

import qualified MyLib (someFunc)
import qualified Enumerate5x5

main :: IO ()
main = do
  let squares = take 1000000 Enumerate5x5.allSquares
  print (all (\s -> sum (Enumerate5x5.allSquareValues s) == 325) squares)



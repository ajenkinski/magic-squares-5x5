module Main where

import qualified MyLib (someFunc)
import qualified Enumerate5x5
import Data.List (foldl')
import qualified System.IO as SIO
import Control.Monad (when, foldM)
import Text.Printf (printf)
import Data.Text.Format.Numbers (prettyI)
import qualified Data.Text

-- Format an integer with thousands separators
formatInt :: Int -> Data.Text.Text
formatInt = prettyI (Just ',')

main :: IO ()
main = do
  let squares = Enumerate5x5.allSquares
  let validate square = sum (Enumerate5x5.allSquareValues square) == 325
  -- print an update every updateEvery squares so I know it's making progress
  let updateEvery = 10000
  printf "Starting computation. Printing an update every %v squares.\n" (formatInt updateEvery)
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  -- The call to seq in the foldM function is necessary to make squares be solved
  -- as the loop goes along, otherwise all the dots get printed immediately, and
  -- the squares don't actually get solved until counts get printed out.
  (numValid, totalCount) <- foldM (\(n,t) square ->
                                  do when (seq n False) (error "Force eval")
                                     when (t `mod` updateEvery == 0) (printf "%15v squares found\r" (formatInt t))
                                     return (if validate square then n+1 else n,
                                             t + 1))
                       (0, 0) squares
  printf "\ntotalCount = %v, numValid = %v\n" (formatInt totalCount) (formatInt numValid)


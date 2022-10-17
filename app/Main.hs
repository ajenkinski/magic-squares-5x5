module Main where

import Control.Concurrent (getNumCapabilities)
import Control.Monad (foldM, forM_, when)
import qualified Control.Parallel.Strategies as PS
import Data.List (foldl', foldl1')
import qualified Data.Text
import Data.Text.Format.Numbers (prettyI)
import qualified Enumerate5x5
import qualified System.IO as SIO
import Text.Printf (printf)

-- Format an integer with thousands separators
formatInt :: Int -> Data.Text.Text
formatInt = prettyI (Just ',')

main :: IO ()
main = do
  numThreads <- getNumCapabilities
  if numThreads > 1
    then computeAllParallel
    else computeAllSequentially

computeAllSequentially :: IO ()
computeAllSequentially = do
  let squares = Enumerate5x5.allSquares
  let validate square = sum (Enumerate5x5.allSquareValues square) == 325
  -- print an update every updateEvery squares so I know it's making progress
  let updateEvery = 1000
  printf "Starting computation. Printing an update every %v squares.\n" (formatInt updateEvery)
  -- disable buffering, otherwise progress updates without a newline don't get immediately displayed
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  (numValid, totalCount) <-
    foldM
      ( \(n, t) square ->
          do
            when (t `mod` updateEvery == 0) (printf "%15v squares found, %v valid\r" (formatInt t) (formatInt n))
            return
              ( if Enumerate5x5.squareIsValid square then n + 1 else n,
                t + 1
              )
      )
      (0, 0)
      squares
  printf "\ntotalCount = %v, numValid = %v\n" (formatInt totalCount) (formatInt numValid)

checkSquares :: [Enumerate5x5.Square] -> (Int, Int)
checkSquares squares =
  let updateStats (numTotal, numValid) square =
        let numTotal' = numTotal + 1
            numValid' = numValid + (if Enumerate5x5.squareIsValid square then 1 else 0)
         in numTotal' `seq` numValid' `seq` (numTotal', numValid')
   in foldl' updateStats (0, 0) squares

computeAllParallel :: IO ()
computeAllParallel = do
  putStrLn "Starting parallel computation"
  let squaresPerGroup = map Enumerate5x5.allSquaresForMainDiag Enumerate5x5.allMainDiagSquares
  let strategy = PS.parListChunk 10 PS.rdeepseq
  let statsPerGroup = map checkSquares squaresPerGroup `PS.using` strategy
  -- disable buffering, otherwise progress updates without a newline don't get immediately displayed
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  (totalCount, validCount) <-
    foldM
      ( \(total, valid) (total', valid') ->
          do
            let (newTotal, newNumValid) = (total + total', valid + valid')
            printf "%15v squares found, %v valid\r" (formatInt newTotal) (formatInt newNumValid)
            return (newTotal, newNumValid)
      )
      (0, 0)
      statsPerGroup
  printf "Total squares found = %d, # valid = %d\n" totalCount validCount

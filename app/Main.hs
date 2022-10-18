module Main where

import Control.Concurrent (getNumCapabilities)
import Control.Monad (foldM, when)
import qualified Control.Parallel.Strategies as PS
import Data.List (foldl')
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
  numThreads <- getNumCapabilities
  let chunkSize = 10
  let numChunks = (length Enumerate5x5.allMainDiagSquares + chunkSize - 1) `div` chunkSize
  let squaresPerGroup = map Enumerate5x5.allSquaresForMainDiag Enumerate5x5.allMainDiagSquares
  printf "Starting parallel computation with %d threads, and %d chunks\n" numThreads numChunks
  let strategy = PS.parListChunk chunkSize PS.rdeepseq
  let statsPerGroup = map checkSquares squaresPerGroup `PS.using` strategy
  -- disable buffering, otherwise progress updates without a newline don't get immediately displayed
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  (_, totalCount, validCount) <-
    foldM
      ( \(groupNum, total, valid) (total', valid') ->
          do
            let (newTotal, newNumValid) = (total + total', valid + valid')
            printf "%d: %v squares found, %v valid\r" groupNum (formatInt newTotal) (formatInt newNumValid)
            return (groupNum + 1, newTotal, newNumValid)
      )
      (0 :: Int, 0, 0)
      statsPerGroup
  printf "Total squares found = %d, # valid = %d\n" totalCount validCount

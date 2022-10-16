module Main where

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
  computeAllParallel

computeAllSequentially :: IO ()
computeAllSequentially = do
  let squares = Enumerate5x5.allSquares
  let validate square = sum (Enumerate5x5.allSquareValues square) == 325
  -- print an update every updateEvery squares so I know it's making progress
  let updateEvery = 1000
  printf "Starting computation. Printing an update every %v squares.\n" (formatInt updateEvery)
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  -- The call to seq in the foldM function is necessary to make squares be solved
  -- as the loop goes along, otherwise all the dots get printed immediately, and
  -- the squares don't actually get solved until counts get printed out.
  (numValid, totalCount) <-
    foldM
      ( \(n, t) square ->
          do
            when (seq n False) (error "Force eval")
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
  let centerValues = [1 .. 13]
  let squaresPerCenter = map Enumerate5x5.allSquaresForCenter centerValues
  let statsPerCenter = PS.parMap PS.rseq checkSquares squaresPerCenter
  forM_ (zip centerValues statsPerCenter) $ \(centerValue, (totalCount, validCount)) ->
    printf "For center value %d: total squares = %d, num valid = %d\n" centerValue totalCount validCount

  let (totalCount, validCount) = foldl1' (\(total, valid) (total', valid') -> (total + total', valid + valid')) statsPerCenter
  printf "Total squares found = %d, # valid = %d\n" totalCount validCount

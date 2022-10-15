module Enumerate5x5 where

import Data.List ((\\))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Array as Array
import Data.Array ((!))
import qualified Data.Maybe as Maybe
import Text.Printf (printf)
import Control.Monad (guard)

-- Return all "subsets of length k" of xs
ssolk :: Int -> [a] -> [[a]]
ssolk k xs
    | k == 0    = [[]]
    | otherwise =
        [x:ss | (x:rest) <- List.tails xs, ss <- ssolk (k-1) rest]

type ComponentVector = [Int]

type Idx = (Int, Int)
type Square = Array.Array Idx Int

data Component = 
    Row Int |
    Col Int |
    MainDiag |  -- top left to bottom right
    MinorDiag   -- top right to bottom left


componentIndices :: Component -> [Idx]
componentIndices comp =
    case comp of
        Row r -> [(r, c) | c <- [0..4]]
        Col c -> [(r, c) | r <- [0..4]]
        MainDiag -> [(i, i) | i <- [0..4]]
        MinorDiag -> [(r, c) | (r, c) <- zip [0..4] [4,3..0]]


allNums :: [Int]
allNums = [1..25]

-- Generate all component vectors that could be a row, column or diagonal.  These are all distinct sets of 5 numbers
-- between 1..25 whose sum is 65.
allVectors :: [ComponentVector]
allVectors = [v | v <- ssolk 5 allNums, sum v == 65]

{-
For this algorithm, we need to be able to efficiently find all component vectors that contain one or more numbers, for
instance, all vectors containing 1 and 5.  We also need to efficiently find all vectors that don't contain
one ore more numbers, i.e. all vectors that don't contain 6 or 7. Finally we need to find the intersection of these,
i.e. all vectors that contain 1 and 5, but don't contain 6 or 7.

To do this, I'll make two indexes: one that lets me look up the set of vectors containing a number, and another that
allows looking up the set of all vectors that *don't* contain a number.  Then I can use set operations to get 
combinations of those.
-}

-- Index that allows looking up all vectors containing number x
vectorsByInclude :: Map.Map Int (Set.Set ComponentVector)
vectorsByInclude = Map.fromListWith Set.union [(n, Set.singleton v) | v <- allVectors, n <- v]

-- Index that allows looking up all vectors that don't contain x
vectorsByExclude :: Map.Map Int (Set.Set ComponentVector)
vectorsByExclude = Map.fromListWith Set.union [(n, Set.singleton v) | v <- allVectors, n <- allNums \\ v]


-- A function that returns a list of vectors that include includes, and exclude excludes
-- For example, (filteredVectors [1, 2] [3, 4]) returns all vectors that contain 1 and 2, and don't contain 3 or 4
filteredVectors :: [Int] -> [Int] -> [ComponentVector]
filteredVectors includes excludes =
    let vectorsThatInclude = [vectorsByInclude Map.! i | i <- includes]
        vectorsThatExclude = [vectorsByExclude Map.! i | i <- excludes]
        vectors = vectorsThatInclude ++ vectorsThatExclude
    in 
        if List.null vectors then 
            [] 
        else 
            {-# SCC "vectors_intersection" #-} Set.toList $ List.foldl1' Set.intersection vectors

-- Operations on squares
        
emptySquare :: Square
emptySquare = Array.listArray ((0, 0), (4, 4)) (repeat 0)

showSquare :: Square -> String
showSquare square =
    let ((rs, cs), (re, ce)) = Array.bounds square
        rows = [unwords [printf "%2d" (square!(r, c)) | c <- [cs..ce]] | r <- [rs..re]]
    in unlines rows


-- Assign a row, column or diagonal vector to square.
-- assignVector startCoord slope vector square
assignVector :: Component -> ComponentVector -> Square -> Square
assignVector comp vector square =
    let idxs = componentIndices comp
        assocs = zip idxs vector
    in square Array.// assocs


-- Returns a (vectorIndices, nonZeroValues) tuple for a component, indicating where the 
-- assigned values for this component are.  nonZeroValues is the list of nonZero values in component,
-- and vectorIndices are the indices of the non-zeros (0..4) along the component.
assignedValues :: Component -> Square -> ([Int], [Int])
assignedValues comp square =
    let idxs = componentIndices comp
        nonZeros = [(vecIdx, val) | 
                    (idx, vecIdx) <- zip idxs [0..], 
                    let val = square!idx, 
                    val /= 0]
    in (map fst nonZeros, map snd nonZeros)


-- Return a list of all assigned values of a square
allSquareValues :: Square -> [Int]
allSquareValues square = filter (/= 0) (Array.elems square)


-- Return a list of all permutations of a list, but with only certain elements allowed to move.
-- For example (vectorPermutations [1,3,4] vec) will return a list of all permutations of vec
-- resulting from permuting the elements with index (0-based) 1, 3 and 4, but elements 0 and 2
-- won't be moved.
vectorPermutations :: [Int] -> ComponentVector -> [ComponentVector]
vectorPermutations toMove vector =
    let enumerated = zip [0..] vector
        valsToMove = [vector!!i | i <- toMove]
        permute toMovePermutation =
            let mapping = zip toMove toMovePermutation
            in [Maybe.fromMaybe val (List.lookup idx mapping) | (idx, val) <- enumerated]
    in [permute p | p <- List.permutations valsToMove]


-- Given (alignVector indices values vector), where values is a list of values contained in vector, returns
-- a copy of vector with values moved to be at the corresponding positions in indices.  Assumes all values are
-- contained in vector, and that indices are in bounds for vector.
alignVector :: [Int] -> [Int] -> ComponentVector -> ComponentVector
alignVector indices vals vector =
    let valAssignments = zip indices vals
        unassignedIndices = [0..length vector] \\ indices
        unassignedValues = vector \\ vals
        -- assign unassigned values to remaining indices
        otherAssignments = zip unassignedIndices unassignedValues
        sortedAssignments = List.sortBy (\a b -> compare (fst a) (fst b)) (valAssignments ++ otherAssignments)
    in map snd sortedAssignments



alignVectorToComponent :: ComponentVector -> Component -> Square -> (ComponentVector, [Int], [Int])
alignVectorToComponent vector comp square =
    let (indices, vals) = assignedValues comp square
    in (alignVector indices vals vector, indices, vals)


centerIndex :: Idx
centerIndex = (2, 2)

vectorIndices :: [Int]
vectorIndices = [0..4]


performStep :: Square -> Component -> [Square]
performStep square component = do
    let (filledIndices, filledVals) = assignedValues component square
    let valsToExclude = allSquareValues square \\ filledVals
    newComponentVec <- filteredVectors filledVals valsToExclude
    let toMove = vectorIndices \\ filledIndices
    let alignedVec = alignVector filledIndices filledVals newComponentVec

    -- iterate over permutations
    newComponentVec <- vectorPermutations toMove alignedVec
    return $ assignVector component newComponentVec square

{-
B J K L F
V C T H X
R M A P S
W I U D Y
G N Q O E
-}
allSquares :: [Square]
allSquares = do
    -- select the center value
    centerValue <- allNums
    let centerSquare = emptySquare Array.// [(centerIndex, centerValue)]

    -- select the main diagonal
    mainDiagSquare <- performStep centerSquare MainDiag

    -- Contraint E > B, D > C > B
    guard (mainDiagSquare!(4,4) > mainDiagSquare!(0,0) &&
           mainDiagSquare!(3,3) > mainDiagSquare!(1,1) &&
           mainDiagSquare!(1,1) > mainDiagSquare!(0,0))

    -- select minor diagonal
    minorDiagSquare <- performStep mainDiagSquare MinorDiag

    -- constraint I > B, H > B, G > F > B
    guard (minorDiagSquare!(3,1) > minorDiagSquare!(0,0) &&
           minorDiagSquare!(1,3) > minorDiagSquare!(0,0) &&
           minorDiagSquare!(4,0) > minorDiagSquare!(0,4) &&
           minorDiagSquare!(4,0) > minorDiagSquare!(0,0))

    -- select 1st row
    row0Square <- performStep minorDiagSquare (Row 0)

    -- select 2nd column
    col1Square <- performStep row0Square (Col 1)

    -- select 4th column
    col3Square <- performStep col1Square (Col 3)

    -- select 5th row
    row4Square <- performStep col3Square (Row 4)

    -- select 3rd row
    row2Square <- performStep row4Square (Row 2)

    -- select 3rd column
    col2Square <- performStep row2Square (Col 2)

    -- select 1st column
    col0Square <- performStep col2Square (Col 0)

    -- select 5th column
    performStep col0Square (Col 4)


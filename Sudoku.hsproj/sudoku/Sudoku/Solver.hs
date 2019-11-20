module Sudoku.Solver (
  canSetCellAt, 
  solutions
) where

import Data.Maybe

import Sudoku.Size
import Sudoku.Cell
import Sudoku.Grid
 

-- | Converts a /row/ and a /column/ into an /index/
rowColToIndex :: Grid -> Int -> Int -> Int
rowColToIndex (Grid size _) r c = (r * size * size) + c

-- | Converts an /index/ into a tupel with a /row/ and a /column/
indexToRowCol :: Grid -> Int -> (Int, Int)
indexToRowCol (Grid size _) i = (i `div` (size * size), i `mod` (size * size))



-- | Tests wether a given cell could fit into the grid at a given row and column. 
canSetCellAt :: Cell -> Int -> Int -> Grid -> Bool 
canSetCellAt cell row col grid@(Grid size _) 
  = writeable && notInRow && notInCol && notInSub
  where 
    writeable = canSetAtIndex (rowColToIndex grid row col) grid
    notInRow = cell `notElem` rowAt row grid
    notInCol = cell `notElem` colAt col grid
    notInSub = cell `notElem` subAt (row `div` size) (col `div` size) grid

    
-- | Tests wether a given cell could fit into the grid at a given index
canSetCellAtIndex :: Cell -> Int -> Grid -> Bool
canSetCellAtIndex cell index grid = canSetCellAt cell row col grid
   where (row, col) = indexToRowCol grid index 

-- | Gets the successor of a cell. Empty yields Value 1, Value 1 yield Value 2, .. etc and Value 9 yields Nothing.
nextCell :: Int -> Cell -> Maybe Cell
nextCell _ Empty         = Just (Value 1)
nextCell size (Value n)  = if n > 0 && n < size * size 
                             then Just (Value (n + 1)) 
                             else Nothing
nextCell _ _             = Nothing

---- | Gets the next possible cell for a position or Nothing if there is no possibility.
nextPossibleCell :: Int -> Grid -> Maybe Cell
nextPossibleCell index grid@(Grid size _) = nextPossibleCell' index (getAtIndex index grid) grid
  where 
    nextPossibleCell' i c g 
      | isNothing next                        = Nothing
      | canSetCellAtIndex (fromJust next) i g = next      
      | otherwise                             = nextPossibleCell' i (fromJust next) g
      where next = nextCell size c


solutions' grid@(Grid size cells) index direction result
      | index < 0                   = result  -- we have found all possible solutions
      -- we're at he end of the grid - record this solution and track back
      | index == area               = solutions' grid (index - 1) (-1) (cells:result) 
      -- we reached a dead end, so we track back
      | writeable && isNothing next = solutions' (setAtIndex index Empty grid) (index - 1) (-1) result  
       -- we found a candidate, now try the next cell
      | writeable && isJust next    = solutions' (setAtIndex index (fromJust next) grid) (index + 1) 1 result
      | otherwise                   = solutions' grid (index + direction) direction result -- skip this cell
      where 
        writeable = canSetAtIndex index grid
        next      = nextPossibleCell index grid
        area      = size * size * size * size 


-- | Gets all possible solutions for a grid. 
solutions :: Grid -> [Grid]
solutions grid@(Grid size cells) = map (\a -> Grid size a) (solutions' grid 0 1 [])
  


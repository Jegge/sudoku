-- | This module contains all types and functions to create, manipulate or query sudoku grids
module Sudoku.Grid where

import Data.Maybe
import Data.List
import Data.List.Split
import Data.Char

import Sudoku.Size
import Sudoku.Cell


-- | A Grid is a list of /side × side/ Cells combined with it's fundamental size.
data Grid = Grid Int [Cell]
  deriving (Show, Eq)


-- | Converts a Grid into a String.
showGrid :: Grid -> String
showGrid (Grid _ cells) = map showCell cells

-- | Tries to convert a String into a Grid.
readGrid :: Int -> String -> Maybe Grid
readGrid size string 
  | length string == area && isJust cells = Just (Grid size (fromJust cells))
  | otherwise                             = Nothing
  where
    cells = mapM readCell string
    area = size * size * size * size
    
  
-- | Pretty prints a Grid into a String.
prettyGrid :: Grid -> String 
prettyGrid (Grid size cells) = (unlines . concat . embed line . chunksOf size . map prettyrow . chunksOf (size * size)) cells
  where 
    line = ["+---+---+---+"]
    prettyrow = concat . embed "|" . chunksOf size . map showCell
    embed a xs = [a] ++ intersperse a xs ++ [a]

-- | Tries to convert a String into a Grid, beeing more permissive than readGrid 
parseGrid :: Int -> String -> Maybe Grid 
parseGrid size string = ((readGrid size) . take area . pad area . filter isValidChar) string
  where 
    pad m xs = xs ++ replicate (m - length xs) ' '
    isValidChar c = isDigit c || c == ' '
    area = size * size * size * size


-- | Gets a cell by a given index.
getAtIndex :: Int -> Grid -> Cell
getAtIndex index (Grid size cells) = cells !! index

-- | Tests wether the cell at the given index may be written to.
canSetAtIndex :: Int -> Grid -> Bool
canSetAtIndex index grid = case getAtIndex index grid of Fixed _ -> False
                                                         _       -> True

-- | Tries to set a Cell to a given index and returns a new Grid containing the change. If the cell is neither a Value nor an Empty cell, or the index is out of range, no change will occur.                                           
setAtIndex :: Int -> Cell -> Grid -> Grid
setAtIndex _ (Fixed _) grid = grid
setAtIndex i cell grid@(Grid size cells) 
  | i < 0 || i >= area || not writable = grid
  | otherwise                          = Grid size (take i cells ++ [cell] ++ drop (i + 1) cells)
  where 
    writable = canSetAtIndex i grid 
    area = size * size * size * size


-- | Get all rows from a grid.
rows :: Grid -> [[Cell]]
rows (Grid size cells) = chunksOf (size * size) cells

-- | Gets all columns from a grid. 
columns :: Grid -> [[Cell]]
columns = transpose . rows

-- | Gets all subs from a grid. 
subs :: Grid -> [[Cell]]
subs (Grid size cells) = (chunksOf (size * size) . concat . concat . transpose . chunksOf size . chunksOf size) cells


-- | Gets the whole row given by it's index.
rowAt :: Int -> Grid -> [Cell]
rowAt row grid = rows grid !! row

-- | Gets the whole column given by it's index.
colAt :: Int -> Grid -> [Cell]
colAt col grid  = columns grid !! col

-- | Gets the whole subgrid given by it's row and column.
subAt :: Int -> Int -> Grid -> [Cell]
subAt row col grid@(Grid size _) = subs grid !! (row + (col * size))

-- | Checks wether the Grid is valid. It must compromise 81 cells and can not have duplicate values per part
isValid :: Grid -> Bool
isValid grid@(Grid size cells) = length cells == area && rowsValid grid && colsValid grid && subsValid grid
  where 
    partIsValid r = nub (notEmpty r) == notEmpty r
    rowsValid = all partIsValid . rows
    colsValid = all partIsValid . columns
    subsValid = all partIsValid . subs
    notEmpty = filter (/=Empty) 
    area = size * size * size * size

-- | Checks wether the Grid is filled. All cells must have either a Value or a Fixed
isFilled :: Grid -> Bool
isFilled grid@(Grid size _) = rowsFilled grid && colsFilled grid && subsFilled grid
  where
    partIsFilled r = length (notEmpty r) == size * size            
    rowsFilled = all partIsFilled . rows
    colsFilled = all partIsFilled . columns
    subsFilled = all partIsFilled . subs
    notEmpty = filter (/=Empty) 

-- | Checks wether the Grid is solved. It needs to be filled and valid
isSolved :: Grid -> Bool
isSolved grid = isValid grid && isFilled grid


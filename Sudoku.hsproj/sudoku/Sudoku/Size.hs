-- | This module contains all types and functions to create, manipulate or query sudoku grids
module Sudoku.Size where

---- | The fundamental size of a Sudoku : /size × size/ boxes with /size × size/ cells
--size :: Int
--size = 3
--
---- | The length of a side a Sudoku : /side × side/ cells
--side :: Int
--side = size * size
--
---- | The count of cells in a Sudoku
--area :: Int
--area = side * side

-- | Converts a /row/ and a /column/ into an /index/
--rowColToIndex :: Int -> Int -> Int
--rowColToIndex r c = (r * side) + c
--
---- | Converts an /index/ into a tupel with a /row/ and a /column/
--indexToRowCol :: Int -> (Int, Int)
--indexToRowCol i = (i `div` side, i `mod`side)
--
---- | Gets all indicees of the rows
--rowIndicees = [0..(side - 1)]
--
---- | Gets all indicees of the columns
--colIndicees = [0..(side - 1)]
--
---- | Gets all indicees of the sub grids
--subIndicees = [(x,y)| x <- [0..(size - 1)], y <- [0..(size - 1)]] 

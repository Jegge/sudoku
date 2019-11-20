-- | This module contains all types and functions related to single cells
module Sudoku.Cell where
  
import Data.Char
import Data.Maybe
  
-- | The cell is the fundamental building block of a Sudoku. A cell may have a /Value/ that can be changed by a user, a /Fixed/ value (that can not be changed) or be /Empty/. Cells are deemed equal when their value is equal, disregaring the fixed/value property or when both are Empty.
data Cell = Value Int   -- ^ A value that can be set externally, e.g. part of the solution
          | Fixed Int   -- ^ A value that can not be set externally, e.g. part of the puzzle
          | Empty       -- ^ A an empty field that can be set externally, e.g. part of the solution
          deriving (Show)
          
instance Eq Cell where
  (==) a b = valueOfCell a == valueOfCell b
  
instance Ord Cell where
  (<=) a b = valueOfCell a <= valueOfCell b
  
-- | Converts a Cell into a Char. Fixed or Value cells get translated into their respective number, empty cells get translated into a space character.
showCell :: Cell -> Char
showCell (Value v)  = intToDigit v
showCell (Fixed v)  = intToDigit v
showCell Empty      = ' '

-- | Converts a Char into a Cell. Space characters get translated into empty cells, digits get translated into fixed cells with the corresponding value. Other characters yield Nothing
readCell :: Char -> Maybe Cell
readCell c
  | c == ' ' || c == '0' = Just Empty
  | c >= '1' && c <= '9' = Just (Fixed (digitToInt c))
  | otherwise            = Nothing 

-- | Converts a Cell into the actual value
valueOfCell :: Cell -> Int
valueOfCell (Fixed v) = v
valueOfCell (Value v) = v
valueOfCell Empty     = 0
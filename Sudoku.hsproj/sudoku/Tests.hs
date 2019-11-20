module Tests where

import Data.Maybe  
import Test.HUnit
import System.IO

import Sudoku.Size
import Sudoku.Cell
import Sudoku.Grid
import Sudoku.Solver

-- test data
text = concat [ "79    3  "
              , "     69  "
              , "8   3  76"
              , "     5  2"
              , "  54187  "
              , "4  7     "
              , "61  9   8"
              , "  23     "
              , "  9    54"
              ]

invalid = concat [ "99    3  "
                 , "     69  "
                 , "8   3  76"
                 , "     5  2"
                 , "  54187  "
                 , "4  7     "
                 , "61  9   8"
                 , "  23     "
                 , "  9    54"
                 ]
              
result = concat [ "796854321"
                , "243176985"
                , "851239476"
                , "137965842"
                , "925418763"
                , "468723519"
                , "614597238"
                , "582341697"
                , "379682154"
                ]

pretty = unlines [ "+---+---+---+"
                 , "|79 |   |3  |"  
                 , "|   |  6|9  |"
                 , "|8  | 3 | 76|"
                 , "+---+---+---+"
                 , "|   |  5|  2|"
                 , "|  5|418|7  |"
                 , "|4  |7  |   |"
                 , "+---+---+---+"
                 , "|61 | 9 |  8|"
                 , "|  2|3  |   |"
                 , "|  9|   | 54|"
                 , "+---+---+---+"
                 ]
                     
grid = fromJust $ readGrid 3 text

rowZero = [Fixed 7, Fixed 9, Empty  , Empty  , Empty  , Empty  , Fixed 3, Empty  , Empty  ]
rowFour = [Empty  , Empty  , Fixed 5, Fixed 4, Fixed 1, Fixed 8, Fixed 7, Empty  , Empty  ]

colZero = [Fixed 7, Empty  , Fixed 8, Empty  , Empty  , Fixed 4, Fixed 6, Empty  , Empty  ]
colFour = [Empty  , Empty  , Fixed 3, Empty  , Fixed 1, Empty  , Fixed 9, Empty  , Empty  ]

subZero = [Fixed 7, Fixed 9, Empty  , Empty  , Empty  , Empty  , Fixed 8, Empty  , Empty  ]
subFour = [Empty  , Empty  , Fixed 5, Fixed 4, Fixed 1, Fixed 8, Fixed 7, Empty  , Empty  ]



allTests = TestList [
-- size tests
--                      TestCase (assertEqual "for size," 3 size)
--
--                    , TestCase (assertEqual "for side," 9 side)
--                    
--                    , TestCase (assertEqual "for area," 81 area)
--                    
--                    , TestCase (assertEqual "for rowColToIndex 2 5," 23 (rowColToIndex 2 5))
--                    
--                    , TestCase (assertEqual "for indexToRowCol 23," (2, 5) (indexToRowCol 23))
--                      
--                    , TestCase (assertEqual "for rowIndicees," [0..8] rowIndicees)
--                    
--                    , TestCase (assertEqual "for colIndicees," [0..8] colIndicees)
--                    
--                    , TestCase (assertEqual "for subIndicees," [(x,y)| x <- [0..2], y <- [0..2]] subIndicees)
                    
-- cell tests
                      TestCase (assertBool "for Empty == Empty" (Empty == Empty))
                    , TestCase (assertBool "for Fixed 1 == Fixed 1" (Fixed 1 == Fixed 1))
                    , TestCase (assertBool "for Value 1 == Value 1" (Value 1 == Value 1))
                    , TestCase (assertBool "for Fixed 1 == Value 1" (Fixed 1 == Value 1))
                    , TestCase (assertBool "for Value 1 == Fixed 1" (Value 1 == Fixed 1))
                    , TestCase (assertBool "for Value 1 /= Value 2" (Value 1 /= Value 2))
                    , TestCase (assertBool "for Fixed 1 /= Fixed 2" (Fixed 1 /= Fixed 2))
                    , TestCase (assertBool "for Fixed 1 /= Value 2" (Fixed 1 /= Value 2))
                    , TestCase (assertBool "for Value 1 /= Fixed 2" (Value 1 /= Fixed 2))
                    
                    , TestCase (assertEqual "for showCell Fixed 1," '1' (showCell (Fixed 1)))
                    , TestCase (assertEqual "for showCell Fixed 6," '6' (showCell (Fixed 6)))
                    , TestCase (assertEqual "for showCell Value 1," '1' (showCell (Value 1)))
                    , TestCase (assertEqual "for showCell Value 6," '6' (showCell (Value 6)))
                    , TestCase (assertEqual "for showCell Empty," ' ' (showCell Empty))
                    
                    , TestCase (assertEqual "for readCell ' ',"  (Just Empty) (readCell ' '))
                    , TestCase (assertEqual "for readCell '1',"  (Just (Fixed 1)) (readCell '1'))
                    , TestCase (assertEqual "for readCell 'x',"  Nothing (readCell 'x'))
                    , TestCase (assertEqual "for readCell '0',"  (Just Empty) (readCell '0'))
                    
                    , TestCase (assertEqual "for valueOfCell Fixed 4," 4 (valueOfCell (Fixed 4)))
                    , TestCase (assertEqual "for valueOfCell Value 4," 4 (valueOfCell (Value 4)))
                    , TestCase (assertEqual "for valueOfCell Empty," 0 (valueOfCell Empty))
                    
-- grid tests
                    , TestCase (assertBool "for readGrid 3 $VALID," (isJust $ readGrid 3 text))
                    , TestCase (assertBool "for readGrid 3 $INVALID," (isNothing $ readGrid 3 ""))
                    , TestCase (assertBool "for readGrid 3 $INVALID," (isNothing $ readGrid 3 (drop 5 text)))
                      
                    , TestCase (assertEqual "for parseGrid 3," (Just grid) (parseGrid 3 pretty))
                    
                    , TestCase (assertEqual "for showGrid," text (showGrid grid))
                    
                    , TestCase (assertEqual "for prettyGrid," pretty (prettyGrid grid))
                    
                    , TestCase (assertEqual "for getAtIndex 0," (Fixed 7) (getAtIndex 0 grid))
                    , TestCase (assertEqual "for getAtIndex 2," Empty (getAtIndex 2 grid))
                    
                     
                    , TestCase (assertEqual "for canSetAtIndex 0," False (canSetAtIndex 0 grid))
                    , TestCase (assertEqual "for canSetAtIndex 2," True (canSetAtIndex 2 grid))
                    
                    , TestCase (assertEqual "for setAtIndex 0 Value 1," grid (setAtIndex 0 (Value 1) grid))
                    , TestCase (assertEqual "for setAtIndex 0 Empty," grid (setAtIndex 0 Empty grid))
                    , TestCase (assertEqual "for setAtIndex 2 Fixed 1," grid (setAtIndex 2 (Fixed 1) grid))
                    , TestCase (assertEqual "for setAtIndex -1 Value 1," grid (setAtIndex (-1) (Value 1) grid))
                    , TestCase (assertEqual "for setAtIndex 81 Value 1," grid (setAtIndex 81 (Value 1) grid))
                    , TestCase (assertBool  "for setAtIndex 2 Value 1" (grid /= setAtIndex 2 (Value 1) grid))
                    , TestCase (assertEqual "for setAtIndex 2 Value 1" (Value 1) (getAtIndex 2 (setAtIndex 2 (Value 1) grid)))
                    , TestCase (assertEqual "for setAtIndex 2 Empty" grid (setAtIndex 2 Empty (setAtIndex 2 (Value 1) grid)))
                    , TestCase (assertEqual "for setAtIndex 2 Empty" Empty (getAtIndex 2 (setAtIndex 2 Empty grid)))
                    
                    , TestCase (assertEqual "for rowAt 0," rowZero (rowAt 0 grid))
                    , TestCase (assertEqual "for rowAt 4," rowFour (rowAt 4 grid))
                    
                    , TestCase (assertEqual "for colAt 0," colZero (colAt 0 grid))
                    , TestCase (assertEqual "for colAt 4," colFour (colAt 4 grid))
                    
                    , TestCase (assertEqual "for subAt 0 0," subZero (subAt 0 0 grid))
                    , TestCase (assertEqual "for subAt 1 1," subFour (subAt 1 1 grid))
                    
                    , TestCase (assertBool "for isValid $VALID," (isValid grid))
                    , TestCase (assertBool "for isValid $INVALID," ((not . isValid . fromJust . readGrid 3) invalid))
                    
                    , TestCase (assertBool "for isFilled $FILLED," ((isFilled .fromJust .readGrid 3) result))
                    , TestCase (assertBool "for isFilled $EMPTY," ((not . isFilled) grid))
                    
                    , TestCase (assertBool "for isSolved $SOLVED," ((isSolved .fromJust .readGrid 3) result))
                    , TestCase (assertBool "for isSolved $INVALID," ((not . isSolved . fromJust . readGrid 3) invalid))
                    , TestCase (assertBool "for isSolved $EMPTY," ((not . isSolved) grid))
                    
-- solver tests                                        
                    , TestCase (assertBool "for canSetCellAt (Fixed 1) 0 0," (not $ canSetCellAt (Fixed 1) 0 0 grid))
                    , TestCase (assertBool "for canSetCellAt (Fixed 1) 0 2," (canSetCellAt (Fixed 1) 0 2 grid))
                    
                    , TestCase (assertEqual "for solutions," result (showGrid $ head $ solutions grid))
                  ]

-- housekeeping
runAllTests = runTestText (putTextToHandle stdout False) allTests
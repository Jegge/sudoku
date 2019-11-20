module Sudoku.Hint where
--module Sudoku.Hint (
--  HintType(..),
--  Hint(..),
--  showHint,
--  hints
--) where
--  
--
--import Sudoku.Size
--import Sudoku.Cell
--import Sudoku.Grid
--import Sudoku.Solver
--import Sudoku.Candidates
--
--data HintType = NakedSingle
--              | HiddenSingle
--
--instance Show HintType where 
--  show NakedSingle  = "Naked Single"
--  show HiddenSingle = "Hidden Single"
--
--type Hint = (HintType, Int, Int, Int) 
--
--candidateToHint :: HintType -> Candidates -> Hint
--candidateToHint t (Candidates r c [(Fixed v)]) = (t, r, c, v)
--candidateToHint t (Candidates r c [(Value v)]) = (t, r, c, v)
--
--
--
--showHint :: Hint -> String
--showHint (t, r, c, v) = "(" ++ show r ++ "," ++ show c ++ ") " ++ show t ++ " '" ++ show v ++ "'"
--
--
--hints :: Grid -> [Hint]
--hints grid = ns grid ++ hs grid
--  where 
--    ns = map (candidateToHint NakedSingle) . nakedSingles
--    hs = map (candidateToHint HiddenSingle) . hiddenSingles
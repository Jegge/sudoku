{-# LANGUAGE DeriveDataTypeable #-}
module Main where 

import Control.Applicative
import Control.Monad
import System.Console.CmdArgs

import Sudoku.Cell
import Sudoku.Grid
import Sudoku.Solver
import Sudoku.Hint

data Sudoku = Solve { pretty :: Bool, file :: FilePath }
            | Hint { allHints :: Bool, file :: FilePath }
            deriving (Data, Typeable, Show, Eq)

solve = Solve { pretty = def &= help "Pretty print output" 
              , file = def &= args &= typFile &= opt ""
              }
              &= help "Solve the sudoku" 
              &= auto

hint = Hint { allHints = def &= name "all" &= help "Show all hints, not just the next " 
            , file = def &= args &= typFile &= opt ""
            }
            &= help "Shows the next or all next moves." 
            &= details [ "If an input file name is omitted, the sudoku will be read from stdin." ]

                
sudoku = cmdArgsMode $ modes [solve, hint] 
                      &= help "A command line swiss army knife for sudokus" 
                      &= summary "Sudoku v1.0.0 (c) Sebastian Boettcher"


printGrid :: Grid -> Bool -> IO ()
printGrid grid pretty = putStrLn (if pretty then prettyGrid grid else showGrid grid)

printSolutions :: [Grid] -> Bool -> IO ()
printSolutions xs pretty
  | null xs        = putStrLn "No solution found."
  | length xs == 1 = printGrid (head xs) pretty
  | otherwise      = putStrLn "Multiple solutions found."
            

--printHints :: [Hint] -> Bool -> IO ()
--printHints [] _     = putStrLn "No hints found."
--printHints xs True  = putStrLn $ unlines $ map showHint xs 
--printHints xs False = putStrLn $ showHint $ head xs


--withSudokuFile :: String -> (Grid -> IO()) -> IO()
--withSudokuFile file function = do
--  g <- parseGrid <$> if null file then getContents else readFile file
--  case g of Nothing    -> error "Failed to parse sudoku."
--            Just grid  -> function grid


main :: IO ()
main = do
  arguments <- cmdArgsRun sudoku
  case arguments of 
    (Solve pretty file) -> do
      g <- parseGrid 3 <$> if null file then getContents else readFile file
      case g of Nothing    -> error "Failed to parse sudoku."
                Just grid  -> printSolutions (solutions grid) pretty
--    (Hint allHints file) -> do
--      g <- parseGrid <$> if null file then getContents else readFile file
--      case g of Nothing    -> error "Failed to parse sudoku."
--                Just grid  -> printHints (hints grid) allHints

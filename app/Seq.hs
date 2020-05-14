-- An interface to Data.Algorithm.CubicSpline
module Main where

import Control.Monad.Extra
    ( unlessM 
    )
import System.Environment
import System.Exit
import Data.Algorithm.CubicSpline

-- Parses a file with profiling information, applies interpolation, and prints
-- the knots and the coefficients of the cubic polynomoials
main :: IO ()
main = do
    pr <- getProgName
    fp <- getArgs >>= parse pr
    kn <- knots . map words . lines <$> readFile fp
    let c = cubicSplineCoefficients kn
    writeFile (fp ++ ".poly") $ show kn ++ "\n" ++ show c

knots :: [[String]] -> [(Double, Double)]
knots [] = []
knots ([l, r] : rest) = (read l, read r) : knots rest
knots _  = error "Error: incorrect format input file. Expected two-column file \
    \ where the lines are of the form '<size> <time>"

parse :: String -> [String] -> IO FilePath
parse pr [] = usage pr >> exitFailure
parse pr ["-h"] = usage pr >> exitFailure
parse pr (fp : _) = return fp

usage :: String -> IO ()
usage pr = putStrLn $ "Usage: " ++ pr ++ " [-h] <filePath>"
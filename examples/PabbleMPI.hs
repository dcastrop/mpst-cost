module CParallel where

import Control.Monad ( zipWithM_ )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Language.SessionTypes.Common
import Language.SessionTypes.Cost

mkRoles :: Int -> GTM [Role]
mkRoles i = sequence (replicate i mkRole)

total :: Map Role Double -> Double
total = Map.foldl' max 0

err :: Double -> Double -> Double
err m s = abs (m - s) / m

_sendDist :: Double -> Double
_recvDist :: Double -> Double

_sendDist _ = 2.17e-06
_recvDist _ = 0.77e-06

cx1Send :: Map (Role, Role) (Double -> Double)
cx1Send = Map.fromList [ ((Rol r1, Rol r2), dist) | r1 <- rs, r2 <- rs ]
  where
    dist = _sendDist
    rs = [0..65]

cx1Recv :: Map (Role, Role) (Double -> Double)
cx1Recv = Map.fromList [ ((Rol r1, Rol r2), dist) | r1 <- rs, r2 <- rs ]
  where
    dist = _recvDist
    rs = [0..65]

printD :: [Double] -> [Double] -> [Double] -> IO ()
printD x y z = mapM_ putStrLn $ zipWith3 app x y z
  where
    app a b c = "& " ++ show a ++ "\n& " ++ show b ++ "\n& " ++ show c ++ "\n"


--------------------------------------------------------------------------------
-- FFT

fftTopo :: String -> [Role] -> GTM ()
fftTopo _ []  = pure ()
fftTopo _ [_] = pure ()
fftTopo c xs  =
  fftTopo "c" evens >>
  fftTopo "c" odds >>
  zipWithM_ sendT evens odds >>
  zipWithM_ sendT odds evens >>
  zipWithM_ recvT evens odds >>
  zipWithM_ recvT odds evens
  where
    (evens, odds) = split xs
    split [] = ([], [])
    split [x] = ([x], [])
    split (x:y:zs) = (x:xt, y:yt) where (xt, yt) = split zs
    sendT r1 r2 = send r1 r2 (Var $ "\\tau")
    recvT r1 r2 = recv r1 r2 (Var $ "\\tau") (CVar c)

mkFft :: Int -> CGT
mkFft i = gclose $ sequence (replicate (2^i) mkRole) >>= fftTopo "c_1"

fft :: [Double]
fft = map (total . tm) [ 1, 2, 3, 4, 5, 6, 7, 8 ]
  where
    tm i = evalTime cx1Send cx1Recv (fftVars i) (cost $ mkFft i)

fftVars :: Int -> Map String Double
fftVars i = Map.fromList [ ("c_1", fftCost 142.1 i)
                         , ("c", 1.25)
                         ]

fftCost :: Double -> Int -> Double
fftCost d i
  | i <= 1 = d + 1
  | otherwise = fftCost (d / 2 + 1)  (i - 1)

realA1Times :: [Double]
realA1Times = [ 2296.84e-9
              , 2139.24e-9
              , 2862.79e-9
              , 3300.9e-9
              ]

fftR :: [Double]
fftR =
  [ 143.016404
  , 74.175541
  , 40.817588
  , 21.755833
  , 14.519975
  , 12.467197
  , 11.970078
  , 12.686601
  ]

fftErr :: [Double]
fftErr = zipWith err fftR fft

printFft :: IO ()
printFft = printD fft fftR fftErr

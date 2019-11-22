module ScribbleGo where

import Control.Monad ( join )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Language.SessionTypes.Common
import Language.SessionTypes.Cost

--- SCATTER-GATHER
scatter :: Role -> [Role] -> GTM ()
scatter _ []     = pure ()
scatter p (q:qs) = message p q (Var $ "\\tau_s") (CVar $ "c_s") >>
                   scatter p qs

gather :: [Role] -> Role -> GTM ()
gather []     _ = pure ()
gather (q:qs) p = message q p (Var $ "\\tau_g") (CVar $ "c_g") >>
                  gather qs p

scatterGather :: Role -> Int -> Role -> GTM () -> GTM ()
scatterGather p i q k = do
  ps <- sequence $ replicate i mkRole
  scatter p ps
  gather ps q
  k

mkSC :: Int -> CGT
mkSC i = gclose $ do
  p <- mkRole
  q <- mkRole
  grec 1 $ scatterGather p i q

mkRoles :: Int -> GTM [Role]
mkRoles i = sequence (replicate i mkRole)

-- go-aa
allToAllM :: [Role] -> [Role] -> GTM ()
allToAllM ps qs = mapM_ (`scatter` qs) ps

goAA :: Int -> CGT
goAA i
  | i >= 2 = gclose $ grec 1 $ (join (allToAllM <$> mkRoles f <*> mkRoles t) >>)
  | otherwise = gclose $ pure ()
  where
    f = i `div` 2
    t = i - f

goTcpSend :: Int -> Map (Role, Role) (Double -> Double)
goTcpSend i
  | i < 2 = Map.empty
  | otherwise
  = Map.fromList [ ((Rol r1, Rol r2), dist) | r1 <- rs1 ++ rs2, r2 <- rs2 ++ rs1 ]
  where
    dist _ = 2.0532e-06
    j = i `div` 2
    k = i - j
    rs1 = [0..j]
    rs2 = [j+1..k]

goTcpRecv :: Int -> Map (Role, Role) (Double -> Double)
goTcpRecv i
  | i < 2 = Map.empty
  | otherwise
  = Map.fromList [ ((Rol r1, Rol r2), dist) | r1 <- rs1 ++ rs2, r2 <- rs2 ++ rs1 ]
  where
    dist _ = 2.27e-06
    j = i `div` 2
    k = i - j
    rs1 = [0..j]
    rs2 = [j+1..k]

goVars :: Map String Double
goVars = Map.fromList [ ("\\tau_s", 2)
                      , ("\\tau_g", 2)
                      , ("c_s", 1e-9)
                      , ("c_g", 1e-9)
                      ]

total :: Map Role Double -> Double
total = Map.foldl' max 0

goAATimes :: [Double]
goAATimes = map (total . tm) [2, 3, 4]
  where
    tm i = evalTime (goTcpSend i) (goTcpRecv i) goVars (cost $ goAA i)

realAATimes :: [Double]
realAATimes =
  [ 2125.19e-9
  , 4081.12e-9
  , 4454.03e-9
  ]

errAATimes :: [Double]
errAATimes = zipWith err realAATimes goAATimes

err :: Double -> Double -> Double
err m s = abs (m - s) / m

-- go-1a
oneToAllM :: Role -> [Role] -> GTM ()
oneToAllM = scatter

-- go-a1
allToOneM :: [Role] -> Role -> GTM ()
allToOneM = gather

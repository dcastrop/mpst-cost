module OCamlGT where

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Language.SessionTypes.Common
import Language.SessionTypes.Cost

----------------------------------------------------------------------
-- Keigo's paper, Recursive pingpong

rpingpong :: CGT
rpingpong = gclose $ do
  p <- mkRole
  q <- mkRole
  grec 1 $ \x -> do
    message p q (Var "\\tau_1") (CVar "c_1")
    message q p (Var "\\tau_2") (CVar "c_2")
    x

ppSizes :: Integer -> Map String Double
ppSizes nints = Map.fromList
  [ ("\\tau_1", fromInteger nints * 2) -- in bytes
  , ("\\tau_2", fromInteger nints * 2)
  , ("c_1", 5.2e-9)
  , ("c_2", 5.2e-9)
  ]

ppIpcSend :: Map (Role, Role) (Double -> Double)
ppIpcSend = Map.fromList
  [ ((Rol 0, Rol 1), ipcDist)
  , ((Rol 1, Rol 0), ipcDist)
  ]
  where
    ipcDist s = 1.13e-6 + s * 2.39e-10

ppIpcRecv :: Map (Role, Role) (Double -> Double)
ppIpcRecv = Map.fromList
  [ ((Rol 0, Rol 1), ipcDist)
  , ((Rol 1, Rol 0), ipcDist)
  ]
  where
    ipcDist s = 2e-6 + s * log s * 2.42e-11

ppThro :: Time
ppThro = throughput rpingpong

ppCost :: Time
ppCost = cost rpingpong

total :: Map Role Double -> Double
total = Map.foldl' max 0

ppIpcTimes :: [Double]
ppIpcTimes = map evalIpc [1, 10, 100, 1000, 10000, 100000]
  where
    evalIpc i = total $ evalTime ppIpcSend ppIpcRecv (ppSizes i) ppCost

realIpcTimes :: [Double]
realIpcTimes = [5.95e-6, 6.12e-6, 6.19e-6, 7.8e-6, 20.85e-6, 218.66e-6]

ppIpcError :: [Double]
ppIpcError = zipWith err realIpcTimes ppIpcTimes

ppEvSend :: Map (Role, Role) (Double -> Double)
ppEvSend = Map.fromList
  [ ((Rol 0, Rol 1), evDist)
  , ((Rol 1, Rol 0), evDist)
  ]
  where
    evDist _ = 1.6e-6

ppEvRecv :: Map (Role, Role) (Double -> Double)
ppEvRecv = Map.fromList
  [ ((Rol 0, Rol 1), evDist)
  , ((Rol 1, Rol 0), evDist)
  ]
  where
    evDist _ = 1.59e-6

ppEvTimes :: [Double]
ppEvTimes = map evalEv [1, 10, 100, 1000]
  where
    evalEv i = total $ evalTime ppEvSend ppEvRecv (ppSizes i) ppCost

ppLwtSend :: Map (Role, Role) (Double -> Double)
ppLwtSend = Map.fromList
  [ ((Rol 0, Rol 1), evDist)
  , ((Rol 1, Rol 0), evDist)
  ]
  where
    evDist _ = 1.025e-7

ppLwtRecv :: Map (Role, Role) (Double -> Double)
ppLwtRecv = Map.fromList
  [ ((Rol 0, Rol 1), evDist)
  , ((Rol 1, Rol 0), evDist)
  ]
  where
    evDist _ = 1.025e-7

ppLwtTimes :: [Double]
ppLwtTimes = map evalEv [1, 10, 100, 1000]
  where
    evalEv i = total $ evalTime ppLwtSend ppLwtRecv (ppSizes i) ppCost


err :: Double -> Double -> Double
err m s = abs (m - s) / m

----------------------------------------------------------------------
----------------------------------------------------------------------

module Cost1 where

import Control.Monad ( zipWithM_, replicateM )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Language.SessionTypes.Common
import Language.SessionTypes.Cost

example1 :: CGT
example1 = gclose $ do
  p <- mkRole
  q <- mkRole
  grec 3 $ \x -> do
    message p q (Var "\\tau_1") (CVar "c_1")
    x

pingpong :: CGT
pingpong = gclose $ do
 p <- mkRole
 q <- mkRole
 message p q (Var "\\tau_1") (CVar "c_1")
 message q p (Var "\\tau_2") (CVar "c_2")

--- PIPELINE
pipeline :: Integer -> Role -> GTM () -> GTM ()
pipeline i p k
  | i <= 0 = k
  | otherwise = do
      q <- mkRole
      message p q (Var $ "\\tau_" ++ show i) (CVar $ "c_" ++ show i)
      pipeline (i-1) q k

pipe2 :: CGT
pipe2 = gclose $ mkRole >>= \r -> pipeline 2 r (pure ())

rpipe2 :: CGT
rpipe2 = gclose $ mkRole >>= \r -> grec 3 $ pipeline 2 r


--- SCATTER-GATHER
scatter :: Integer -> Role -> GTM [Role]
scatter i p
  | i <= 0 = pure []
  | otherwise = do
      q <- mkRole
      message p q (Var "\\tau_1") (CVar "c_1")
      (q:) <$> scatter (i-1) p

gather :: Role -> [Role] -> GTM ()
gather _ [] = pure ()
gather p (q:rs) = do
  message q p (Var "\\tau_2") (CVar "c_2")
  gather p rs

scatterGather :: Integer -> GTM () -> GTM ()
scatterGather i k = do
  p <- mkRole
  rs <- scatter i p
  q <- mkRole
  gather q rs
  k

sc2 :: CGT
sc2 = gclose $ scatterGather 2 (pure ())

rsc2 :: CGT
rsc2 = gclose $ grec 3 $ \k -> scatterGather 2 k


--- RING
ring :: [Role] -> GTM ()
ring ps@(p : _) = sendAll ps >> recvAll ps
  where
    sendAll (q : qs@(r : _))
      = send q r (Var "\\tau") >> sendAll qs
    sendAll [q] = send q p (Var "\\tau")
    sendAll _ = pure ()

    recvAll (q : qs@(r : _))
      = recv q r (Var "\\tau") (CVar "c") >> recvAll qs
    recvAll [q] = recv q p (Var "\\tau") (CVar "c")
    recvAll _ = pure ()
ring [] = pure ()

ringTopo :: Int -> GTM ()
ringTopo i = replicateM i mkRole >>= ring

ring2 :: CGT
ring2 = gclose $ ringTopo 2

rring2 :: CGT
rring2 = gclose $ grec 3 $ \k -> ringTopo 2 >> k


-- FFT
fftTopo :: [Role] -> GTM ()
fftTopo []  = pure ()
fftTopo [_] = pure ()
fftTopo xs  =
  fftTopo evens >>
  fftTopo odds >>
  zipWithM_ sendT evens odds >>
  zipWithM_ sendT odds evens >>
  zipWithM_ recvT evens odds >>
  zipWithM_ recvT odds evens
  where
    (evens, odds) = split xs
    split [] = ([], [])
    split [x] = ([x], [])
    split (x:y:zs) = (x:xt, y:yt) where (xt, yt) = split zs
    sendT r1 r2 = send r1 r2 (Var "\\tau")
    recvT r1 r2 = recv r1 r2 (Var "\\tau") (CVar "c")

mkFft :: Int -> GTM ()
mkFft i = Control.Monad.replicateM (2^i) mkRole >>= fftTopo

fft2 :: CGT
fft2 = gclose $ mkFft 2


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

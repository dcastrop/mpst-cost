module Cost1 where

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

rpingpong :: CGT
rpingpong = gclose $ do
  p <- mkRole
  q <- mkRole
  grec 2 $ \x -> do
    message p q (Var "\\tau_1") (CVar "c_1")
    message q p (Var "\\tau_2") (CVar "c_2")
    x

exampleVars :: Map String Double
exampleVars = Map.fromList
  [ ("\\tau_1", 0.7)
  , ("\\tau_2", 0.8)
  , ("c_1", 1.2)
  , ("c_2", 2.6)
  ]

exampleTopo :: Map (Role, Role) (Double -> Double)
exampleTopo = Map.fromList
  [ ((Rol 0, Rol 1), \s -> s * 10) -- Constant time to send/recv
  , ((Rol 1, Rol 0), \s -> s * 10)
  ]

thro1 :: Time
thro1 = throughput example1

thro2 :: Time
thro2 = throughput rpingpong

cost1 :: Time
cost1 = cost example1

cost2 :: Time
cost2 = cost rpingpong

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
      message p q (Var $ "\\tau_1") (CVar $ "c_1")
      (q:) <$> scatter (i-1) p

gather :: Role -> [Role] -> GTM ()
gather _ [] = pure ()
gather p (q:rs) = do
  message q p (Var $ "\\tau_2") (CVar $ "c_2")
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
      = send q r (Var $ "\\tau") >> sendAll qs
    sendAll [q] = send q p (Var $ "\\tau")
    sendAll _ = pure ()

    recvAll (q : qs@(r : _))
      = recv q r (Var $ "\\tau") (CVar $ "c") >> recvAll qs
    recvAll [q] = recv q p (Var $ "\\tau") (CVar $ "c")
    recvAll _ = pure ()
ring [] = pure ()

ring2 :: CGT
ring2 = gclose $ sequence (replicate 2 mkRole) >>= ring

rring2 :: CGT
rring2 = gclose $ grec 3 $ \k -> sequence (replicate 2 mkRole) >>= ring >> k

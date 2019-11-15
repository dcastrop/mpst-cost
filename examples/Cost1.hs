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

example2 :: CGT
example2 = gclose $ do
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
thro2 = throughput example2

cost1 :: Time
cost1 = cost example1

cost2 :: Time
cost2 = cost example2

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

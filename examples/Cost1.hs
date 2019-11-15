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
    message p q (Var "s1") (CVar "c1")
    x

example2 :: CGT
example2 = gclose $ do
  p <- mkRole
  q <- mkRole
  grec 3 $ \x -> do
    message p q (Var "s1") (CVar "c1")
    message q p (Var "s2") (CVar "c2")
    x

exampleVars :: Map String Double
exampleVars = Map.fromList
  [ ("s1", 0.7)
  , ("s2", 0.8)
  , ("c1", 1.2)
  , ("c2", 2.6)
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

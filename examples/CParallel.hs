module CParallel where

import Control.Monad ( join, replicateM )
import Data.Map.Strict ( Map )
import Data.List ( transpose )
import qualified Data.Map.Strict as Map
import Language.SessionTypes.Common
import Language.SessionTypes.Cost

mkRoles :: Int -> GTM [Role]
mkRoles i = replicateM i mkRole

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
-- RING

ringM :: [Role] -> GTM ()
ringM ps@(p : _) = sendAll p ps >> recvAll p ps
ringM [] = pure ()

ringP :: Int -> CGT
ringP i = gclose $ join (ringM <$> mkRoles i)

seqNBCost :: Double -> Double
seqNBCost d = 177 / d + 9.08e-1 + d * log d * 2.3e-3

nbodyVars :: Int -> Map String Double
nbodyVars p = Map.fromList [ ("c", seqNBCost $ fromIntegral p)
                           , ("\\tau", 10**6)
                           ]
nbody :: [Double]
nbody = map go [1, 4, 16, 32, 64]
  where
    go i = total $ evalTime cx1Send cx1Recv (nbodyVars i) (cost $ ringP i)

nbodyR :: [Double]
nbodyR = [ 177.908376
         , 44.710916
         , 11.097
         , 7.8411185
         , 4.282142
         ]

nbodyErr :: [Double]
nbodyErr = zipWith err nbodyR nbody

--------------------------------------------------------------------------------
-- MESH

sendAll :: Role -> [Role] -> GTM ()
sendAll p (q : qs@(r : _))
  = send q r (Var "\\tau") >> sendAll p qs
sendAll p [q] = send q p (Var "\\tau")
sendAll _ _ = pure ()

recvAllC :: String -> Role -> [Role] -> GTM ()
recvAllC c p (q : qs@(r : _))
  = recv q r (Var "\\tau") (CVar c) >> recvAllC c p qs
recvAllC c p [q] = recv q p (Var "\\tau") (CVar c)
recvAllC _ _ _ = pure ()

recvAll :: Role -> [Role] -> GTM ()
recvAll = recvAllC "c"

meshM :: [[Role]] -> GTM ()
meshM ps = mapM_ sAll ps >> mapM_ sAll sp >>
           mapM_ (rAll False) ps >> mapM_ (rAll True) sp
  where
    sp = transpose ps
    sAll qs@(p:_) = sendAll p qs
    sAll _ = pure ()
    rAll b qs@(p:_)
      | b         = recvAll p qs
      | otherwise = recvAllC "c_w" p qs
    rAll _ _ = pure ()

mkRoleMatrix :: Int -> GTM [[Role]]
mkRoleMatrix i = replicateM i (mkRoles i)

meshP :: Int -> CGT
meshP i = gclose $ join (meshM <$> mkRoleMatrix i)

solver :: [Double]
solver = map go [1, 2, 4, 6, 8]
  where
    go i = total $ evalTime cx1Send cx1Recv (solverVars i) (cost $ meshP i)

solverVars :: Int -> Map String Double
solverVars i = Map.fromList [ ("c", solverCost $ fromIntegral i)
                            ]

solverCost :: Double -> Double
solverCost d = 4.5836835 / (d*d) + 6.4 / d

solverR :: [Double]
solverR =
  [ sum [10.5836835, 10.5836835, 10.5836835, 10.5836835, 10.5836835, 10.5836835] / 6
  , sum [3.488013  , 3.543354  , 4.595234  , 4.808623  , 4.308554  , 5.927986  ] / 6
  , sum [1.882221  , 1.801822  , 1.217853  , 1.679229  , 1.883321  , 1.856217  ] / 6
  , sum [1.099540  , 1.371320  , 1.353402  , 1.356969  , 1.442624  , 1.230535  ] / 6
  , sum [0.755533  , 0.783954  , 0.634727  , 0.715382  , 0.737718  , 0.715501  ] / 6
  ]

solverErr :: [Double]
solverErr = zipWith err solverR solver

--- ad predictor & wordcount

scatter :: Role -> [Role] -> GTM ()
scatter _ []     = pure ()
scatter p (q:qs) = message p q (Var "\\tau_s") (CVar "c_s") >>
                   scatter p qs

gather :: [Role] -> Role -> GTM ()
gather []     _ = pure ()
gather (q:qs) p = message q p (Var "\\tau_g") (CVar "c_g") >>
                  gather qs p

scatterGather :: Role -> Int -> Role -> GTM ()
scatterGather p i q = do
  ps <- replicateM i mkRole
  scatter p ps
  gather ps q

mkSC :: Int -> CGT
mkSC i = gclose $ do
  p <- mkRole
  q <- mkRole
  scatterGather p i q

ad :: [Double]
ad = map go [1, 2, 4, 8, 12, 16, 24, 48, 64]
  where
    go i = total $ evalTime cx1Send cx1Recv (adVars i) (cost $ mkSC i)

adR :: [Double]
adR =
  [ sum [ 653,  660] / 2
  , sum [ 293,  275] / 2
  , sum [ 141,  154] / 2
  , sum [ 74 , 73  ] / 2
  , sum [  55,  74 ] / 2
  , sum [  65,  65 ] / 2
  , sum [  47,  44 ] / 2
  , sum [  54,  53 ] / 2
  , sum [  63,  64 ] / 2
  ]


adVars :: Int -> Map String Double
adVars i = Map.fromList [ ("c_s", adCost $ fromIntegral i)
                            ]
adCost :: Double -> Double
adCost d = 656 / d + d

adErr :: [Double]
adErr = zipWith err adR ad

printAd :: IO ()
printAd = printD ad adR adErr

---------------------------------------------------------------------------------
-- WordCount

wc :: [Double]
wc = map go [1, 2, 4, 8, 12, 16, 24, 48, 64]
  where
    go i = total $ evalTime cx1Send cx1Recv (wcVars i) (cost $ mkSC i)

wcR :: [Double]
wcR =
  [ sum [ 42, 72 ] / 2
  , sum [ 26, 29 ] / 2
  , sum [ 17, 22 ] / 2
  , sum [ 13, 19 ] / 2
  , sum [ 12, 20 ] / 2
  , sum [ 12, 13 ] / 2
  , sum [ 26, 13 ] / 2
  , sum [ 23, 23 ] / 2
  , sum [ 23, 23 ] / 2
  ]


wcVars :: Int -> Map String Double
wcVars i = Map.fromList [ ("c_s", wcCost $ fromIntegral i)
                            ]
wcCost :: Double -> Double
wcCost d = 57 / d + log d * 4.75

wcErr :: [Double]
wcErr = zipWith err wcR wc

printWc :: IO ()
printWc = printD wc wcR wcErr

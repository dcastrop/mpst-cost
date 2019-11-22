module PabbleMPI where

import Control.Monad ( join )
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
-- RING

ringM :: [Role] -> GTM ()
ringM ps@(p : _) = sendAll ps >> recvAll ps
  where
    sendAll (q : qs@(r : _))
      = send q r (Var $ "\\tau") >> sendAll qs
    sendAll [q] = send q p (Var $ "\\tau")
    sendAll _ = pure ()

    recvAll (q : qs@(r : _))
      = recv q r (Var $ "\\tau") (CVar $ "c") >> recvAll qs
    recvAll [q] = recv q p (Var $ "\\tau") (CVar $ "c")
    recvAll _ = pure ()
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
-- RING

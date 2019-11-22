module ScribbleGo where

-- import Control.Monad ( zipWithM_ )
-- import Data.Map.Strict ( Map )
-- import qualified Data.Map.Strict as Map
import Language.SessionTypes.Common
import Language.SessionTypes.Cost

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

mkSC :: Integer -> CGT
mkSC i = gclose $ grec 1 $ scatterGather i

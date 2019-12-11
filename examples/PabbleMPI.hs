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

-- sizes: 67108864
printFft :: IO ()
printFft = printD fft fftR fftErr

--------------------------------------------------------------------------------
-- Mergesort

data PTree = Leaf Role | Node Role PTree PTree
  deriving Show

ps :: PTree -> [Role]
ps (Leaf p) = [p]
ps (Node _ l r) = ps l ++ ps r

hd :: PTree -> Role
hd (Leaf p) = p
hd (Node p _ _) = p

dcTree :: Int -> Role -> GTM PTree
dcTree i p
  | i <= 1 = pure $ Leaf p
  | otherwise = do
      q <- mkRole
      r <- mkRole
      t1 <- dcTree (i-1) q
      t2 <- dcTree (i-1) r
      pure $ Node p t1 t2

choices :: Role -> [Role] -> (Label, GTM ()) -> (Label, GTM ()) -> GTM ()
choices _ [] _ _ = pure ()
choices a (b:cs) (ll, gl) (lr, gr)
  = choice a b ((ll ... choiceL ll cs gl) .| ((lr ... choiceL lr cs gr) .| mempty))
  where
    choiceL _l [] g = g
    choiceL l (c:ds) g = choice a c ((l ... choiceL l ds g) .| mempty)

divConq :: String -> PTree -> GTM ()
divConq _ (Leaf _) = pure ()
divConq s (Node p t1 t2) = do
  choices p (ps t1 ++ ps t2) (Lbl 1, pure ()) (Lbl 2, body)
  where
    body = message p (hd t1) (Var "\\tau") (CVar s) >>
      message p (hd t2) (Var "\\tau") (CVar s) >>
      divConq "c_s" t1 >>
      divConq "c_s" t2 >>
      message (hd t1) p (Var "\\tau") (CVar "c_m") >>
      message (hd t2) p (Var "\\tau") (CVar "c_m")

dcP :: Int -> CGT
dcP i = gclose $ do
  p <- mkRole
  t <- dcTree i p
  divConq "c" t


-- 536870912
msR :: [Double]
msR =
  [ 98.145231
  , 53.183744
  , 31.325671
  , 18.091493
  , 14.214225
  ]

ms :: [Double]
ms = 98.145231 : map (total . tm) [ 2, 3, 4, 5 ]
  where
    tm i = evalTime cx1Send cx1Recv (dcVars i) (cost $ dcP i)

dcVars :: Int -> Map String Double
dcVars i = Map.fromList [ ("c_m", 0)
                         , ("c", dcCost 98.145231 i 1)
                         ]

dcCost :: Double -> Int -> Int -> Double
dcCost d i j -- = d / fromIntegral i
  | i <= 1 = d
  | otherwise = dcCost (d / 2 + 4.5) (i - 1) (j+1)


-- divConq i p
--   | i <= 1 = pure (Left p)
--   | otherwise = do
--       q <- mkRole
--       r <- mkRole
--
--       divConq (i-1)

msErr :: [Double]
msErr = zipWith err msR ms

-- sizes: 67108864
printMs :: IO ()
printMs = printD ms msR msErr

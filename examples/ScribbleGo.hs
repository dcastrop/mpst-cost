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

scatterGather :: Role -> Int -> Role -> GTM ()
scatterGather p i q = do
  ps <- sequence $ replicate i mkRole
  scatter p ps
  gather ps q

mkSC :: Int -> CGT
mkSC i = gclose $ do
  p <- mkRole
  q <- mkRole
  scatterGather p i q

mkRoles :: Int -> GTM [Role]
mkRoles i = sequence (replicate i mkRole)

-- go-aa
allToAllM :: [Role] -> [Role] -> GTM ()
allToAllM ps qs = mapM_ (`scatter` qs) ps

goAA :: Int -> CGT
goAA i
  | i >= 2 = gclose $ join (allToAllM <$> mkRoles f <*> mkRoles t)
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
    dist _ = 1.8532e-06
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
    dist _ = 0.47e-06
    j = i `div` 2
    k = i - j
    rs1 = [0..j]
    rs2 = [j+1..k]

goVars :: Map String Double
goVars = Map.fromList [ ("\\tau_s", 2)
                      , ("\\tau_g", 2)
                      , ("c_s", 1e-7)
                      , ("c_g", 1e-9)
                      ]

total :: Map Role Double -> Double
total = Map.foldl' max 0

goAATimes :: [Double]
goAATimes = map (total . tm) [2, 3, 4]
  where
    tm i = evalTime (goTcpSend 10) (goTcpRecv 10) goVars (cost $ goAA i)

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

go1A :: Int -> CGT
go1A i = gclose $ join (oneToAllM <$> mkRole <*> mkRoles i)

go1AVars :: Map String Double
go1AVars = Map.fromList [ ("\\tau_s", 2)
                      , ("\\tau_g", 2)
                      , ("c_s", 2.1e-6)
                      , ("c_g", 0e-6)
                      ]

go1ATimes :: [Double]
go1ATimes = map (total . tm) [1, 2, 3, 4]
  where
    tm i = evalTime (goTcpSend 16) (goTcpRecv 16) go1AVars (cost $ go1A i)

real1ATimes :: [Double]
real1ATimes = [ 2315.19e-9
              , 4458.45e-9
              , 7639.46e-9
              , 9851.17e-9
              ]

err1ATimes :: [Double]
err1ATimes = zipWith err real1ATimes go1ATimes

-- go-a1
allToOneM :: [Role] -> Role -> GTM ()
allToOneM = gather

goA1 :: Int -> CGT
goA1 i = gclose $ join (allToOneM <$> mkRoles i <*> mkRole)

goA1Times :: [Double]
goA1Times = map (total . tm) [1, 2, 3, 4]
  where
    tm i = evalTime (goTcpSend 16) (goTcpRecv 16) goVars (cost $ goA1 i)

realA1Times :: [Double]
realA1Times = [ 2296.84e-9
              , 2139.24e-9
              , 2862.79e-9
              , 3300.9e-9
              ]

errA1Times :: [Double]
errA1Times = zipWith err realA1Times goA1Times

--------------------------------------------------------------------------------
-- CLBG

-- Spectral Norm
--
-- global protocol Proto(role A(k), role B(k))
-- {
--   choice at A[1] {
--     times(int) from A[1..1] to B[1..k]; // u x v
--     done(int)  from B[1..k] to A[1..1];
--     next(int) from A[1..1] to B[1..k]; // v x u
--     done(int)  from B[1..k] to A[1..1];
--
--     timestr(int) from A[1..1] to B[1..k]; // v x u
--     done(int)  from B[1..k] to A[1..1];
--     next(int) from A[1..1] to B[1..k]; // v x u
--     done(int)  from B[1..k] to A[1..1];
--
--     do Proto(A, B);
--   } or {
--     end(int) from A[1..1] to B[1..k];
--   }
-- }

-- FIXME: Fixity of labels & choices
spectralNorm :: Role -> [Role] -> GTM ()
spectralNorm a bs = do
  l1 <- mkLabel
  l2 <- mkLabel
  grec 1 $ \k -> do
    choices bs (l1, body >> k) (l2, mapM_ sndEnd bs)
  where
    choices [] _ _ = pure ()
    choices (b:cs) (ll, gl) (lr, gr)
      = choice a b ((ll ... choiceL ll cs gl) .| ((lr ... choiceL lr cs gr) .| mempty))
    choiceL _l [] g = g
    choiceL l (b:cs) g = choice a b ((l ... choiceL l cs g) .| mempty)
    sndEnd   b = message a b (Var "\\tau_e") (CVar "c_e")
    sndTimes b = message a b (Var "\\tau_t") (CVar "c_t")
    sndNext b = message a b (Var "\\tau_n") (CVar "c_n")
    rcvDone  b = message b a (Var "\\tau_d") (CVar "c_d")
    sndTimesR b = message a b (Var "\\tau_r") (CVar "c_r")
    body = do
      mapM_ sndTimes bs
      mapM_ rcvDone bs
      mapM_ sndNext bs
      mapM_ rcvDone bs
      mapM_ sndTimesR bs
      mapM_ rcvDone bs
      mapM_ sndNext bs
      mapM_ rcvDone bs

snP :: Int -> CGT
snP i = gclose $ join (spectralNorm <$> mkRole <*> mkRoles i)

-- Local computation cost from measurements using input size 11000
snVars :: Int -> Map String Double
snVars i = Map.fromList [ ("c_t", 5.79 / fromIntegral i + 2e-2 * fromIntegral i)
                        , ("c_r", 5.79 / fromIntegral i + 2e-2 * fromIntegral i)
                        ]

sn :: [Double]
sn = map (total . tm) [1, 2, 3, 4]
  where
    tm i = evalTime (goTcpSend 16) (goTcpRecv 16) (snVars i) (cost $ snP i)


snReal :: [Double]
snReal = [ 11577124467e-9
         , 5809008740e-9
         , 3948746084e-9
         , 3052476325e-9
         ]

snErr :: [Double]
snErr = zipWith err snReal sn


--------------------------------------------------------------------------------
-- KNuc

-- global protocol Proto(role A(k), role B(k), role S(k))
-- {
--   sort(int)       from A[1..1] to S[1..2];
--   match(strings)  from A[1..1] to B[1..k];
--   done(int)       from S[1..2] to A[1..1];
--   gather(strings) from B[1..k] to A[1..1];
-- }
knucM :: Role -> Role -> Role -> [Role] -> GTM ()
knucM a s1 s2 bs = do
  message a s1 (Var "\\tau_s") (CVar "c_s")
  message a s2 (Var "\\tau_s") (CVar "c_s")
  mapM_ sndMatch bs
  message s1 a (Var "\\tau_d") (CVar "c_d")
  message s2 a (Var "\\tau_d") (CVar "c_d")
  mapM_ rcvMatch bs
  where
    sndMatch b = message a b (Var "\\tau_m") (CVar "c_m")
    rcvMatch b = message b a (Var "\\tau_g") (CVar "c_g")

knucP :: Int -> CGT
knucP i = gclose $ join (knucM <$> mkRole <*> mkRole <*> mkRole <*> mkRoles i)

knVars :: Int -> Map String Double
knVars i = Map.fromList [ ("c_s", 7.65)
                        , ("c_d", 0.1)
                        , ("c_m", 10.65 + 0.15 * d * log d)
                        , ("c_g", 0.2e-1 + 0.26 * d * log d)
                        ]
  where
    d = fromIntegral i

knuc :: [Double]
knuc = map (total . tm) [1, 2, 3, 4]
  where
    tm i = evalTime (goTcpSend 16) (goTcpRecv 16) (knVars i) (cost $ knucP i)


-- input size: 254166667
knucR :: [Double]
knucR  = [ 10649992008e-9
         , 11131557520e-9
         , 13009752428e-9
         , 17168755778e-9
         ]

knucErr :: [Double]
knucErr = zipWith err knucR knuc


--------------------------------------------------------------------------------
-- Regex

-- global protocol Proto(role A(k), role B(k), role C(k))
-- {
--   count(string)   from A[1..1] to B[1..k];
--   measure(int)    from A[1..1] to C[1..1];
--   donec(string)   from B[1..k] to A[1..1];
--   len(int)        from C[1..1] to A[1..1];
-- }


dnaM :: Role -> [Role] -> Role -> GTM ()
dnaM a bs c = do
  mapM_ sendCount bs
  message a c (Var "\\tau_m") (CVar "c_m")
  mapM_ sendDone bs
  message c a (Var "\\tau_l") (CVar "c_l")
  where
    sendCount b = message a b (Var "\\tau_c") (CVar "c_c")
    sendDone b = message b a (Var "\\tau_d") (CVar "c_d")


dnaP :: Int -> CGT
dnaP i = gclose $ join (dnaM <$> mkRole <*> mkRoles i <*> mkRole)

dnaVars :: Int -> Map String Double
dnaVars _i = Map.fromList [ ("c_c", 2461211137e-9)
                          , ("c_d", 0.34)
                          , ("c_l", 0.2)
                         ]
--  where
--    d = fromIntegral i

dna :: [Double]
dna = map (total . tm) [1, 2, 3, 4]
  where
    tm i = evalTime (goTcpSend 16) (goTcpRecv 16) (dnaVars i) (cost $ dnaP i)


-- size 25416745
dnaR :: [Double]
dnaR  = [ 2931456050e-9
        , 3387995755e-9
        , 3663504164e-9
        , 4011542040e-9
        ]

dnaErr :: [Double]
dnaErr = zipWith err dnaR dna

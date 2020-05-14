module Language.SessionTypes.Seq
    ( seqCost
    ) where

import Data.Maybe

seqCost :: FilePath -> IO (Double -> Double)
seqCost fp = do
    c <- lines <$> readFile fp
    case c of
        [kn, poly] -> pure $ buildFunction fp (read kn) (read poly)
        _ -> error $ "Error: cannot parse coefficients file '" ++ fp
             ++ "'\n" ++ show c

-- XXX: Rewrite as fold
findIdx :: Double -> [(Double, Double)] -> Either Int Double
findIdx x = go Nothing
    where
        go ix [] = Left $ maybe 0 (+ (-1)) ix
        go ix ((y, v) : knots)
            = case compare x y of
                LT -> Left $ fromMaybe 0 ix
                EQ -> Right v
                GT -> go (Just $ maybe 0 (+1) ix) knots 

instantiate :: Double -> [Double] -> Double
instantiate v [t, z, y, x] = v ^ (3 :: Int) * x + v ^ (2 :: Int) * y + v * z + t
instantiate _ _ = error "wrong number of coefficients when parsing \
    \ interpolation data"

buildFunction :: FilePath -> [(Double, Double)] -> [[Double]] -> Double -> Double
buildFunction fp knots poly
    | length knots -1 == length poly && all (== 4) len
    = \x -> case findIdx x knots of
                Left ix -> instantiate x (poly !! ix)
                Right v -> v
    | otherwise
    = error $ "Error: cannot parse coefficients file '" ++ fp ++ "'" ++
                "\n\t" ++ show (length knots) ++ "\n\t" ++ show (length poly)
    where
        len = map length poly
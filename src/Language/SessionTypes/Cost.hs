{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.SessionTypes.Cost
  ( CGT
  , showCGT
  , printCGT
  , Size (..)
  , Cost (..)
  , Time
  , bCost
  , rCost
  , sCost
  , showCost
  , LAlt
  , GAlt
  , (...)
  , (.|)
  , message
  , choice
  , grec
  , gclose
  , mkRole
  , mkLabel
  , throughput
  , showEqns
  , printEqns
  , cost
  , evalTime
  ) where

import Control.Monad.State.Strict
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Text.Prettyprint.Doc ( Pretty, pretty )
import qualified Data.Text.Prettyprint.Doc as Pretty

import Language.SessionTypes.Common

data Size i
  = Var i
  | K Integer
  | SAdd (Size i) (Size i)
  | SSub (Size i) (Size i)
  | SMul (Size i) (Size i)
  | SDiv (Size i) (Size i)
  deriving Show

instance Pretty i => Pretty (Size i) where
  pretty (Var i) = pretty i
  pretty (K i) = pretty i
  pretty (SAdd l r) = Pretty.hsep [ pretty l, pretty "+", pretty r]
  pretty (SSub l r) = Pretty.hsep [ pretty l, pretty "-", prettyP r]
    where
      prettyP SAdd{} = Pretty.parens $ pretty r
      prettyP SSub{} = Pretty.parens $ pretty r
      prettyP _ =  pretty r
  pretty (SMul l r) = Pretty.hsep [ prettyP l, pretty "*", prettyP r]
    where
      prettyP SAdd{} = Pretty.parens $ pretty r
      prettyP SSub{} = Pretty.parens $ pretty r
      prettyP _ =  pretty r
  pretty (SDiv l r) = Pretty.hsep [ prettyP1 l, pretty "/", prettyP2 r]
    where
      prettyP1 SAdd{} = Pretty.parens $ pretty r
      prettyP1 SSub{} = Pretty.parens $ pretty r
      prettyP1 _ =  pretty r
      prettyP2 SAdd{} = Pretty.parens $ pretty r
      prettyP2 SSub{} = Pretty.parens $ pretty r
      prettyP2 SMul{} = Pretty.parens $ pretty r
      prettyP2 SDiv{} = Pretty.parens $ pretty r
      prettyP2 _ =  pretty r

type VSize = Size String

data Cost i
  = CSize (Size i)
  | CVar String
  | CRec Role
  | CAdd (Cost i) (Cost i)
  | CMax (Cost i) (Cost i)
  | CMul (Size i) (Cost i)
  | CSend Role Role VSize
  | CRecv Role Role VSize
  | CDelta Time
  deriving Show

instance Pretty i => Pretty (Cost i) where
  pretty (CSize i) = pretty i
  pretty (CRec r) = pretty r
  pretty (CVar i) = pretty i
  pretty (CAdd l r) = Pretty.hsep [ pretty l, pretty "+", pretty r]
  pretty (CMul l r) = Pretty.hsep [ prettyPs l, pretty "*", prettyPc r]
    where
      prettyPs SAdd{} = Pretty.parens $ pretty l
      prettyPs SSub{} = Pretty.parens $ pretty l
      prettyPs _ =  pretty l
      prettyPc CAdd{} = Pretty.parens $ pretty r
      prettyPc _ =  pretty r
  pretty (CMax l r) = go [l, r] []
    where
      go (CMax hl hr : t) mm = go (hl : hr : t) mm
      go (h : t) [] = go t [pretty h]
      go (h : t) mm = go t (pretty h <> pretty "," : mm)
      go [] mm = Pretty.hsep [ pretty "max", Pretty.parens $ Pretty.hsep mm]
  pretty (CSend f t sz) =
    Pretty.hsep [ pretty "T_send"
                , Pretty.parens $ Pretty.hsep [ pretty f <> pretty ","
                                              , pretty t <> pretty ","
                                              , pretty sz
                                              ]
                ]
  pretty (CRecv f t sz) =
    Pretty.hsep [ pretty "T_recv"
                , Pretty.parens $ Pretty.hsep [ pretty f <> pretty ","
                                              , pretty t <> pretty ","
                                              , pretty sz
                                              ]
                ]
  pretty (CDelta t) =
    pretty "\\Delta" <> Pretty.parens (Pretty.hsep $ map time $ Map.toList t)
    where
      time (r, c) = pretty r Pretty.<+> pretty "=" Pretty.<+> pretty c <> pretty ";"

showCost :: Pretty i => Cost i -> String
showCost = show . pretty

type VCost = Cost String

-- data Msg pl ann =
--   Msg { rfrom :: ![Role]
--       , rto :: ![Role]
--       , rty :: pl
--       , msgAnn :: !(Maybe ann) }

data CGT
  = CChoice Role Role (Alt CGT)
  | CComm Role Role VSize VCost CGT
  | CGRec String Integer CGT
  | CGVar String
  | CGEnd
  deriving Show

instance Pretty CGT where
  pretty (CChoice src dest b) = Pretty.align
                                $! Pretty.vsep
                                $! [ Pretty.hsep [ pretty src
                                                 , pretty "->"
                                                 , pretty dest
                                                 ]
                                   , pretty b
                                  ]
  pretty c@CComm{} = Pretty.align $!
                     Pretty.vsep $!
                     Pretty.punctuate (pretty ".") $!
                     go c
    where
      go (CComm f t ty cc b) = msg : go b
        where
          msg = Pretty.hsep
                [ pretty f
                , pretty "->"
                , pretty t
                , pretty ":"
                , Pretty.parens $ Pretty.hsep [ pretty ty
                                              , pretty "<>"
                                              , pretty cc
                                              ]
                ]
      go g          = [pretty g]
  pretty (CGRec v k x) = Pretty.hsep [ pretty "rec[" <> pretty k <> pretty "]"
                                  , pretty v Pretty.<> pretty "."
                                  , pretty x
                                  ]
  pretty (CGVar v) = pretty v
  pretty CGEnd = pretty "end"

showCGT :: CGT -> String
showCGT = show . pretty

printCGT :: CGT -> IO ()
printCGT = print . pretty

data GTSt = GTSt
  { nextRole :: Role
  , nextLabel :: Label
  , nextVar :: [String]
  , globalType :: CGT -> CGT
  }

type GTM a = State GTSt a

-- message r r' typ
message :: Role -> Role -> VSize -> VCost -> GTM ()
message r1 r2 ty ann = do
  s <- get
  put s { globalType = globalType s . CComm r1 r2 ty ann }

newtype LAlt = LAlt (Label, GTM ())

infixr 4 ...

(...) :: Label -> GTM () -> LAlt
l ... g = LAlt (l, g)

newtype GAlt = GAlt [LAlt]
  deriving (Semigroup, Monoid)

(.|) :: LAlt -> GAlt -> GAlt
l .| GAlt ls = GAlt $ l : ls

choice :: Role -> Role -> GAlt -> GTM ()
choice r1 rs (GAlt gs) = do
  s <- get
  put s { globalType = id }
  as <- runAll gs
  put s { globalType = globalType s . CChoice r1 rs . as }
  where
    runAll [] = pure $! \_ -> emptyAlt
    runAll (LAlt (l, g1) : gk) = do
      gh <- getG g1
      gt <- runAll gk
      pure $ \x -> addLAlt l (gh x) (gt x)
    getG :: GTM () -> GTM (CGT -> CGT)
    getG g = do
      g
      s <- get
      put s { globalType = id }
      pure $! globalType s

newVar :: GTM String
newVar = do
  s <- get
  case nextVar s of
    [] -> error "empty variable generator"
    v : vs -> do
      put s { nextVar = vs }
      pure v

grec :: Integer -> (GTM () -> GTM ()) -> GTM ()
grec k f = do
  v <- newVar
  modify $ \s -> s { globalType = globalType s . CGRec v k }
  f $ mkVar v
  where
    mkVar :: String -> GTM ()
    mkVar v = do
      s <- get
      put s { globalType = \_ -> globalType s (CGVar v) }

class Var a where
  varGen :: [a]

instance Var Integer where
  varGen = [0..]

instance Var String where
  varGen = vg1 ++ [ v ++ show i | i <- [1::Integer ..], v <- vg1 ]
    where
      vg1 = map (:[]) ['a'..'z']

gclose :: GTM () -> CGT
gclose g = globalType (execState g initSt) CGEnd

initSt :: GTSt
initSt = GTSt { nextRole = Rol 0
              , nextLabel = Lbl 0
              , nextVar = varGen
              , globalType = id
              }

mkRole :: GTM Role
mkRole = do
  s <- get
  put s { nextRole = inc $! nextRole s }
  pure $! nextRole s
  where
    inc (Rol i) = Rol $! i+1

mkLabel :: GTM Label
mkLabel = do
  s <- get
  put s { nextLabel = inc $! nextLabel s }
  pure $! nextLabel s
  where
    inc (Lbl i) = Lbl $! i+1

roles :: CGT -> Set Role
roles CGEnd = Set.empty
roles CGVar{} = Set.empty
roles (CGRec _ _ g) = roles g
roles (CComm f t _ _ g) = Set.unions [ Set.fromList $ [f, t]
                                     , roles g
                                     ]
roles (CChoice r rs Alt { altMap = m } )
  = Set.unions [ Set.fromList $ [r, rs]
               , Set.unions $ map (roles . snd) $ Map.toList m
               ]

type VTime i = Map Role (Cost i)
type Time = VTime String

newtype MTime a = MTime { unMTime :: State Time a }
  deriving (Functor, Applicative, Monad, MonadState Time)

maxTime :: Time -> Time -> Time
maxTime = Map.unionWith CMax

instance Semigroup (MTime ()) where
  c1 <> c2 = do
    s <- get
    s1 <- c1 *> get <* put s
    s2 <- c2 *> get
    put $! maxTime s1 s2

instance Monoid (MTime ()) where
  mempty = pure ()

subst :: String -> CGT -> CGT -> CGT
subst v g (CChoice r1 rs alts) = CChoice r1 rs $! mapAlt (\_ -> subst v g) alts
subst v g (CComm f t ty c k) = CComm f t ty c $! subst v g k
subst v g gr@(CGRec v' i g')
  | v == v' = gr
  | otherwise = CGRec v' i $! subst v g g'
subst v g gv@(CGVar v')
  | v == v' = g
  | otherwise = gv
subst _ _ CGEnd = CGEnd

unrollG :: Integer -> String -> CGT -> CGT
unrollG i v g
  | i <= 0 = CGEnd
  | otherwise = subst v (unrollG (i-1) v g) g

bCost :: CGT -> MTime ()
bCost CGEnd = pure ()
bCost CGVar{} = pure ()
bCost (CGRec v i g) = bCost (unrollG i v g)
bCost (CComm f t ty c k) = do
  sendCost t ty f
  recvCost f ty (Just c) t
  bCost k
bCost (CChoice r1 rs alts) = do
  sendCost rs (K 1) r1
  recvCost r1 (K 1) Nothing rs
  foldAlt (<>) mempty (mapAlt (\_ -> bCost) alts)

sendCost :: Role -> VSize -> Role -> MTime ()
sendCost rs sz f = modify $ Map.alter (Just . step) f
  where
    step Nothing = CSend f rs sz
    step (Just c) = CAdd (CSend f rs sz) c

recvCost :: Role -> VSize -> Maybe VCost -> Role -> MTime ()
recvCost rs sz cc f = modify $ \s ->
  case Map.lookup rs s of
    Nothing -> Map.alter (Just . step Nothing) f s
    Just c' -> Map.alter (Just . step (Just c')) f s
  where
    addmC c1 (Just c2) = CAdd c1 c2
    addmC c1 Nothing = c1
    maxmC (Just c1) (Just c2) = Just (CMax c1 c2)
    maxmC c1 Nothing = c1
    maxmC Nothing c2 = c2
    step c1 c2 = CRecv f rs sz `addmC` cc `addmC` maxmC c1 c2

addCost :: Time -> Time -> Time
addCost = Map.unionWith CAdd

mulCost :: Integer -> Time -> Time
mulCost i t = Map.map (CMul (K i)) t

delta :: Time -> Time
delta t = Map.map (\_ -> CDelta t) t

sCost :: CGT -> MTime ()
sCost CGEnd = pure ()
sCost CGVar{} = pure ()
sCost (CGRec _ i g) = do
  s <- get
  rCost g
  k <- get
  put $! addCost s (mulCost i $ delta k)
sCost (CComm f t ty c k) = do
  sendCost t ty f
  recvCost f ty (Just c) t
  sCost k
sCost (CChoice r1 rs alts) = do
  sendCost rs (K 1) r1
  recvCost r1 (K 1) Nothing rs
  foldAlt (<>) mempty (mapAlt (\_ -> bCost) alts)

-- Cost of costw (mu X. G)
rCost :: CGT -> MTime ()
rCost g = put ks >> bCost g
  where
    ks = Map.fromList $! map (\r -> (r, CRec r)) rs
    rs = Set.toList $ roles g

throughput :: CGT -> Time
throughput (CGRec _ _ g) = execState (unMTime $ rCost g) Map.empty
throughput _  = error "Cannot compute throughput of non-recursive protocols"

cost :: CGT -> Time
cost g = execState (unMTime $ sCost g) Map.empty

showEqns :: Time -> String
showEqns t = show $ Pretty.vsep $ map time $ Map.toList t
  where
    time (r, c) = pretty r Pretty.<+> pretty "=" Pretty.<+> pretty c

printEqns :: Time -> IO ()
printEqns = putStrLn . showEqns

evalTime :: Map (Role, Role) (Double -> Double)
         -> Map String Double
         -> Time
         -> Map Role Double
evalTime distm vars = Map.map evalC
  where
    evalC :: VCost -> Double
    evalC (CSize s    ) = evalS s
    evalC (CVar v     ) = maybe 0 id $! Map.lookup v vars
    evalC (CRec _     ) = 0 -- XXX: Fixme
    evalC (CAdd l r   ) = evalC l + evalC r
    evalC (CMax l r   ) = max (evalC l) (evalC r)
    evalC (CMul l r   ) = evalS l * evalC r
    evalC (CSend f t s) = maybe 0 ($ evalS s) (Map.lookup (f, t) distm)
    evalC (CRecv f t s) = maybe 0 ($ evalS s) (Map.lookup (f, t) distm)
    evalC (CDelta _   ) = 0 -- XXX: Fixme

    evalS :: VSize -> Double
    evalS (Var v   ) = maybe 0 id $! Map.lookup v vars
    evalS (K k     ) = fromInteger k
    evalS (SAdd l r) = evalS l + evalS r
    evalS (SSub l r) = evalS l - evalS r
    evalS (SMul l r) = evalS l * evalS r
    evalS (SDiv l r) = evalS l / evalS r

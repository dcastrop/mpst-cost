{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.SessionTypes.Cost
  ( CGT
  , GTM
  , showCGT
  , printCGT
  , latexCGT
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
  , send
  , recv
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
  , evalDelta
  , unroll
  , latexEqns
  ) where

import Control.Monad.State.Strict
import Data.Maybe ( fromMaybe )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Text.Prettyprint.Doc ( Doc, Pretty, pretty )
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

class LaTeX t where
  latex :: t -> Doc ann

instance LaTeX String where
  latex = pretty

instance LaTeX i => LaTeX (Size i) where
  latex (Var i) = latex i
  latex (K i) = pretty i
  latex (SAdd s1 s2) = go [s2, s1] []
    where
      go (SAdd sl sr : ss) acc = go (sr : sl : ss) acc
      go (s : ss) acc = go ss (s : acc)
      go [] acc = Pretty.hsep $! Pretty.punctuate (pretty "+") $! map latex acc
  latex (SMul s1 s2) = go [s2, s1] []
    where
      go (SMul sl sr : ss) acc = go (sr : sl : ss) acc
      go (s : ss) acc = go ss (s : acc)
      go [] acc = Pretty.hsep $! Pretty.punctuate (pretty "+") $! map latexP acc
      latexP t@SAdd{} = Pretty.parens (latex t)
      latexP t@SSub{} = Pretty.parens (latex t)
      latexP t = latex t
  latex (SSub s1 s2) = Pretty.hsep [latex s1, pretty "-", latexP s2]
    where
      latexP t@SAdd{} = Pretty.parens (latex t)
      latexP t@SSub{} = Pretty.parens (latex t)
      latexP t = latex t
  latex (SDiv l r) = Pretty.hsep [ prettyP1 l, pretty "/", prettyP2 r]
    where
      prettyP1 SAdd{} = Pretty.parens $ latex r
      prettyP1 SSub{} = Pretty.parens $ latex r
      prettyP1 _ =  latex r
      prettyP2 SAdd{} = Pretty.parens $ latex r
      prettyP2 SSub{} = Pretty.parens $ latex r
      prettyP2 SMul{} = Pretty.parens $ latex r
      prettyP2 SDiv{} = Pretty.parens $ latex r
      prettyP2 _ =  latex r

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
  | CDelta Role Time
  deriving Show

instance LaTeX i => LaTeX (Cost i) where
  latex (CSize i) = latex i
  latex (CRec r)  = pretty "T_" <> latex r
  latex (CVar i) = latex i
  latex (CAdd l r) = Pretty.hsep [ latex l, pretty "+", latex r]
  latex (CMul l r) = Pretty.hsep [ prettyPs l, pretty "*", prettyPc r]
    where
      prettyPs SAdd{} = Pretty.parens $ latex l
      prettyPs SSub{} = Pretty.parens $ latex l
      prettyPs _ = latex l
      prettyPc CAdd{} = Pretty.parens $ latex r
      prettyPc _ =  latex r
  latex (CMax l r) = go [l, r] []
    where
      go (CMax hl hr : t) mm = go (hl : hr : t) mm
      go (h : t) [] = go t [latex h]
      go (h : t) mm = go t (latex h <> pretty "," : mm)
      go [] mm = Pretty.hsep [ pretty "\\max", Pretty.parens $ Pretty.hsep mm]
  latex (CSend _ _ sz) = pretty "\\csend" <> Pretty.parens (latex sz)
  latex (CRecv _ _ sz) = pretty "\\crecv" <> Pretty.parens (latex sz)
  latex (CDelta r t) = pretty "\\Delta_" <> pretty r
                       <> pretty "\\left(\\begin{array}{@{}l@{}}"
                       <> Pretty.vsep (map tme $ Map.toList t)
                       <> pretty "\\end{array}\\right)"
    where
      tme (Rol rr, c) = pretty "T_" <> pretty rr
                         Pretty.<+> pretty "="
                         Pretty.<+> latex c
                         <> pretty ";"

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
  pretty (CDelta r t) =
    pretty "\\Delta_" <> pretty r <> Pretty.parens (Pretty.hsep $ map tme $ Map.toList t)
    where
      tme (rr, c) = pretty rr Pretty.<+> pretty "=" Pretty.<+> pretty c <> pretty ";"

showCost :: Pretty i => Cost i -> String
showCost = show . pretty

type VCost = Cost String

data CGT
  = CChoice Role Role (Alt CGT)
  | CComm Role Role VSize VCost CGT
  | CGSend Role Role VSize CGT
  | CGRecv Role Role VSize VCost CGT
  | CGRec String Integer CGT
  | CGVar String
  | CGEnd
  deriving Show

roleList :: [String]
roleList = bl ++ [ b ++ "_" ++ show i | i <- [(1::Integer)..], b <- bl ]
  where
    bl = ["p", "q", "r", "s"]

instance LaTeX Role where
  latex (Rol r) = pretty "\\role{" <> pretty (roleList !! r) <> pretty "}"

instance LaTeX Label where
  latex (Lbl l) = pretty "l_" <> pretty l

instance LaTeX (Alt CGT) where
  latex (Alt m)
    = Pretty.braces $
      Pretty.hsep $
      Pretty.punctuate (pretty ";") $
      map lalt $
      Map.toList m
    where
      lalt (l, g) = latex l <> pretty "." Pretty.<+> latex g

instance LaTeX CGT where
  latex (CChoice src dest b) = Pretty.align
                               $! Pretty.vsep [ Pretty.hsep [ latex src
                                                , pretty "\\gMsg"
                                                , latex dest
                                                 ]
                                   , latex b
                                  ]
  latex (CGRec v _ x) = Pretty.hsep [ pretty "\\gFix"
                                    , pretty v <> pretty "."
                                    , latex x
                                    ]
  latex (CGVar v) = pretty v
  latex CGEnd = pretty "\\gEnd"
  latex c = Pretty.align $!
                    Pretty.vsep $!
                    Pretty.punctuate (pretty ".") $!
                    go c
    where
      go (CComm f t ty cc b) = msg : go b
        where
          msg = Pretty.hsep
                [ latex f
                , pretty "\\gMsg"
                , latex t
                , pretty "\\gTy{"
                , latex ty
                , pretty "\\hasCost"
                , latex cc
                , pretty "}"
                ]
      go (CGSend f t ty b) = msg : go b
        where
          msg = Pretty.hsep
                [ latex f
                , pretty "\\gSend"
                , latex t
                , pretty "\\gTy{"
                , latex ty
                , pretty "}"
                ]
      go (CGRecv f t ty cc b) = msg : go b
        where
          msg = Pretty.hsep
                [ latex t
                , pretty "\\gRecv"
                , latex f
                , pretty "\\gTy{"
                , latex ty
                , pretty "\\hasCost"
                , latex cc
                , pretty "}"
                ]
      go g          = [latex g]

instance Pretty CGT where
  pretty (CChoice src dest b) = Pretty.align
                                $! Pretty.vsep [ Pretty.hsep [ pretty src
                                                 , pretty "->"
                                                 , pretty dest
                                                 ]
                                   , pretty b
                                  ]
  pretty (CGRec v k x) = Pretty.hsep [ pretty "rec[" <> pretty k <> pretty "]"
                                  , pretty v Pretty.<> pretty "."
                                  , pretty x
                                  ]
  pretty (CGVar v) = pretty v
  pretty CGEnd = pretty "end"
  pretty c = Pretty.align $!
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
      go (CGSend f t ty b) = msg : go b
        where
          msg = Pretty.hsep
                [ pretty f
                , pretty "~>"
                , pretty t
                , pretty ":"
                , Pretty.parens $ pretty ty
                ]
      go (CGRecv f t ty cc b) = msg : go b
        where
          msg = Pretty.hsep
                [ pretty t
                , pretty "<~"
                , pretty f
                , pretty ": ("
                , pretty ty
                , pretty "<>"
                , pretty cc
                , pretty ")"
                ]
      go g          = [pretty g]

showCGT :: CGT -> String
showCGT = show . pretty

printCGT :: CGT -> IO ()
printCGT = print . pretty

latexCGT :: CGT -> IO ()
latexCGT = print . latex

data GTSt = GTSt
  { nextRole :: Role
  , nextLabel :: Label
  , nextVar :: [RVar]
  , globalType :: CGT -> CGT
  }

type GTM a = State GTSt a

-- message r r' typ
message :: Role -> Role -> VSize -> VCost -> GTM ()
message r1 r2 ty ann = do
  s <- get
  put s { globalType = globalType s . CComm r1 r2 ty ann }

send :: Role -> Role -> VSize -> GTM ()
send r1 r2 ty = do
  s <- get
  put s { globalType = globalType s . CGSend r1 r2 ty }

recv :: Role -> Role -> VSize -> VCost -> GTM ()
recv r1 r2 ty acc = do
  s <- get
  put s { globalType = globalType s . CGRecv r1 r2 ty acc }

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
    runAll [] = pure $! const emptyAlt
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

newRVar :: GTM String
newRVar = do
  s <- get
  case nextVar s of
    [] -> error "empty variable generator"
    RVar v : vs -> do
      put s { nextVar = vs }
      pure v

newtype RVar = RVar String

instance Var RVar where
  varGen = map RVar vg1 ++
           [ RVar $ v ++ show i | i <- [1::Integer ..], v <- vg1 ]
    where
      vg1 = map (:[]) ['X'..'Z']


grec :: Integer -> (GTM () -> GTM ()) -> GTM ()
grec k f = do
  v <- newRVar
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
roles (CComm f t _ _ g) = Set.unions [ Set.fromList [f, t]
                                     , roles g
                                     ]
roles (CGSend f t _ g) = Set.unions [ Set.fromList [f, t]
                                     , roles g
                                     ]
roles (CGRecv f t _ _ g) = Set.unions [ Set.fromList [f, t]
                                     , roles g
                                     ]
roles (CChoice r rs Alt { altMap = m } )
  = Set.unions [ Set.fromList [r, rs]
               , Set.unions $ map (roles . snd) $ Map.toList m
               ]

type VTime i = Map Role (Cost i)
type VMsgQ i = Map (Role, Role) [Cost i]
type Time = VTime String
type MsgQ = VMsgQ String

data TSt = TSt { time :: Time, msgQ :: MsgQ }

emptyTSt :: TSt
emptyTSt = TSt { time = Map.empty, msgQ = Map.empty }

newtype MTime a = MTime { unMTime :: State TSt a }
  deriving (Functor, Applicative, Monad, MonadState TSt)

maxTime :: Time -> Time -> Time
maxTime = Map.unionWith CMax

appMsgQ :: MsgQ -> MsgQ -> MsgQ
appMsgQ = Map.unionWith maxQ
  where
    maxQ = zipWith CMax

instance Semigroup (MTime ()) where
  c1 <> c2 = do
    s <- get
    s1 <- c1 *> get <* put s
    s2 <- c2 *> get
    put $! TSt { time = maxTime (time s1) (time s2)
               , msgQ = appMsgQ (msgQ s1) (msgQ s2)
               }

instance Monoid (MTime ()) where
  mempty = pure ()

subst :: String -> CGT -> CGT -> CGT
subst v g (CChoice r1 rs alts) = CChoice r1 rs $! mapAlt (\_ -> subst v g) alts
subst v g (CComm f t ty c k) = CComm f t ty c $! subst v g k
subst v g (CGSend f t ty k) = CGSend f t ty $! subst v g k
subst v g (CGRecv f t ty c k) = CGRecv f t ty c $! subst v g k
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
bCost (CGSend f t ty k) = do
  asendCost t ty f
  bCost k
bCost (CGRecv f t ty c k) = do
  arecvCost f ty (Just c) t
  bCost k
bCost (CChoice r1 rs alts) = do
  sendCost rs (K 1) r1
  recvCost r1 (K 1) Nothing rs
  foldAlt (<>) mempty (mapAlt (const bCost) alts)

alterTime :: (Maybe (Cost String) -> Cost String) -> Role -> TSt -> TSt
alterTime f r s = s { time = Map.alter (Just . f) r $ time s }

sendCost :: Role -> VSize -> Role -> MTime ()
sendCost rs sz f = modify $ alterTime step f
  where
    step Nothing = CSend f rs sz
    step (Just c) = CAdd (CSend f rs sz) c

enqueue :: Maybe [Cost String] -> Cost String -> Maybe [Cost String]
enqueue Nothing c = Just [c]
enqueue (Just q) c = Just $ q ++ [c]

dequeue :: (Role, Role) -> MsgQ -> (Maybe VCost, MsgQ)
dequeue k m
  | Just (h:t) <- Map.lookup k m = (Just h, Map.insert k t m)
  | otherwise = (Nothing, m)

-- FIXME
asendCost :: Role -> VSize -> Role -> MTime ()
asendCost rs sz f = do
  s <- get
  let tf = step (Map.lookup f $ time s)
  put s { time = Map.insert f tf $ time s
        , msgQ = Map.alter (`enqueue` tf) (f, rs) $ msgQ s
        }
  where
    step Nothing = CSend f rs sz
    step (Just c) = CAdd (CSend f rs sz) c

arecvCost :: Role -> VSize -> Maybe VCost -> Role -> MTime ()
arecvCost rs sz cc f = modify $ \s ->
  let (td, dq) = dequeue (rs, f) $ msgQ s
  in s { time = Map.alter (Just . step td) f $ time s
       , msgQ = dq
       }
  where
    step c1 c2 = CRecv f rs sz `addmC` cc `addmC` maxmC c1 c2

recvCost :: Role -> VSize -> Maybe VCost -> Role -> MTime ()
recvCost rs sz cc f = modify $ \s ->
  case Map.lookup rs $ time s of
    Nothing -> alterTime (step Nothing)   f s
    Just c' -> alterTime (step $ Just c') f s
  where
    step c1 c2 = CRecv f rs sz `addmC` cc `addmC` maxmC c1 c2

addmC :: Cost i -> Maybe (Cost i) -> Cost i
addmC c1 (Just c2) = CAdd c1 c2
addmC c1 Nothing = c1

maxmC :: Maybe (Cost i) -> Maybe (Cost i) -> Maybe (Cost i)
maxmC (Just c1) (Just c2) = Just (CMax c1 c2)
maxmC c1 Nothing = c1
maxmC Nothing c2 = c2

addCost :: Time -> Time -> Time
addCost = Map.unionWith CAdd

mulCost :: Integer -> Time -> Time
mulCost i = Map.map (CMul (K i))

delta :: Time -> Time
delta t = Map.mapWithKey (\r _ -> CDelta r t) t

sCost :: CGT -> MTime ()
sCost CGEnd = pure ()
sCost CGVar{} = pure ()
sCost (CGRec _ i g) = do
  s <- get
  rCost g
  k <- get
  put $! s { time = addCost (time s) (mulCost i $ delta $ time k)
           }
sCost (CComm f t ty c k) = do
  sendCost t ty f
  recvCost f ty (Just c) t
  sCost k
sCost (CGSend f t ty k) = do
  asendCost t ty f
  sCost k
sCost (CGRecv f t ty c k) = do
  arecvCost f ty (Just c) t
  sCost k
sCost (CChoice r1 rs alts) = do
  sendCost rs (K 1) r1
  recvCost r1 (K 1) Nothing rs
  foldAlt (<>) mempty (mapAlt (const bCost) alts)

-- Cost of costw (mu X. G)
rCost :: CGT -> MTime ()
rCost g = modify ks >> bCost g
  where
    ks s = TSt { time = Map.fromList $! map (\r -> (r, CRec r)) rs
               , msgQ = msgQ s
               }
    rs = Set.toList $ roles g

throughput :: CGT -> Time
throughput (CGRec _ _ g) = time $ execState (unMTime $ rCost g) emptyTSt
throughput _  = error "Cannot compute throughput of non-recursive protocols"

cost :: CGT -> Time
cost g = time $ execState (unMTime $ sCost g) emptyTSt

showEqns :: Time -> String
showEqns t = show $ Pretty.vsep $ map tme $ Map.toList t
  where
    tme (r, c) = pretty r Pretty.<+> pretty "=" Pretty.<+> pretty c

printEqns :: Time -> IO ()
printEqns = putStrLn . showEqns

latexEqns :: Time -> IO ()
latexEqns t = print (Pretty.vsep $ map tme $ Map.toList t)
  where
    tme (r, c) = pretty "T_" <> latex r
      Pretty.<+> pretty "=" Pretty.<+> latex c

evalTime :: Map (Role, Role) (Double -> Double)
         -> Map (Role, Role) (Double -> Double)
         -> Map String Double
         -> Time
         -> Map Role Double
evalTime csend crecv vars = Map.map evalC
  where
    evalC :: VCost -> Double
    evalC (CSize s    ) = evalS s
    evalC (CVar v     ) = fromMaybe 0 $! Map.lookup v vars
    evalC (CRec _     ) = 0 -- XXX: Fixme
    evalC (CAdd l r   ) = evalC l + evalC r
    evalC (CMax l r   ) = max (evalC l) (evalC r)
    evalC (CMul l r   ) = evalS l * evalC r
    evalC (CSend f t s) = maybe 0 ($ evalS s) (Map.lookup (f, t) csend)
    evalC (CRecv f t s) = maybe 0 ($ evalS s) (Map.lookup (f, t) crecv)
    evalC (CDelta r d ) = rd 2 - rd 1
      where
        rd i = maybe 0 evalC $! Map.lookup r (unroll i d)

    evalS :: VSize -> Double
    evalS (Var v   ) = fromMaybe 0 $! Map.lookup v vars
    evalS (K k     ) = fromInteger k
    evalS (SAdd l r) = evalS l + evalS r
    evalS (SSub l r) = evalS l - evalS r
    evalS (SMul l r) = evalS l * evalS r
    evalS (SDiv l r) = evalS l / evalS r

unrollTime :: Time -> Time -> Time
unrollTime to = Map.map doUnroll
  where
    doUnroll c@CSize{} = c
    doUnroll c@CVar{}  = c
    doUnroll c@CSend{} = c
    doUnroll c@CRecv{} = c
    doUnroll c@CDelta{} = c
    doUnroll c@(CRec r) = fromMaybe c $! Map.lookup r to
    doUnroll (CAdd l r) = CAdd (doUnroll l) (doUnroll r)
    doUnroll (CMax l r) = CMax (doUnroll l) (doUnroll r)
    doUnroll (CMul l r) = CMul l (doUnroll r)

unroll :: Integer -> Time -> Time
unroll i t
  | i <= 0 = Map.map (\_ -> CSize $ K 0) t
  | otherwise = go t $ i - 1
  where
    go tt j | j == 0 = Map.map rzero tt
            | otherwise = go (unrollTime t tt) (j-1)
    rzero CRec{} = CSize (K 0)
    rzero c@CSize{} = c
    rzero c@CVar{}  = c
    rzero c@CSend{} = c
    rzero c@CRecv{} = c
    rzero c@CDelta{} = c
    rzero (CAdd l r) = CAdd (rzero l) (rzero r)
    rzero (CMax l r) = CMax (rzero l) (rzero r)
    rzero (CMul l r) = CMul l (rzero r)

evalDelta :: Map (Role, Role) (Double -> Double)
          -> Map (Role, Role) (Double -> Double)
          -> Map String Double
          -> Time
          -> Map Role Double
evalDelta csend crecv vars = Map.map evalC
  where
    evalC :: VCost -> Double
    evalC (CSize s    ) = evalS s
    evalC (CVar v     ) = fromMaybe 0 $! Map.lookup v vars
    evalC (CRec _     ) = 0 -- XXX: Fixme
    evalC (CAdd l r   ) = evalC l + evalC r
    evalC (CMax l r   ) = max (evalC l) (evalC r)
    evalC (CMul l r   ) = evalS l * evalC r
    evalC (CSend f t s) = maybe 0 ($ evalS s) (Map.lookup (f, t) csend)
    evalC (CRecv f t s) = maybe 0 ($ evalS s) (Map.lookup (f, t) crecv)
    evalC (CDelta r d ) = rd 2 - rd 1
      where
        rd i = maybe 0 evalC $! Map.lookup r (unroll i d)

    evalS :: VSize -> Double
    evalS (Var v   ) = fromMaybe 0 $! Map.lookup v vars
    evalS (K k     ) = fromInteger k
    evalS (SAdd l r) = evalS l + evalS r
    evalS (SSub l r) = evalS l - evalS r
    evalS (SMul l r) = evalS l * evalS r
    evalS (SDiv l r) = evalS l / evalS r

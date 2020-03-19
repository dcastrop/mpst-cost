{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.SessionTypes.Global
  ( GT (..)
  , getRoles
  , seqComm
  , mSeqComm
  , mChoice
  , mComm
  , mGRec
  , mGVar
  , mGEnd
  , GBranch
  , Msg (..)
  , (...)
  , GTM
  , GTSt
  , Var(..)
  , mkRole
  , mkLabel
  , message
  , (.|)
  , choice
  , grec
  , gend
  ) where

-- import Control.Monad ( liftM2, liftM3 )
import Control.Monad.State.Strict
import qualified Data.List as List
import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc ( Pretty, pretty )
import qualified Data.Text.Prettyprint.Doc as Pretty

import Language.SessionTypes.Common

-- | Annotated messages
data Msg pl ann =
  Msg { rfrom :: ![Role]
      , rto :: ![Role]
      , rty :: pl
      , msgAnn :: !(Maybe ann) }
  deriving Show

instance (Pretty pl, Pretty ann) => Pretty (Msg pl ann) where
  pretty (Msg from to ty Nothing) =
    Pretty.hsep
    [ pretty $ RS from
    , pretty "->"
    , pretty $ RS to
    , pretty ":"
    , pretty ty ]
  pretty (Msg from to ty (Just ann)) =
    Pretty.hsep
    [ pretty $ RS from
    , pretty "->"
    , pretty $ RS to
    , Pretty.braces $ pretty ann
    , pretty ":"
    , pretty ty ]

-- | Global types
data GT v pl ann = Choice Role [Role] (GBranch v pl ann)
              | Comm (Msg pl ann) (GT v pl ann)
              | GRec v (GT v pl ann)
              | GVar v
              | GEnd
  deriving Show

data GTSt v pl ann = GTSt
  { nextRole :: Role
  , nextLabel :: Label
  , nextVar :: [v]
  , globalType :: GT v pl ann -> GT v pl ann
  }

type GTM v pl ann a = State (GTSt v pl ann) a

-- message r r' typ
message :: Role -> Role -> pl -> ann -> GTM v pl ann ()
message r1 r2 ty ann = do
  s <- get
  put s { globalType = globalType s . Comm (Msg [r1] [r2] ty (Just ann)) }

newtype LAlt v pl ann = LAlt (Label, GTM v pl ann ())

infixr 4 ...

(...) :: Label -> GTM v pl ann () -> LAlt v pl ann
l ... g = LAlt (l, g)

newtype GAlt v pl ann = GAlt [LAlt v pl ann]
  deriving (Semigroup, Monoid)

(.|) :: LAlt v pl ann -> GAlt v pl ann -> GAlt v pl ann
l .| GAlt ls = GAlt $ l : ls

choice :: Role -> [Role] -> GAlt v pl ann -> GTM v pl ann ()
choice r1 rs (GAlt gs) = do
  s <- get
  put s { globalType = id }
  as <- runAll gs
  put s { globalType = globalType s . Choice r1 rs . as }
  where
    runAll [] = pure $! const emptyAlt
    runAll (LAlt (l, g1) : gk) = do
      gh <- getG g1
      gt <- runAll gk
      pure $ \x -> addLAlt l (gh x) (gt x)
    getG :: GTM v pl ann () -> GTM v pl ann (GT v pl ann -> GT v pl ann)
    getG g = do
      g
      s <- get
      put s { globalType = id }
      pure $! globalType s

newVar :: GTM v pl ann v
newVar = do
  s <- get
  case nextVar s of
    [] -> error "empty variable generator"
    v : vs -> do
      put s { nextVar = vs }
      pure v

grec :: (GTM v pl ann () -> GTM v pl ann ()) -> GTM v pl ann ()
grec f = (mkVar <$> newVar) >>= f
  where
    mkVar :: v -> GTM v pl ann ()
    mkVar v = do
      s <- get
      put s { globalType = \_ -> globalType s (GVar v) }

class Var a where
  varGen :: [a]

instance Var Integer where
  varGen = [0..]

instance Var String where
  varGen = vg1 ++ [ v ++ show i | i <- [1::Integer ..], v <- vg1 ]
    where
      vg1 = map (:[]) ['a'..'z']

gend :: Var v => GTM v pl ann a -> GT v pl ann
gend g = globalType (execState g initSt) GEnd

initSt :: Var v => GTSt v pl ann
initSt = GTSt { nextRole = Rol 0
              , nextLabel = Lbl 0
              , nextVar = varGen
              , globalType = id
              }

mkRole :: GTM v pl ann Role
mkRole = do
  s <- get
  put s { nextRole = inc $! nextRole s }
  pure $! nextRole s
  where
    inc (Rol i) = Rol $! i+1

mkLabel :: GTM v pl ann Label
mkLabel = do
  s <- get
  put s { nextLabel = inc $! nextLabel s }
  pure $! nextLabel s
  where
    inc (Lbl i) = Lbl $! i+1

-- choice :: Role -> [Role] -> [GTM ()] -> GTM ()

seqComm :: [Msg t a] -> GT v t a -> GT v t a
seqComm msg = go (reverse msg)
  where
    go = flip (List.foldl' (flip Comm))

mSeqComm :: Monad m => m [Msg t a] -> m (GT v t a) -> m (GT v t a)
mSeqComm = liftM2 seqComm

mChoice :: Monad m
        => m Role
        -> m [Role]
        -> m (GBranch v pl ann)
        -> m (GT v pl ann)
mChoice = liftM3 Choice

mComm :: Monad m
      => m (Msg pl ann)
      -> m (GT v pl ann)
      -> m (GT v pl ann)
mComm = liftM2 Comm

mGRec :: Monad m
      => m v
      -> m (GT v pl ann)
      -> m (GT v pl ann)
mGRec = liftM2 GRec

mGVar :: Monad m
      => m v
      -> m (GT v pl ann)
mGVar = fmap GVar

mGEnd :: Monad m
      => m (GT v pl ann)
mGEnd = return GEnd

-- (...) :: Msg pl ann -> GT v pl ann -> GT v pl ann
-- (...) = Comm

type GBranch v pl ann = Alt (GT v pl ann)

getRoles :: GT v pl ann -> Set Role
getRoles GEnd = Set.empty
-- getRoles (GSeq gs) = Set.unions $ map getRoles gs
getRoles GVar{} = Set.empty
getRoles (GRec _ g) = getRoles g
getRoles (Comm m g) = Set.unions [ Set.fromList $ rfrom m
                                 , Set.fromList $ rto m
                                 , getRoles g
                                 ]
getRoles (Choice r rs Alt { altMap = m } )
  = Set.unions [ Set.fromList $ r : rs
               , Set.unions $ map (getRoles . snd) $ Map.toList m
               ]

instance (Pretty v, Pretty ann, Pretty pl) => Pretty (GT v pl ann) where
  -- pretty (GSeq gs) = Pretty.align $!
  --                    Pretty.vsep $!
  --                    Pretty.punctuate (pretty ";") $!
  --                    map pretty gs
  pretty (Choice src dest b) = Pretty.align
                               $! Pretty.vsep [ Pretty.hsep [ pretty src
                                                , pretty "->"
                                                , pretty $ RS dest
                                                ]
                                  , pretty b
                                  ]
  pretty c@Comm{} = Pretty.align $!
                    Pretty.vsep $!
                    Pretty.punctuate (pretty ".") $!
                    go c
    where
      go (Comm i b) = pretty i : go b
      go g          = [pretty g]
  pretty (GRec v x) = Pretty.hsep [ pretty "rec"
                                  , pretty v Pretty.<> pretty "."
                                  , pretty x
                                  ]
  pretty (GVar v) = pretty v
  pretty GEnd = pretty "end"

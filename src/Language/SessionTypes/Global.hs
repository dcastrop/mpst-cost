{-# LANGUAGE QuasiQuotes #-}
module Language.SessionTypes.Global
  ( GT (..)
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
  ) where

import Control.Monad ( liftM, liftM2, liftM3 )
import qualified Data.List as List
import Data.Text.Prettyprint.Doc ( Pretty, pretty )
import Data.Text.Prettyprint.EDoc

import Language.SessionTypes.Common

-- | Annotated messages
data Msg pl ann =
  Msg { rfrom :: [Role]
      , rto :: [Role]
      , rty :: pl
      , msgAnn :: ann }

instance (Pretty pl, Pretty ann) => Pretty (Msg pl ann) where
  pretty (Msg from to ty ann) =
      [ppr| RS from > "->" > RS to > "{" > ann > "} :" > ty |]

-- | Global types
data GT v pl ann = Choice Role [Role] (GBranch v pl ann)
              | Comm (Msg pl ann) (GT v pl ann)
              | GRec v (GT v pl ann)
              | GVar v
              | GEnd

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
mGVar = liftM GVar

mGEnd :: Monad m
      => m (GT v pl ann)
mGEnd = return GEnd

infixr 4 ...

(...) :: Msg pl ann -> GT v pl ann -> GT v pl ann
(...) = Comm

type GBranch v pl ann = Alt ann (GT v pl ann)

instance (Pretty v, Pretty ann, Pretty pl) => Pretty (GT v pl ann) where
  pretty (Choice src dest b) = [ppr| src > "->" > RS dest > ":" > b |]
  pretty (Comm i b) = [ppr| i + "." > b |]
  pretty (GRec v x) = [ppr| "rec" > v + "." > x |]
  pretty (GVar v) = [ppr| v |]
  pretty GEnd = [ppr| "end" |]

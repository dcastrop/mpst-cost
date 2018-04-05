{-# LANGUAGE QuasiQuotes #-}
module Language.SessionTypes.Prefix.Global
  ( GT (..)
  , Op (..)
  , gPar
  , gSeq
  , fr
  , inr
  , outr
  , seqComm
  , mSeqComm
  , mChoice
  , mComm
  , mGPar
  , mGSeq
  , mNewRole
  , mGSkip
  , GBranch
  , Msg (..)
  ) where

import Control.Monad ( liftM2 )
import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Foldable as F
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

data Op = Seq | Par deriving Eq

-- | Global type prefixes
data GT pl ann
  = Choice Role (GBranch pl ann)
  | Comm (Msg pl ann)
  | NewRole Role (GT pl ann)
  | GComp Op (GT pl ann) (GT pl ann)
  | GSkip

gPar :: GT pl ann -> GT pl ann -> GT pl ann
gPar = GComp Par

gSeq :: GT pl ann -> GT pl ann -> GT pl ann
gSeq = GComp Seq

fr :: GT pl ann -> Set Role
fr (Choice x1 x2)  = x1 `Set.insert` F.foldl1 Set.union (fmap fr x2)
fr (Comm x)        = Set.fromList $ rfrom x ++ rto x
fr (NewRole x1 x2) = x1 `Set.delete` fr x2
fr (GComp _ x1 x2)    = fr x1 `Set.union` fr x2
fr GSkip           = Set.empty

inr :: GT pl ann -> Set Role
inr (Choice x1 x2)  = x1 `Set.insert` F.foldl1 Set.union (fmap inr x2)
inr (Comm x)        = Set.fromList $ rfrom x
inr (NewRole _x x2) = inr x2
inr (GComp Par x1 x2)    = inr x1 `Set.union` inr x2
inr (GComp Seq x1 x2)    = inr x1 `Set.union` (inr x2 `Set.difference` outr x1)
inr GSkip           = Set.empty

outr :: GT pl ann -> Set Role
outr (Choice x1 x2)  = x1 `Set.insert` F.foldl1 Set.union (fmap outr x2)
outr (Comm x)        = Set.fromList $ rto x
outr (NewRole _x x2) = outr x2
outr (GComp Par x1 x2)    = outr x1 `Set.union` outr x2
outr (GComp Seq x1 x2)    = outr x2 `Set.union` (outr x1 `Set.difference` inr x2)
outr GSkip           = Set.empty

seqComm :: [Msg t a] -> GT t a
seqComm = F.foldr1 (GComp Seq) . map Comm

mSeqComm :: Monad m => m [Msg t a] -> m (GT t a)
mSeqComm = fmap seqComm

mChoice :: Monad m
        => m Role
        -> m (GBranch pl ann)
        -> m (GT pl ann)
mChoice = liftM2 Choice

mComm :: Monad m
      => m (Msg pl ann)
      -> m (GT pl ann)
mComm = fmap Comm

mGPar :: Monad m
      => m (GT pl ann)
      -> m (GT pl ann)
      -> m (GT pl ann)
mGPar = liftM2 (GComp Par)

mGSeq :: Monad m
      => m (GT pl ann)
      -> m (GT pl ann)
      -> m (GT pl ann)
mGSeq = liftM2 (GComp Seq)

mNewRole :: Monad m
          => m Role
          -> m (GT pl ann)
          -> m (GT pl ann)
mNewRole = liftM2 NewRole

mGSkip :: Monad m
      => m (GT pl ann)
mGSkip = return GSkip

type GBranch pl ann = Alt ann (GT pl ann)

instance (Pretty ann, Pretty pl) => Pretty (GT pl ann) where
  pretty (Choice src b) = [ppr| src > "{" > b > "}" |]
  pretty (Comm i) = [ppr| i |]
  pretty (GComp Par g1 g2) = [ppr| g1 + "||" + g2 |]
  pretty (GComp Seq g1 g2) = [ppr| g1 + ".." + g2 |]
  pretty (NewRole r g) = [ppr| "\\" > r + "." + g |]
  pretty GSkip = [ppr| "skip" |]

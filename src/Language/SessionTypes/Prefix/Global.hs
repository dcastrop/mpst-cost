{-# LANGUAGE QuasiQuotes #-}
module Language.SessionTypes.Prefix.Global
  ( GT (..)
  , Op (..)
  , gPar
  , gSeq
  , fr
  , inputr
  , outputr
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
import qualified Data.Text.Prettyprint.Doc as Ppr
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
      [ppr| RS from > "->" > RS to > "<" > ty + "|" + ann > ">" |]

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

inputr :: GT pl ann -> Set Role
inputr (Choice x1 x2)  = x1 `Set.insert` F.foldl1 Set.union (fmap inputr x2)
inputr (Comm x)        = Set.fromList $ rfrom x
inputr (NewRole _x x2) = inputr x2
inputr (GComp Par x1 x2)    = inputr x1 `Set.union` inputr x2
inputr (GComp Seq x1 x2)    = inputr x1 `Set.union` (inputr x2 `Set.difference` outputr x1)
inputr GSkip           = Set.empty

outputr :: GT pl ann -> Set Role
outputr (Choice x1 x2)  = x1 `Set.insert` F.foldl1 Set.union (fmap outputr x2)
outputr (Comm x)        = Set.fromList $ rto x
outputr (NewRole _x x2) = outputr x2
outputr (GComp Par x1 x2)    = outputr x1 `Set.union` outputr x2
outputr (GComp Seq x1 x2)    = outputr x2 `Set.union` (outputr x1 `Set.difference` inputr x2)
outputr GSkip           = Set.empty

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

type GBranch pl ann = Alt (GT pl ann)

instance (Pretty ann, Pretty pl) => Pretty (GT pl ann) where
  pretty (Choice src b) = [ppr| src > "{" > b > "}" |]
  pretty (Comm i) = [ppr| i |]
  pretty (GComp Par g1 g2) = Ppr.vsep [[ppr| "(" > g1 |],
                                         Ppr.nest 2 [ppr|"||" + g2 > ")" |]
                                                    ]
  pretty (GComp Seq g1 g2) = Ppr.vsep [[ppr| "(" > g1 |],
                                         Ppr.nest 2 [ppr|".." + g2 > ")" |]
                                                    ]
  pretty (NewRole r g) = [ppr| "(\\" > r > "." | g > ")" |]
  pretty GSkip = [ppr| "skip" |]

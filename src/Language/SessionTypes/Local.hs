{-# LANGUAGE QuasiQuotes #-}
module Language.SessionTypes.Local
( LBranch
, LT (..)
) where

import Data.Text.Prettyprint.Doc ( Pretty, pretty )
import Data.Text.Prettyprint.EDoc

import Language.SessionTypes.Common

-- | Local types
data LT v pl ann =
    Branch Role (LBranch v pl ann)
  | Select [Role] (LBranch v pl ann)
  | Recv [Role] pl (LT v pl ann)
  | Send [Role] pl (LT v pl ann)
  | LRec v (LT v pl ann)
  | LVar v
  | LEnd

type LBranch v pl ann = Alt (LT v pl ann)

instance (Pretty v, Pretty ann, Pretty pl) => Pretty (LT v pl ann) where
  pretty (Send r t b) = [ppr| RS r > '!' > t + "." + b |]
  pretty (Recv r t b) = [ppr| r > '?' > t + "." + b |]
  pretty (Select r b) = [ppr| RS r > '*' > b |]
  pretty (Branch r b) = [ppr| r > '&' > b |]
  pretty (LRec v x)   = [ppr| "rec" > v + "." > x |]
  pretty (LVar v)     = [ppr| v |]
  pretty LEnd         = [ppr| "end" |]

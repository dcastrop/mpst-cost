{-# LANGUAGE QuasiQuotes #-}
module Language.SessionTypes.Global
( GT (..)
, GBranch
, Msg (..)
, (...)
) where

import Data.Set ( Set )
import Data.Text.Prettyprint.Doc ( Pretty, pretty )
import Data.Text.Prettyprint.EDoc

import Language.SessionTypes.Common

-- | Annotated messages
data Msg pl ann =
  Msg { rfrom :: Role
      , rto :: Set Role
      , rty :: pl
      , msgAnn :: ann }

instance (Pretty pl, Pretty ann) => Pretty (Msg pl ann) where
  pretty (Msg from to ty ann) =
      [ppr| from > "->" > RS to > "{" > ann > "} :" > ty |]

-- | Global types
data GT v pl ann = Choice Role (Set Role) (GBranch v pl ann)
              | Comm (Msg pl ann) (GT v pl ann)
              | GRec v (GT v pl ann)
              | GVar v
              | GEnd

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



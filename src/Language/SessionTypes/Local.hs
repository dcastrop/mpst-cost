module Language.SessionTypes.Local
( LBranch
, LT (..)
) where

import Data.Text.Prettyprint.Doc ( Pretty, pretty )
import qualified Data.Text.Prettyprint.Doc as Pretty

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
  pretty (Send r t b) =
    Pretty.hsep [ pretty $ RS r
                , pretty "!"
                , Pretty.angles $ pretty t
                , pretty "."
                , pretty b
                ]
  pretty (Recv r t b) =
    Pretty.hsep [ pretty $ RS r
                , pretty "?"
                , Pretty.parens $ pretty t
                , pretty "."
                , pretty b
                ]
  pretty (Select r b) =
    Pretty.hsep [ pretty $ RS r
                , pretty "*"
                , pretty b
                ]
  pretty (Branch r b) =
    Pretty.hsep [ pretty r
                , pretty "&"
                , pretty b
                ]
  pretty (LRec v x)   =
    Pretty.hsep [ pretty "rec"
                , pretty v Pretty.<> pretty "."
                , pretty "."
                , pretty x
                ]
  pretty (LVar v) =
    pretty v
  pretty LEnd =
    pretty "end"

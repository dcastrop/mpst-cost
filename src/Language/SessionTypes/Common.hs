{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.SessionTypes.Common
( Label
, Role
, RoleSet (..)
, Alt (..)
) where

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Text.Prettyprint.Doc ( Pretty, pretty )
import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.EDoc

newtype Label  = Lbl { labelId  :: Int }
  deriving (Eq, Ord)

instance Pretty Label where
  pretty (labelId -> l) = [ppr| "_l" + l |]

newtype Role   = Rol { roleName :: Int }
  deriving (Eq, Ord)

instance Pretty Role where
  pretty (roleName -> r) = [ppr| "_r" + r |]

newtype RoleSet = RS { unRS :: Set Role }

instance Pretty RoleSet where
  pretty (unRS -> s) =
      Pretty.braces $ Pretty.hsep $
        Pretty.punctuate (pretty ',') $ map pretty $ Set.toList s

newtype Alt ann c = Alt { altMap :: Map ann c }

instance (Pretty c, Pretty ann) => Pretty (Alt ann c) where
  pretty (Map.assocs . altMap -> [(_, c)]) =
      pretty c

  pretty (Map.assocs . altMap -> m) =
      Pretty.braces $ Pretty.align $
        Pretty.vsep $
          Pretty.punctuate Pretty.semi $
            map (uncurry prettyLblAlt) m
    where
      prettyLblAlt lbl c = [ppr| lbl > '.' > c |]


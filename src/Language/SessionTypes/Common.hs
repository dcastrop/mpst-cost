{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.SessionTypes.Common
  ( Label (..)
  , Role
  , RoleSet (..)
  , Alt
  , addAlt
  , emptyAlt
  ) where

import Data.Map ( Map )
import qualified Data.Map.Strict as Map
import Data.Text.Prettyprint.Doc ( Pretty, pretty )
import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.EDoc

data Label ann = Lbl { labelId  :: Int, labelAnn :: ann }

instance Eq (Label ann) where
  l1 == l2 = labelId l1 == labelId l2

instance Ord (Label ann) where
  l1 `compare` l2 = labelId l1 `compare` labelId l2

instance Pretty (Label ann) where
  pretty (labelId -> l) = [ppr| "_l" + l |]

newtype Role   = Rol { roleName :: Int }
  deriving (Eq, Ord)

instance Pretty Role where
  pretty (roleName -> r) = [ppr| "_r" + r |]

newtype RoleSet = RS { unRS :: [Role] }

instance Pretty RoleSet where
  pretty =
    Pretty.braces . Pretty.hsep . Pretty.punctuate (pretty ',')
    . map pretty . unRS

newtype Alt ann c = Alt { altMap :: Map (Label ann) c }

deriving instance Foldable (Alt ann)
deriving instance Functor (Alt ann)
deriving instance Traversable (Alt ann)

addAlt :: Int -> ann -> c -> Alt ann c -> Alt ann c
addAlt i a c = Alt . Map.insert (Lbl i a) c . altMap

emptyAlt :: Alt ann c
emptyAlt = Alt Map.empty

instance Pretty c => Pretty (Alt ann c) where
  pretty (Map.assocs . altMap -> [(_, c)]) = pretty c

  pretty (Map.assocs . altMap -> m) =
      Pretty.braces $ Pretty.align $ Pretty.vsep $
        Pretty.punctuate Pretty.semi $ map (uncurry prettyLblAlt) m
    where
      prettyLblAlt lbl c = [ppr| lbl > '.' > c |]

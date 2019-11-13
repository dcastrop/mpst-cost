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
  ) where

import Control.Monad ( liftM2, liftM3 )
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
              | GSeq [GT v pl ann]
              | GRec v (GT v pl ann)
              | GVar v
              | GEnd
  deriving Show

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

infixr 4 ...

(...) :: Msg pl ann -> GT v pl ann -> GT v pl ann
(...) = Comm

type GBranch v pl ann = Alt (GT v pl ann)

getRoles :: GT v pl ann -> Set Role
getRoles GEnd = Set.empty
getRoles (GSeq gs) = Set.unions $ map getRoles gs
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
  pretty (GSeq gs) = Pretty.align $!
                     Pretty.vsep $!
                     Pretty.punctuate (pretty ";") $!
                     map pretty gs
  pretty (Choice src dest b) = Pretty.align
                               $! Pretty.vsep
                               $! [ Pretty.hsep [ pretty src
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

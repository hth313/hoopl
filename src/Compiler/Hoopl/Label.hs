{-# LANGUAGE CPP, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
#if __GLASGOW_HASKELL__ >= 701
-- {-# LANGUAGE Safe #-}
#endif

module Compiler.Hoopl.Label
  ( Label
  , freshLabel
  , LabelSet, LabelMap
  , FactBase, noFacts, lookupFact

  , uniqueToLbl -- MkGraph and GHC use only
  , lblToUnique -- GHC use only
  )

where

import Compiler.Hoopl.Unique
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM
import Data.EnumSet (EnumSet)
#if !MIN_VERSION_base(4,8,0)
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
#endif

-----------------------------------------------------------------------------
--		Label
-----------------------------------------------------------------------------

newtype Label = Label { lblToUnique :: Unique }
  deriving (Eq, Ord)

uniqueToLbl :: Unique -> Label
uniqueToLbl = Label

instance Enum Label where
  fromEnum = lblToUnique
  toEnum = Label

instance Show Label where
  show (Label n) = "L" ++ show n

freshLabel :: UniqueMonad m => m Label
freshLabel = freshUnique >>= return . uniqueToLbl

-----------------------------------------------------------------------------
-- LabelSet

type LabelSet = EnumSet Label

-----------------------------------------------------------------------------
-- LabelMap

type LabelMap v = EnumMap Label v

-----------------------------------------------------------------------------
-- FactBase

type FactBase f = LabelMap f

noFacts :: FactBase f
noFacts = EM.empty

lookupFact :: Label -> FactBase f -> Maybe f
lookupFact = EM.lookup

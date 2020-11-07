{- Baseclasses for Map-like and Set-like collections inspired by containers. -}

{-# LANGUAGE CPP, TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 701
--{-# LANGUAGE Safe #-}
#endif

module Compiler.Hoopl.Collections ( setInsertList, setDeleteList
                                  , mapInsertList, mapDeleteList
                                  ) where

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as ES

-- Helper functions
setInsertList :: Enum k => [k] -> EnumSet k -> EnumSet k
setInsertList keys set = ES.union set (ES.fromList keys)
-- setInsertList keys set = foldl' (flip setInsert) set keys

setDeleteList :: Enum k => [k] -> EnumSet k -> EnumSet k
setDeleteList keys set = ES.difference set (ES.fromList keys)
--setDeleteList keys set = foldl' (flip setDelete) set keys


-- Helper functions for IsMap class
mapInsertList :: Enum k => [(k, a)] -> EnumMap k a -> EnumMap k a
mapInsertList assocs map = EM.union map (EM.fromList assocs)
--mapInsertList assocs map = foldl' (flip (uncurry mapInsert)) map assocs

mapDeleteList :: Enum k => [k] -> EnumMap k a -> EnumMap k a
mapDeleteList keys map = EM.restrictKeys map (ES.fromList keys)

{- |

   Collection functions for compatibility with earlier versions.

-}

module Compiler.Hoopl.Compat ( setNull
                             , setSize
                             , setMember
                             , setEmpty
                             , setSingleton
                             , setInsert
                             , setDelete
                             , setFilter
                             , setUnion
                             , setDifference
                             , setIntersection
                             , setIsSubsetOf
                             , setFold
                             , setElems
                             , setFromList
                             , setUnions
                             , mapNull
                             , mapSize
                             , mapMember
                             , mapLookup
                             , mapFindWithDefault
                             , mapEmpty
                             , mapSingleton
                             , mapInsert
                             , mapInsertWith
                             , mapDelete
                             , mapAdjust
                             , mapUnion
                             , mapUnionWithKey
                             , mapDifference
                             , mapIntersection
                             , mapIsSubmapOf
                             , mapMap
                             , mapMapWithKey
                             , mapFold
                             , mapFoldWithKey
                             , mapFilter
                             , mapFilterWithKey
                             , mapAccum
                             , mapAccumWithKey
                             , mapElems
                             , mapKeys
                             , mapToList
                             , mapFromList
                             , mapFromListWith
                             , mapUnions
                             ) where

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as ES

setNull :: Enum k => EnumSet k -> Bool
setNull = ES.null
{-# INLINE setNull #-}

setSize :: Enum k => EnumSet k -> Int
setSize = ES.size
{-# INLINE setSize #-}

setMember :: Enum k => k -> EnumSet k -> Bool
setMember = ES.member
{-# INLINE setMember #-}

setEmpty :: EnumSet k
setEmpty = ES.empty
{-# INLINE setEmpty #-}

setSingleton :: Enum k => k -> EnumSet k
setSingleton = ES.singleton
{-# INLINE setSingleton #-}

setInsert :: Enum k => k -> EnumSet k -> EnumSet k
setInsert = ES.insert
{-# INLINE setInsert #-}

setDelete :: Enum k => k -> EnumSet k -> EnumSet k
setDelete = ES.delete
{-# INLINE setDelete #-}

setFilter :: Enum k => (k -> Bool) -> EnumSet k -> EnumSet k
setFilter = ES.filter
{-# INLINE setFilter #-}

setUnion :: EnumSet k -> EnumSet k -> EnumSet k
setUnion = ES.union
{-# INLINE setUnion #-}

setDifference :: EnumSet k -> EnumSet k -> EnumSet k
setDifference = ES.difference
{-# INLINE setDifference #-}

setIntersection :: EnumSet k -> EnumSet k -> EnumSet k
setIntersection = ES.intersection
{-# INLINE setIntersection #-}

setIsSubsetOf :: EnumSet k -> EnumSet k -> Bool
setIsSubsetOf = ES.isSubsetOf
{-# INLINE setIsSubsetOf #-}

setFold :: Enum k => (k -> b -> b) -> b -> EnumSet k -> b
setFold = ES.fold
{-# INLINE setFold #-}

setElems :: Enum k => EnumSet k -> [k]
setElems = ES.elems
{-# INLINE setElems #-}

setFromList :: Enum k => [k] -> EnumSet k
setFromList = ES.fromList
{-# INLINE setFromList #-}

setUnions :: [EnumSet k] -> EnumSet k
setUnions = ES.unions
{-# INLINE setUnions #-}

mapNull :: EnumMap k a -> Bool
mapNull = EM.null
{-# INLINE mapNull #-}

mapSize :: EnumMap k a -> Int
mapSize = EM.size
{-# INLINE mapSize #-}

mapMember :: Enum k => k -> EnumMap k a -> Bool
mapMember = EM.member
{-# INLINE mapMember #-}

mapLookup :: Enum k => k -> EnumMap k a -> Maybe a
mapLookup = EM.lookup
{-# INLINE mapLookup #-}

mapFindWithDefault :: Enum k => a -> k -> EnumMap k a -> a
mapFindWithDefault = EM.findWithDefault
{-# INLINE mapFindWithDefault #-}

mapEmpty :: EnumMap k a
mapEmpty = EM.empty
{-# INLINE mapEmpty #-}

mapSingleton :: Enum k => k -> a -> EnumMap k a
mapSingleton = EM.singleton
{-# INLINE mapSingleton #-}

mapInsert :: Enum k => k -> a -> EnumMap k a -> EnumMap k a
mapInsert = EM.insert
{-# INLINE mapInsert #-}

mapInsertWith :: Enum k => (a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
mapInsertWith = EM.insertWith
{-# INLINE mapInsertWith #-}

mapDelete :: Enum k => k -> EnumMap k a -> EnumMap k a
mapDelete = EM.delete
{-# INLINE mapDelete #-}

mapAdjust ::  Enum k => (a -> a) -> k -> EnumMap k a -> EnumMap k a
mapAdjust = EM.adjust
{-# INLINE mapAdjust #-}

mapUnion :: EnumMap k a -> EnumMap k a -> EnumMap k a
mapUnion = EM.union
{-# INLINE mapUnion #-}

mapUnionWithKey :: Enum k => (k -> a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
mapUnionWithKey = EM.unionWithKey
{-# INLINE mapUnionWithKey #-}

mapDifference :: EnumMap k a -> EnumMap k b -> EnumMap k a
mapDifference = EM.difference
{-# INLINE mapDifference #-}

mapIntersection :: EnumMap k a -> EnumMap k b -> EnumMap k a
mapIntersection = EM.intersection
{-# INLINE mapIntersection #-}

mapIsSubmapOf :: Eq a => EnumMap k a -> EnumMap k a -> Bool
mapIsSubmapOf = EM.isSubmapOf
{-# INLINE mapIsSubmapOf #-}

mapMap :: (a -> b) -> EnumMap k a -> EnumMap k b
mapMap = EM.map
{-# INLINE mapMap #-}

mapMapWithKey :: Enum k => (k -> a -> b) -> EnumMap k a -> EnumMap k b
mapMapWithKey = EM.mapWithKey
{-# INLINE mapMapWithKey #-}

mapFold :: (a -> b -> b) -> b -> EnumMap k a -> b
mapFold = EM.foldr
{-# INLINE mapFold #-}

mapFoldWithKey :: (Enum k) => (k -> a -> b -> b) -> b -> EnumMap k a -> b
mapFoldWithKey = EM.foldrWithKey
{-# INLINE mapFoldWithKey #-}

mapFilter :: (a -> Bool) -> EnumMap k a -> EnumMap k a
mapFilter = EM.filter
{-# INLINE mapFilter #-}

mapFilterWithKey :: Enum k => (k -> a -> Bool) -> EnumMap k a -> EnumMap k a
mapFilterWithKey = EM.filterWithKey
{-# INLINE mapFilterWithKey #-}

mapAccum :: (a -> b -> (a, c)) -> a -> EnumMap k b -> (a, EnumMap k c)
mapAccum = EM.mapAccum
{-# INLINE mapAccum #-}

mapAccumWithKey :: Enum k => (a -> k -> b -> (a, c)) -> a -> EnumMap k b -> (a, EnumMap k c)
mapAccumWithKey = EM.mapAccumWithKey
{-# INLINE mapAccumWithKey #-}

mapElems :: EnumMap k a -> [a]
mapElems = EM.elems
{-# INLINE mapElems #-}

mapKeys :: (Enum k) => EnumMap k a -> [k]
mapKeys = EM.keys
{-# INLINE mapKeys #-}

mapToList :: Enum k => EnumMap k a -> [(k, a)]
mapToList = EM.toList
{-# INLINE mapToList #-}

mapFromList :: Enum k => [(k, a)] -> EnumMap k a
mapFromList = EM.fromList
{-# INLINE mapFromList #-}

mapFromListWith :: Enum k => (a -> a -> a) -> [(k, a)] -> EnumMap k a
mapFromListWith = EM.fromListWith
{-# INLINE mapFromListWith #-}

mapUnions :: [EnumMap k a] -> EnumMap k a
mapUnions = EM.unions
{-# INLINE mapUnions #-}

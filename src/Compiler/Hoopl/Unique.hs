{-# LANGUAGE CPP, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif


module Compiler.Hoopl.Unique
  ( Unique, intToUnique
  , UniqueMonad(..)
  , SimpleUniqueMonad, runSimpleUniqueMonad
  , UniqueMonadT, runUniqueMonadT

  , uniqueToInt -- exposed through GHC module only!
  )

where

import Compiler.Hoopl.Checkpoint
import Control.Applicative as AP
import Control.Monad (ap,liftM)
#if !MIN_VERSION_base(4,8,0)
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
#endif

-----------------------------------------------------------------------------
--		Unique
-----------------------------------------------------------------------------

type Unique = Int

uniqueToInt :: Unique -> Int
uniqueToInt = id

intToUnique :: Int -> Unique
intToUnique = id

----------------------------------------------------------------
-- Monads

class Monad m => UniqueMonad m where
  freshUnique :: m Unique

newtype SimpleUniqueMonad a = SUM { unSUM :: [Unique] -> (a, [Unique]) }

instance Functor SimpleUniqueMonad where
  fmap = liftM

instance Applicative SimpleUniqueMonad where
  pure a = SUM $ \us -> (a, us)
  (<*>) = ap

instance Monad SimpleUniqueMonad where
  return = AP.pure
  m >>= k  = SUM $ \us -> let (a, us') = unSUM m us in
                              unSUM (k a) us'

instance UniqueMonad SimpleUniqueMonad where
  freshUnique = SUM $ f
    where f (u:us) = (u, us)
          f _ = error "Unique.freshUnique(SimpleUniqueMonad): empty list"

instance CheckpointMonad SimpleUniqueMonad where
  type Checkpoint SimpleUniqueMonad = [Unique]
  checkpoint = SUM $ \us -> (us, us)
  restart us = SUM $ \_  -> ((), us)

runSimpleUniqueMonad :: SimpleUniqueMonad a -> a
runSimpleUniqueMonad m = fst (unSUM m allUniques)

----------------------------------------------------------------

newtype UniqueMonadT m a = UMT { unUMT :: [Unique] -> m (a, [Unique]) }

instance Monad m => Functor (UniqueMonadT m) where
  fmap  = liftM

instance Monad m => Applicative (UniqueMonadT m) where
  pure a = UMT $ \us -> return (a, us)
  (<*>) = ap

instance Monad m => Monad (UniqueMonadT m) where
  return = pure
  m >>= k  = UMT $ \us -> do { (a, us') <- unUMT m us; unUMT (k a) us' }

instance Monad m => UniqueMonad (UniqueMonadT m) where
  freshUnique = UMT $ f
    where f (u:us) = return (u, us)
          f _ = error "Unique.freshUnique(UniqueMonadT): empty list"

runUniqueMonadT :: Monad m => UniqueMonadT m a -> m a
runUniqueMonadT m = do { (a, _) <- unUMT m allUniques; return a }

allUniques :: [Unique]
allUniques = [1..]

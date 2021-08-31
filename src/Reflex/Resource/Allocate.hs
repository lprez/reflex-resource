{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving #-}

module Reflex.Resource.Allocate (
    MonadAllocate(..),
    AllocateT(..),
    newResource
) where

import Control.Monad.Reader
import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.PerformEvent.Class

class (Monoid (Allocation m), Monad m) => MonadAllocate m where
    type family Allocation m
    createFrame :: Allocation m -> m (Performable m (), Performable m ())

-- | A basic 'MonadAllocate' implementation. It allocates resources using the underlying monad
-- and returns results right away.
newtype AllocateT m a = AllocateT { unAllocateT :: m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadSample t, MonadHold t)

instance MonadTrans AllocateT where
    lift = AllocateT

instance PerformEvent t m => PerformEvent t (AllocateT m) where
    type instance Performable (AllocateT m) = AllocateT (Performable m)
    performEvent_ e = AllocateT $ performEvent_ (fmap unAllocateT e)
    performEvent e = AllocateT $ performEvent (fmap unAllocateT e)

-- | Contains the destructors of the resources in the current frame.
newtype AllocationM m = AllocationM (m ())

instance Monad m => Semigroup (AllocationM m) where
    AllocationM x <> AllocationM y = AllocationM $ y >> x

instance Monad m => Monoid (AllocationM m) where
    mempty = AllocationM $ return ()

instance (Monad m, Monad (Performable m)) => MonadAllocate (AllocateT m) where
    type Allocation (AllocateT m) = AllocationM (Performable m)
    createFrame (AllocationM deallocations) = return (return (), AllocateT deallocations)

-- | Allocate a new resource and returns the destructor.
newResource :: (Monad m, Functor f) => m (f (a, Performable m ())) -> AllocateT m (f (a, AllocationM (Performable m)))
newResource = AllocateT . fmap (fmap (\(x, d) -> (x, AllocationM d)))

instance Adjustable t m => Adjustable t (AllocateT m) where
    runWithReplace (AllocateT r0) r' =
        AllocateT $ runWithReplace r0 (fmap unAllocateT r')
    traverseIntMapWithKeyWithAdjust f m0 m' =
        AllocateT $ traverseIntMapWithKeyWithAdjust (\k v -> unAllocateT (f k v)) m0 m'
    traverseDMapWithKeyWithAdjust f m0 m' =
        AllocateT $ traverseDMapWithKeyWithAdjust (\k v -> unAllocateT (f k v)) m0 m'
    traverseDMapWithKeyWithAdjustWithMove f m0 m' =
        AllocateT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unAllocateT (f k v)) m0 m'

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving #-}

module Reflex.Resource.Allocate (
    MonadAllocate(..),
    -- * AllocateT
    AllocateT(..),
    AllocationM,
    newResource
) where

import Control.Monad.Morph
import Control.Monad.Reader
import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.PerformEvent.Class

-- | Monads that manage the allocation of the resources.
class (Monoid (Allocation pm m), Monad m) => MonadAllocate pm m where
    -- | An individual allocation or a group of allocations.
    type family Allocation pm m
    -- | Builds the initialization and finalization actions for a group of resources.
    createFrame :: Allocation pm m -> m (pm (), pm ())

-- | A basic 'MonadAllocate' implementation.
newtype AllocateT m a = AllocateT { runAllocateT :: m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadSample t, MonadHold t)

instance MonadTrans AllocateT where
    lift = AllocateT

instance MFunctor AllocateT where
    hoist f (AllocateT m) = AllocateT $ f m

instance MMonad AllocateT where
    embed f (AllocateT m) = f m

instance PerformEvent t m => PerformEvent t (AllocateT m) where
    type instance Performable (AllocateT m) = Performable m
    performEvent_ e = AllocateT $ performEvent_ e
    performEvent e = AllocateT $ performEvent e

-- | Contains the initialization and finalization actions of the resources in the current frame.
newtype AllocationM m = AllocationM (m (), m ())

instance Monad m => Semigroup (AllocationM m) where
    AllocationM (in1, fin1) <> AllocationM (in2, fin2) =
        AllocationM (in1 >> in2, fin2 >> fin1)

instance Monad m => Monoid (AllocationM m) where
    mempty = AllocationM (return (), return ())

instance (Monad m, Monad pm) => MonadAllocate pm (AllocateT m) where
    type Allocation pm (AllocateT m) = AllocationM pm
    createFrame (AllocationM infin) = return infin

-- | Creates a new resource. The actual allocation can either happen during the execution
-- of the action in the first argument, or during the initialization. The deallocation
-- must happen during the finalization.
newResource :: (Monad m, Functor f)
            => m a
            -- ^ The action that returns the value that will be wrapped in the @Res@
            -- (e.g. a handle).
            -> (a -> f (pm (), pm ()))
            -- ^ The initialization and finalization actions for the resource.
            -> AllocateT m (a, f (AllocationM pm))
newResource m f = AllocateT $ fmap (\x -> (x, fmap AllocationM $ f x)) m

instance Adjustable t m => Adjustable t (AllocateT m) where
    runWithReplace (AllocateT r0) r' =
        AllocateT $ runWithReplace r0 (fmap runAllocateT r')
    traverseIntMapWithKeyWithAdjust f m0 m' =
        AllocateT $ traverseIntMapWithKeyWithAdjust (\k v -> runAllocateT (f k v)) m0 m'
    traverseDMapWithKeyWithAdjust f m0 m' =
        AllocateT $ traverseDMapWithKeyWithAdjust (\k v -> runAllocateT (f k v)) m0 m'
    traverseDMapWithKeyWithAdjustWithMove f m0 m' =
        AllocateT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> runAllocateT (f k v)) m0 m'

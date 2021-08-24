{-# LANGUAGE RankNTypes, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving #-}

module Reflex.Resource.IntMapAlloc (
    AllocationM(..),
    IntMapAllocT,
    runIntMapAllocTIO,
    --runIntMapAllocT,
    IntMapPerformT,
    unwrapIntMapPerformT,
    newResource,
    newResource',
    getResource,
    dynResourceSampler,
    tmpResourceSampler,
) where

import Data.IORef
import qualified Data.IntMap as I
import Control.Monad.IO.Class
import Control.Monad.Reader
import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.PerformEvent.Class

import Reflex.Resource

newtype IntMapAllocT rt m a = IntMapAllocT (ReaderT (IORef (I.IntMap rt, I.IntMap (Performable m ()))) m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadSample t, MonadHold t)

newtype IntMapPerformT rt m a = IntMapPerformT { unIntMapPerformT :: ReaderT (IORef (I.IntMap rt, I.IntMap (m ()))) m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadSample t, MonadHold t)

-- | A monadic action that allocates a resource and returns the respective deallocator.
newtype AllocationM m = AllocationM (m (m ()))

instance Monad m => Semigroup (AllocationM m) where
    AllocationM x <> AllocationM y = AllocationM $ flip (>>) <$> x <*> y

instance Monad m => Monoid (AllocationM m) where
    mempty = AllocationM $ return (return ())


instance MonadTrans (IntMapAllocT rt) where
    lift = IntMapAllocT . lift

instance MonadTrans (IntMapPerformT rt) where
    lift = IntMapPerformT . lift

intMapAllocT :: (IORef (I.IntMap rt, I.IntMap (Performable m ())) -> m a) -> IntMapAllocT rt m a
intMapAllocT f = IntMapAllocT $ ReaderT f

intMapPerformT :: (IORef (I.IntMap rt, I.IntMap (m ())) -> m a) -> IntMapPerformT rt m a
intMapPerformT f = IntMapPerformT $ ReaderT f

runIntMapAllocT :: IntMapAllocT rt m a -> IORef (I.IntMap rt, I.IntMap (Performable m ())) -> m a
runIntMapAllocT (IntMapAllocT m) = runReaderT m

runIntMapAllocTIO :: MonadIO m => IntMapAllocT rt m a -> m a
runIntMapAllocTIO m = liftIO (newIORef (I.empty, I.empty)) >>= runIntMapAllocT m

runIntMapPerformT :: IntMapPerformT rt m a -> IORef (I.IntMap rt, I.IntMap (m ())) -> m a
runIntMapPerformT (IntMapPerformT m) = runReaderT m

unwrapIntMapPerformT :: Monad m => ((forall a. IntMapPerformT rt (Performable m) a -> Performable m a) -> b) -> IntMapAllocT rt m b
unwrapIntMapPerformT f = (\imapRef -> f $ flip runIntMapPerformT imapRef) <$> IntMapAllocT ask

newResource :: MonadIO m => m (rt, m ()) -> Int -> AllocationM (IntMapPerformT rt m)
newResource r = newResource' (fmap (\(r', d) -> (r', lift d)) $ lift r)

newResource' :: MonadIO m => IntMapPerformT rt m (rt, IntMapPerformT rt m ()) -> Int -> AllocationM (IntMapPerformT rt m)
newResource' r resid = AllocationM . intMapPerformT $ \imapRef -> do (resval, deallocate) <- runIntMapPerformT r imapRef
                                                                     liftIO . atomicModifyIORef' imapRef $
                                                                         \(resmap, rmap) -> ((I.insert resid resval resmap, rmap), ())
                                                                     return deallocate

getResource :: MonadIO m => Int -> IntMapPerformT rt m (Maybe rt)
getResource resid = intMapPerformT $ \imapRef -> liftIO (readIORef imapRef) >>= \(resmap, _) -> return $ I.lookup resid resmap

dynResourceSampler :: (Monad m, Reflex t, MonadSample t (Performable m), MonadIO (Performable m))
                   => DynRes r t Int
                   -> ResourceT r t (IntMapAllocT rt m) (Res r (Performable m rt))
dynResourceSampler x = do sampler <- dynResSampler x
                          imapRef <- lift $ IntMapAllocT ask
                          return $ fmap (\s -> do resid <- runIntMapPerformT s imapRef
                                                  (resmap, _) <- liftIO (readIORef imapRef)
                                                  return $ resmap I.! resid) sampler

tmpResourceSampler :: (Monad m, Reflex t, MonadSample t (Performable m), MonadIO (Performable m))
                   => Res (TmpResourceContext r) Int
                   -> ResourceT r t (IntMapAllocT rt m) (Res (TmpResourceContext r) (Performable m rt))
tmpResourceSampler x = do sampler <- tmpResSampler x
                          imapRef <- lift $ IntMapAllocT ask
                          return $ fmap (\s -> do resid <- runIntMapPerformT s imapRef
                                                  (resmap, _) <- liftIO (readIORef imapRef)
                                                  return $ resmap I.! resid) sampler

instance PerformEvent t m => PerformEvent t (IntMapAllocT rt m) where
    type instance Performable (IntMapAllocT rt m) = IntMapPerformT rt (Performable m)
    performEvent_ e = IntMapAllocT $ performEvent_ (fmap unIntMapPerformT e)
    performEvent e = IntMapAllocT $ performEvent (fmap unIntMapPerformT e)

instance MonadIO m => MonadAllocate (IntMapPerformT rt m) where
    type Allocation (IntMapPerformT rt m) = AllocationM (IntMapPerformT rt m)
    allocateFrame rid (AllocationM allocations) = do imapRef <- IntMapPerformT ask
                                                     deallocations <- allocations
                                                     liftIO . atomicModifyIORef' imapRef $
                                                       \(resmap, rmap) -> ((resmap, I.insert rid (runIntMapPerformT deallocations imapRef) rmap), ())
    deallocateFrame rid = do imapRef <- IntMapPerformT ask
                             deallocations <- liftIO . atomicModifyIORef' imapRef $
                               \(resmap, rmap) -> case I.lookup rid rmap of
                                                       Just ds -> ((resmap, I.delete rid rmap), ds)
                                                       Nothing -> ((resmap, rmap), return ())
                             lift deallocations

instance Adjustable t m => Adjustable t (IntMapAllocT rt m) where
    runWithReplace (IntMapAllocT r0) r' =
        intMapAllocT $ \imapRef -> runWithReplace (runReaderT r0 imapRef) (fmap (flip runIntMapAllocT imapRef) r')
    traverseIntMapWithKeyWithAdjust f m0 m' =
        intMapAllocT $ \imapRef -> traverseIntMapWithKeyWithAdjust (\k v -> runIntMapAllocT (f k v) imapRef) m0 m'
    traverseDMapWithKeyWithAdjust f m0 m' =
        intMapAllocT $ \imapRef -> traverseDMapWithKeyWithAdjust (\k v -> runIntMapAllocT (f k v) imapRef) m0 m'
    traverseDMapWithKeyWithAdjustWithMove f m0 m' =
        intMapAllocT $ \imapRef -> traverseDMapWithKeyWithAdjustWithMove (\k v -> runIntMapAllocT (f k v) imapRef) m0 m'

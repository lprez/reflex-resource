{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, FlexibleContexts, RankNTypes #-}

module ValidateAlloc where

import Data.IORef
import qualified Data.IntMap as I
import qualified Data.IntSet as S
import Control.Monad.IO.Class
import Control.Monad.Reader
import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.PerformEvent.Class
import System.Random

import Reflex.Resource.Allocate

type ValidateState = (S.IntSet, I.IntMap S.IntSet)

newtype ValidateAllocT m a = ValidateAllocT { unValidateAllocT :: ReaderT (IORef ValidateState) m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadSample t, MonadHold t)

instance MonadTrans ValidateAllocT where
    lift = ValidateAllocT . lift

runValidateAllocT :: ValidateAllocT m a -> IORef ValidateState -> m a
runValidateAllocT (ValidateAllocT m) = runReaderT m

runValidateAllocTIO :: MonadIO m => ValidateAllocT m a -> m a
runValidateAllocTIO m = liftIO (newIORef (S.empty, I.empty)) >>= runValidateAllocT m

validateAllocT :: (IORef ValidateState -> m a) -> ValidateAllocT m a
validateAllocT f = ValidateAllocT $ ReaderT f

instance PerformEvent t m => PerformEvent t (ValidateAllocT m) where
    type instance Performable (ValidateAllocT m) = ValidateAllocT (Performable m)
    performEvent_ e = ValidateAllocT $ performEvent_ (fmap unValidateAllocT e)
    performEvent e = ValidateAllocT $ performEvent (fmap unValidateAllocT e)

instance (MonadIO m, MonadIO pm) => MonadAllocate pm (ValidateAllocT m) where
    type Allocation pm (ValidateAllocT m) = S.IntSet
    createFrame allocations = validateAllocT $ \stateRef ->
            do rid <- liftIO randomIO
               return ( liftIO . atomicModifyIORef' stateRef $
                            \(resources, frames) -> ((S.union resources allocations, I.insert rid allocations frames), ())
                      , liftIO . atomicModifyIORef' stateRef $
                            \(resources, frames) -> ((S.difference resources (frames I.! rid), I.delete rid frames), ())
                      )

newResource :: (MonadIO m, Applicative f) => m (Int, f S.IntSet)
newResource = do rid <- liftIO randomIO
                 return (rid, pure $ S.singleton rid)

checkResource :: MonadIO m => Int -> ValidateAllocT m Bool
checkResource resid = validateAllocT $ \stateRef -> liftIO (readIORef stateRef) >>= \(resources, _) -> return $ S.member resid resources

noLeaks :: MonadIO m => ValidateAllocT m Bool
noLeaks = validateAllocT $ \stateRef -> liftIO $ readIORef stateRef >>=
                            \(resources, frames) -> return $ I.null frames && S.null resources

unwrapValidateAllocT :: Monad m => ((forall a. ValidateAllocT (Performable m) a -> Performable m a) -> b) -> ValidateAllocT m b
unwrapValidateAllocT f = (\imapRef -> f $ flip runValidateAllocT imapRef) <$> ValidateAllocT ask

instance Adjustable t m => Adjustable t (ValidateAllocT m) where
    runWithReplace (ValidateAllocT r0) r' =
        validateAllocT $ \imapRef -> runWithReplace (runReaderT r0 imapRef) (fmap (flip runValidateAllocT imapRef) r')
    traverseIntMapWithKeyWithAdjust f m0 m' =
        validateAllocT $ \imapRef -> traverseIntMapWithKeyWithAdjust (\k v -> runValidateAllocT (f k v) imapRef) m0 m'
    traverseDMapWithKeyWithAdjust f m0 m' =
        validateAllocT $ \imapRef -> traverseDMapWithKeyWithAdjust (\k v -> runValidateAllocT (f k v) imapRef) m0 m'
    traverseDMapWithKeyWithAdjustWithMove f m0 m' =
        validateAllocT $ \imapRef -> traverseDMapWithKeyWithAdjustWithMove (\k v -> runValidateAllocT (f k v) imapRef) m0 m'

{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleContexts #-}

module RIO where

import Control.Monad.Morph
import Control.Monad.Trans
import Reflex.Class
import Reflex.Resource
import Reflex.Resource.Allocate
import Reflex.PerformEvent.Class

type RIOT r t m a = ResourceT' r t IO (AllocateT m) a

liftRIO :: Monad m => m a -> RIOT r t m a
liftRIO = lift . lift

hoistRIO :: (Monad m, Monad n, MonadIO (Performable m))
         => (forall a. m a -> n a)
         -> RIOT r t m a
         -> RIOT r t n a
hoistRIO f = hoistResourceT liftIO (hoist f)

runRIO :: (Reflex t, Monad m, MonadSample t m', MonadIO (Performable m))
       => (forall r'. ResourceContext () r' => RIOT r' t m (Res r' a))
       -> m (a, IO (), m' (IO ()))
runRIO m = runAllocateT $ runResourceTContext' liftIO m

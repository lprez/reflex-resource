{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, UndecidableInstances, RecursiveDo, RankNTypes, TypeFamilies, GADTs #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent.Supply
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix
import Data.Dependent.Sum
import Data.Maybe
-- import Data.Type.Equality
-- import Data.GADT.Compare
import Data.IORef
import Data.Functor.Identity
import Data.Functor.Const
import Data.Functor.Misc (weakenDMapWith, ComposeMaybe(..))
-- import qualified Data.Map.Lazy as M
import qualified Data.IntSet as S
import qualified Data.IntMap as I
import qualified Data.IntMap.Merge.Lazy as I
-- import qualified Data.Patch.IntMap as M
-- import qualified Data.Dependent.Map as D
-- import qualified Data.Patch.DMapWithMove as D
import Reflex.Adjustable.Class
import Reflex.Class
import Reflex.Host.Class
import Reflex.Dynamic
import Reflex.Spider
import Reflex.PerformEvent.Base
import Reflex.PerformEvent.Class
import Reflex.Resource
import System.Random

import ValidateAlloc

type ValidateT r t m a = ResourceT r t (ValidateAllocT m) a

testResource :: (Reflex t, Monad m, MonadIO (Performable m), MonadIO m, ResourceContext r r') => ValidateT r' t m (DynRes r' t S.IntSet)
testResource = fmap (constDynRes . fmap S.singleton) $ allocate (pure <$> newResource)

testIntMap :: (ReplaceableResourceContext rp r t (ValidateAllocT m), MonadIO m, MonadIO (Performable m), Adjustable t m)
           => (forall r'. ResourceContext r r' => ValidateT r' t m (DynRes r' t S.IntSet))
           -> Event t Int
           -> ValidateT r t m (DynRes r t S.IntSet)
testIntMap f ev = do resultMap <- dynResIntMap (\_ _ -> f) (I.fromList [(0, ())]) (fmap mkPatch ev)
                     return $ fmap (I.foldr S.union S.empty) resultMap
    where mkPatch n | n < 0 = PatchIntMap $ I.fromList [(-n, Nothing)]
          mkPatch n = PatchIntMap $ I.fromList [(n, Just ())]

test :: (ReplaceableResourceContext rp r t (ValidateAllocT m), Monad m, MonadIO m, MonadIO (Performable m), MonadSample t (Performable m), Adjustable t m)
     => Event t Int
     -> Event t Int
     -> Event t Int
     -> ValidateT r t m (Res r (ValidateAllocT (Performable m) Bool))
test e0 e1 e2 = do resourcesToCheck <- testIntMap (testIntMap (testIntMap testResource e2) e1) e0
                   sampler <- dynResSampler resourcesToCheck
                   return $ fmap (\s -> s >>= \rids -> mapM checkResource (S.toAscList rids) >>= return . and) sampler

run :: (Adjustable t m, MonadHold t m, MonadFix m, PerformEvent t m, MonadSample t (Performable m), MonadIO (Performable m), MonadIO m)
    => Event t Bool
    -> (forall r. ResourceContext () r => ValidateT r t m (Res r (ValidateAllocT (Performable m) Bool)))
    -> m (Performable m (), Performable m ())
run exec act = do supply <- liftIO newSupply
                  (as, ds) <- runValidateAllocTIO $ do (cmd, _, as, ds) <- runResourceTContext supply act
                                                       performEvent_ (fmapCheap (\end -> if end then checkLeaks else checkSafe cmd) exec)
                                                       (as, ds) <- unwrapValidateAllocT $ \f -> (f as, fmap f ds)
                                                       return (as, ds)
                  return (as, join ds)
    where checkLeaks = noLeaks >>= flip when (error "leaks") . not
          checkSafe = (>>= flip when (error "tried to access unallocated resource") . not)

main :: IO ()
main = runSpiderHost $ do (e0, e0triggerRef) <- runHostFrame newEventWithTriggerRef
                          (e1, e1triggerRef) <- runHostFrame newEventWithTriggerRef
                          (e2, e2triggerRef) <- runHostFrame newEventWithTriggerRef
                          (validate, validatetriggerRef) <- runHostFrame newEventWithTriggerRef
                          ((as, des), FireCommand fire) <- hostPerformEventT (run validate (test e0 e1 e2))

                          runHostFrame as

                          validatetrigger <- liftIO . fmap fromJust $ readIORef validatetriggerRef
                          e2trigger <- liftIO . fmap fromJust $ readIORef e2triggerRef
                          e1trigger <- liftIO . fmap fromJust $ readIORef e1triggerRef
                          e0trigger <- liftIO . fmap fromJust $ readIORef e0triggerRef

                          fire [validatetrigger :=> Identity False] (return ())
                          let go 0 = return ()
                              go n = do trigger <- ([e0trigger, e1trigger, e2trigger] !!) <$> liftIO (randomRIO (0, 2))
                                        i <- liftIO $ randomRIO (-10, 10)
                                        fire [trigger :=> Identity i] (return ())
                                        fire [validatetrigger :=> Identity False] (return ())
                                        go (n - 1)
                          go 1000

                          runHostFrame des
                          fire [validatetrigger :=> Identity True] (return ())
                          return ()

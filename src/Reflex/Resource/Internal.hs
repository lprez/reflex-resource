{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, UndecidableInstances, PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo, RankNTypes, TypeApplications, TypeFamilies, StandaloneDeriving, ConstraintKinds, FunctionalDependencies, QuantifiedConstraints #-}

module Reflex.Resource.Internal where

import Control.Applicative
import Control.Concurrent.Supply
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Data.Maybe
import Data.Functor.Compose
import Data.Functor.Misc (weakenDMapWith, ComposeMaybe(..))
import qualified Data.Map as M
import qualified Data.Patch.MapWithMove as M
import qualified Data.IntMap as I
import qualified Data.IntMap.Merge.Lazy as I
import qualified Data.Patch.IntMap as I
import qualified Data.Dependent.Map as D
import qualified Data.Patch.DMapWithMove as D
import Data.GADT.Compare (GCompare)
import Data.Some (mkSome)
import qualified Data.Patch.Class as Patch
import Reflex.Class
import Reflex.Dynamic
import Reflex.Adjustable.Class
import Reflex.PerformEvent.Class

type RFrameId = Int

class (Monoid (Allocation m), Monad m) => MonadAllocate m where
    type family Allocation m
    allocateFrame :: RFrameId -> Allocation m -> m ()
    deallocateFrame :: RFrameId -> m ()

-- | A resource or set of resources guaranteed to exist within the context @r@. Use the 'Monad' instance
-- to combine different resources, use the 'UnRes' type to combine them with other values you can then
-- extract.
newtype Res r a = Res { unRes :: a }

-- | A wrapped non-resource.
newtype UnRes a = UnRes { unUnRes :: a }

-- | Cast a 'Res' from a parent context.
castRes :: ResourceContext rp r => Res rp a -> Res r a
castRes (Res x) = Res x

-- | Wrap a non-resource.
toRes :: a -> Res r (UnRes a)
toRes = Res . UnRes

-- | Unwrap a non-resource.
fromRes :: Res r (UnRes a) -> a
fromRes (Res (UnRes x)) = x

-- | Create an action that samples a 'Res'. 
resSampler :: (Monad m, Monad (Performable m)) => Res r a -> ResourceT r t m (Res r (Performable m a))
resSampler (Res x) = return . Res $ return x

-- | Wrap a resource in a temporary context.
tmpRes :: Res r a -> Res (TmpResourceContext r) a
tmpRes (Res x) = Res x

-- | Create an action that samples a temporary 'Res'. The action can only be used within 'allocateTmp'.
tmpResSampler :: (Monad m, Monad (Performable m)) => Res (TmpResourceContext r) a -> ResourceT r t m (Res (TmpResourceContext r) (Performable m a))
tmpResSampler (Res x) = return . Res $ return x

instance Functor (Res r) where
    fmap f (Res x) = Res (f x)

instance Applicative (Res r) where
    pure = Res
    Res f <*> Res x = Res (f x)

instance Monad (Res r) where
    Res x >>= f = f x

instance Eq a => Eq (Res r a) where
    Res x == Res y = x == y

-- | A switchable resource that is guaranteed to exist within the context @r@.
newtype DynRes r t a = DynRes { unDynRes :: Dynamic t a }

deriving instance Reflex t => Functor (DynRes r t)
deriving instance Reflex t => Applicative (DynRes r t)
deriving instance Reflex t => Monad (DynRes r t)

-- | Cast a 'DynRes' from a parent context.
castDynRes :: ResourceContext rp r => DynRes rp t a -> DynRes r t a
castDynRes (DynRes x) = DynRes x

-- | Create an action that samples a 'DynRes'. For this operation to be safe, the sampled value must be used
-- in the same Reflex frame in which it's retrieved. That is, the 'Performable' action must not, for instance,
-- save the value to an @IORef@ which is then read by an action run in a different frame.
dynResSampler :: (Monad m, Reflex t, MonadSample t (Performable m)) => DynRes r t a -> ResourceT r t m (Res r (Performable m a))
dynResSampler (DynRes x) = return . Res $ sample (current x)

dynResDyn :: Dynamic t a -> DynRes r t a
dynResDyn = DynRes

unwrapDynRes :: Res r (DynRes r t a) -> DynRes r t a
unwrapDynRes (Res x) = x

dynRes :: Reflex t => Dynamic t (Res r a) -> DynRes r t a
dynRes = DynRes . fmap unRes

toDynRes :: Reflex t => Dynamic t a -> DynRes r t (UnRes a)
toDynRes = DynRes . fmap UnRes

fromDynRes :: Reflex t => DynRes r t (UnRes a) -> Dynamic t a
fromDynRes (DynRes d) = unUnRes <$> d

withDynRes :: Reflex t => DynRes r t a -> (forall r'. Dynamic t (Res r' a) -> b) -> b
withDynRes (DynRes dyn) f = f $ Res <$> dyn

-- useless?
-- sampling :: (Reflex t, MonadSample t m) => DynRes r t a -> (forall r'. Res r' a -> b) -> m b
-- sampling (DynRes dyn) f = f . Res <$> sample (current dyn)

sampleUnRes :: (Reflex t, MonadSample t m) => DynRes r t (UnRes a) -> m a
sampleUnRes (DynRes dyn) = unUnRes <$> sample (current dyn)

holdDynRes :: (Reflex t, MonadHold t m) => Res r a -> Event t (Res r a) -> m (DynRes r t a)
holdDynRes (Res x) ev = DynRes <$> holdDyn x (unRes <$> ev)

accumDynRes :: (Reflex t, MonadHold t m, MonadFix m) => (forall r'. Res r' a -> b -> Res r' a) -> Res r a -> Event t b -> m (DynRes r t a)
accumDynRes f (Res a0) = fmap DynRes . accumDyn (\a -> unRes . f (Res a)) a0

accumMDynRes :: (Reflex t, MonadHold t m, MonadFix m) => (forall r'. Res r' a -> b -> PushM t (Res r' a)) -> Res r a -> Event t b -> m (DynRes r t a)
accumMDynRes f (Res a0) = fmap DynRes . accumMDyn (\a -> fmap unRes . f (Res a)) a0

mapAccumDynRes :: (Reflex t, MonadHold t m, MonadFix m) => (forall r'. Res r' a -> b -> ((Res r' a), c)) -> Res r a -> Event t b -> m (DynRes r t a, Event t c)
mapAccumDynRes f (Res a0) = fmap (\(d, e) -> (DynRes d, e)) . mapAccumDyn (\a -> (\(Res a', c) -> (a', c)) . f (Res a)) a0

mapAccumMDynRes :: (Reflex t, MonadHold t m, MonadFix m) => (forall r'. Res r' a -> b -> PushM t ((Res r' a), c)) -> Res r a -> Event t b -> m (DynRes r t a, Event t c)
mapAccumMDynRes f (Res a0) = fmap (\(d, e) -> (DynRes d, e)) . mapAccumMDyn (\a -> fmap (\(Res a', c) -> (a', c)) . f (Res a)) a0

mapAccumMaybeDynRes :: (Reflex t, MonadHold t m, MonadFix m) => (forall r'. Res r' a -> b -> (Maybe (Res r' a), Maybe c)) -> Res r a -> Event t b -> m (DynRes r t a, Event t c)
mapAccumMaybeDynRes f (Res a0) = fmap (\(d, e) -> (DynRes d, e)) . mapAccumMaybeDyn (\a -> (\(a', c) -> (fmap unRes a', c)) . f (Res a)) a0

mapAccumMaybeMDynRes :: (Reflex t, MonadHold t m, MonadFix m) => (forall r'. Res r' a -> b -> PushM t (Maybe (Res r' a), Maybe c)) -> Res r a -> Event t b -> m (DynRes r t a, Event t c)
mapAccumMaybeMDynRes f (Res a0) = fmap (\(d, e) -> (DynRes d, e)) . mapAccumMaybeMDyn (\a -> fmap (\(a', c) -> (fmap unRes a', c)) . f (Res a)) a0


data RFrame as pm t = RFrame { allocations :: as
                             , nestedAllocations :: pm ()
                             , nestedRFrames :: Behavior t [RFrameId] -- (to deallocate when the current RFrame is switched out)
                             }

newtype ResourceT r t m a = ResourceT { unResourceT :: StateT (Supply, RFrame (Allocation (Performable m)) (Performable m) t) m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

data InternalResourceContext r

-- | A temporary context. The resources allocated inside of it only exist while the resources in the
-- context @r@ are being allocated.
data TmpResourceContext r
data AnyResourceContext

class ResourceContext parent r | r -> parent
instance {-# OVERLAPPABLE #-} (ResourceContext r1 r2, ResourceContext r2 r3) => ResourceContext r1 r3
instance ResourceContext r (InternalResourceContext r)

instance MonadTrans (ResourceT r t) where
    lift = ResourceT . lift

instance MonadSample t m => MonadSample t (ResourceT r t m) where
    sample = lift . sample

instance MonadHold t m => MonadHold t (ResourceT r t m) where
    hold x = lift . hold x
    holdDyn x = lift . holdDyn x
    holdIncremental x = lift . holdIncremental x
    buildDynamic x = lift . buildDynamic x
    headE = lift . headE
    now = lift now

instance PerformEvent t m => PerformEvent t (ResourceT r t m) where
    type instance Performable (ResourceT r t m) = Performable m
    performEvent_ = lift . performEvent_
    performEvent = lift . performEvent

runRFrame :: (Monad m, Reflex t, Monad (Performable m), MonadAllocate (Performable m))
          => Supply
          -> ResourceT r t m a
          -> m (a, (Supply, (RFrameId, RFrame (Allocation (Performable m)) (Performable m) t)))
runRFrame supply r = do ((rid, x), (supply', rframe)) <- runStateT (unResourceT $ liftA2 (,) getFreshId r) (supply, emptyRFrame)
                        return (x, (supply', (rid, rframe)))
    where emptyRFrame = RFrame mempty (return ()) (constant [])

getFreshId :: Monad m => ResourceT r t m Int
getFreshId = ResourceT $ state (\(supply, rframe) -> let (rid, supply') = freshId supply in (rid, (supply', rframe)))

getNewSupply :: Monad m => ResourceT r t m Supply
getNewSupply = ResourceT $ state (\(supply, rframe) -> let (s1, s2) = splitSupply supply in (s1, (s2, rframe)))

-- | Run a ResourceT computation, returning the result of the computation, the initial allocations
-- and a sampling action that returns the final deallocations. @'Performable' m@ actions that access
-- resources must not be schedule before the allocating action or after the deallocating action.
runResourceT :: (Reflex t, Monad m, MonadAllocate (Performable m), MonadSample t m')
             => Supply
             -> ResourceT r t m a
             -> m (a, Supply, Performable m (), m' (Performable m ()))
runResourceT supply r = do (x, (supply', (rid, rframe))) <- runRFrame supply r
                           return ( x
                                  , supply'
                                  , allocateRFrame rid rframe
                                  , fmap (deallocateRFrame rid) (sample (nestedRFrames rframe))
                                  )

-- | Run a ResourceT computation in a 'ResourceContext', allowing you to return resources.
-- Resources referred to by the @Res@ value must not be used before the allocation or after the deallocation.
runResourceTContext :: (Reflex t, Monad m, MonadAllocate (Performable m), MonadSample t m')
                    => Supply
                    -> (forall r'. ResourceContext () r' => ResourceT r' t m (Res r' a))
                    -> m (a, Supply, Performable m (), m' (Performable m ()))
runResourceTContext s f = runResourceT s (unRes <$> f @(InternalResourceContext ()))


-- | Run a ResourceT computation in a context in which resources can be allocated.
resourceContext :: (forall r'. ResourceContext r r' => ResourceT r' t m a) -> ResourceT r t m a
resourceContext r = ResourceT (unResourceT (r @(InternalResourceContext _)))

fmapResourceContext :: Functor f => (forall r'. ResourceContext r r' => f (ResourceT r' t m a)) -> f (ResourceT r t m a)
fmapResourceContext r = fmap (\r' -> ResourceT (unResourceT r')) (r @(InternalResourceContext _))

-- | Same as 'resourceContext', but allows you to return resources wrapped in the 'Res' monad.
resourceContextRes :: Monad m => (forall r'. ResourceContext r r' => ResourceT r' t m (Res r' a)) -> ResourceT r t m (Res r a)
resourceContextRes r = ResourceT (unResourceT (Res . unRes <$> (r @(InternalResourceContext _))))

fmapResourceContextRes :: (Functor f, Monad m) => (forall r'. ResourceContext r r' => f (ResourceT r' t m (Res r' a))) -> f (ResourceT r t m (Res r a))
fmapResourceContextRes r = fmap (\r' -> ResourceT (unResourceT (Res . unRes <$> r'))) (r @(InternalResourceContext _))

-- | Combination of 'resourceContext' and 'resourceContextRes'.
resourceContextRes' :: Monad m => (forall r'. ResourceContext r r' => ResourceT r' t m (a, Res r' b)) -> ResourceT r t m (a, Res r b)
resourceContextRes' r = ResourceT (unResourceT (fmap (\(x, s) -> (x, Res $ unRes s))  (r @(InternalResourceContext _))))

fmapResourceContextRes' :: (Functor f, Monad m) => (forall r'. ResourceContext r r' => f (ResourceT r' t m (a, Res r' b))) -> f (ResourceT r t m (a, Res r b))
fmapResourceContextRes' r = fmap (\r' -> ResourceT (unResourceT (fmap (\(x, s) -> (x, Res $ unRes s)) r'))) (r @(InternalResourceContext _))

-- | Same as 'resourceContext', but allows you to return resources wrapped in the 'DynRes' monad.
resourceContextDynRes :: Monad m => (forall r'. ResourceContext r r' => ResourceT r' t m (DynRes r' t a)) -> ResourceT r t m (DynRes r t a)
resourceContextDynRes r = ResourceT (unResourceT (DynRes . unDynRes <$> (r @(InternalResourceContext _))))

fmapResourceContextDynRes :: (Functor f, Monad m) => (forall r'. ResourceContext r r' => f (ResourceT r' t m (DynRes r' t a))) -> f (ResourceT r t m (DynRes r t a))
fmapResourceContextDynRes r = fmap (\r' -> ResourceT (unResourceT (DynRes . unDynRes <$> r'))) (r @(InternalResourceContext _))

-- | Combination of 'resourceContext' and 'resourceContextDynRes'.
resourceContextDynRes' :: Monad m => (forall r'. ResourceContext r r' => ResourceT r' t m (a, DynRes r' t b)) -> ResourceT r t m (a, DynRes r t b)
resourceContextDynRes' r = ResourceT (unResourceT (fmap (\(x, s) -> (x, DynRes $ unDynRes s))  (r @(InternalResourceContext _))))

fmapResourceContextDynRes' :: (Functor f, Monad m) => (forall r'. ResourceContext r r' => f (ResourceT r' t m (a, DynRes r' t b))) -> f (ResourceT r t m (a, DynRes r t b))
fmapResourceContextDynRes' r = fmap (\r' -> ResourceT (unResourceT (fmap (\(x, s) -> (x, DynRes $ unDynRes s)) r'))) (r @(InternalResourceContext _))

performEventRes_ :: PerformEvent t m => Res r (Event t (Performable m ())) -> ResourceT r t m ()
performEventRes_ (Res x) = performEvent_ x

performEventRes :: PerformEvent t m => Res r (Event t (Performable m a)) -> ResourceT r t m (Event t a)
performEventRes (Res x) = performEvent x

allocateRFrame :: MonadAllocate m => RFrameId -> RFrame (Allocation m) m t -> m ()
allocateRFrame rid rframe = do nestedAllocations rframe
                               allocateFrame rid (allocations rframe)

deallocateRFrame :: MonadAllocate m => RFrameId -> [RFrameId] -> m ()
deallocateRFrame rid nestedRIds = do deallocateFrame rid
                                     mapM_ deallocateFrame nestedRIds

-- | Allocate a new resource.
allocate :: (ResourceContext rp r, MonadAllocate (Performable m), Monad m)
         => m (a, Res r (Allocation (Performable m)))
         -> ResourceT r t m (Res r a)
allocate mkAllocation = do (x, Res newAllocations) <- lift mkAllocation
                           ResourceT . modify $ \(supply, rframe) ->
                                let allocations' = allocations rframe <> newAllocations
                                in (supply, rframe { allocations = allocations' })
                           return (Res x)

-- | Allocate a new resource.
allocateTmp :: (ResourceContext rp r, MonadAllocate (Performable m), Monad m)
            => m (a, Res (TmpResourceContext r) (Allocation (Performable m)))
            -> ResourceT r t m (Res r a)
allocateTmp = allocate . fmap (\(x, Res a) -> (x, Res a))

-- | Get a unique identifier and allocate a new resource using it.
allocateFreshId :: (ResourceContext rp r, MonadAllocate (Performable m), Monad m)
                => (Int -> m (Res r (Allocation (Performable m))))
                -> ResourceT r t m (Res r Int)
allocateFreshId f = getFreshId >>= allocate . (\sid -> ((,) sid) <$> f sid)

-- | Get a unique identifier and allocate a new resource using it.
allocateFreshIdTmp :: (ResourceContext rp r, MonadAllocate (Performable m), Monad m)
                   => (Int -> m (Res (TmpResourceContext r) (Allocation (Performable m))))
                   -> ResourceT r t m (Res r Int)
allocateFreshIdTmp f = getFreshId >>= allocateTmp . (\sid -> ((,) sid) <$> f sid)

-- | Use a temporary context to allocate some resources.
withTmpContext :: (Reflex t, MonadSample t m, MonadSample t (Performable m), MonadAllocate (Performable m), ResourceContext rp r, PerformEvent t m)
               => (forall r' t'. ResourceContext r r' => ResourceT r' t' m (Res r' x))
                  -- ^ Context in which the temporary resources are created.
               -> (forall r'. ResourceContext r r' => Res (TmpResourceContext r') x -> ResourceT r' t m (Res r' b))
                  -- ^ Context in which the temporary resources are used.
               -> ResourceT r t m (Res r b)
withTmpContext t f = do supply <- fst <$> ResourceT get
                        (Res act, supply', as', mds') <- lift $ runResourceT supply (t @(InternalResourceContext _))
                        (Res x, (supply'', (rid', rframe'))) <- lift $ runRFrame supply' $ (f @(InternalResourceContext _)) (Res act)
                        ds' <- mds'
                        ownRid <- getFreshId
                        ResourceT . modify $ \(_, rframe) -> ( supply''
                                                             , rframe { allocations = mempty
                                                                      , nestedAllocations = do allocateRFrame ownRid rframe
                                                                                               as'
                                                                                               allocateRFrame rid' rframe'
                                                                                               ds'
                                                                      , nestedRFrames = do ownNestedRFrames <- nestedRFrames rframe
                                                                                           nestedRFrames' <- nestedRFrames rframe'
                                                                                           return $ rid' : nestedRFrames' ++ (ownRid : ownNestedRFrames)
                                                                      }
                                                             )
                        return (Res x)

unsafeRunWithReplace :: (Adjustable t m, MonadHold t m, MonadFix m, PerformEvent t m, MonadAllocate (Performable m))
                     => ResourceT r t m a
                     -> Event t (ResourceT r t m b)
                     -> ResourceT r t m (a, Event t b)
unsafeRunWithReplace r0 ev =
    do sInit0 <- getNewSupply
       rec ((x, state0@(_, (rid0, rframe0))), res') <- lift $ runWithReplace (runRFrame sInit0 r0)
                                                                             (uncurry runRFrame <$> attach supply' ev)
           (state', ev') <- mapAccumMB (\(_, (rid1, rframe1)) state2@(_, (rid2, rframe2)) ->
                                            do nested1 <- sample $ nestedRFrames rframe1
                                               return (state2, (do deallocateRFrame rid1 nested1
                                                                   allocateRFrame rid2 rframe2)))
                                       state0
                                       (fmapCheap snd res')
           let supply' = fmap fst state'
       performEvent_ ev'
       ownRid <- getFreshId
       ResourceT . modify $ \(supply, rframe) -> ( supply
                                                 , rframe { allocations = mempty
                                                          , nestedAllocations = do allocateRFrame ownRid rframe
                                                                                   allocateRFrame rid0 rframe0
                                                          , nestedRFrames = do ownNestedRFrames <- nestedRFrames rframe
                                                                               (_, (rid', rframe')) <- state'
                                                                               nestedRFrames' <- nestedRFrames rframe'
                                                                               return $ rid' : nestedRFrames' ++ (ownRid : ownNestedRFrames)
                                                          }
                                                 )
       return (x, fmap fst res')

type ReplaceableResourceContext rp r t m = (Adjustable t m, MonadHold t m, MonadFix m, PerformEvent t m, MonadAllocate (Performable m), ResourceContext rp r)

-- | Select between two actions based on the availability of a 'Res'.
ifThenElseRes :: ResourceContext rp r
              => Res r (Maybe a)
              -> (forall r'. ResourceContext r r' => Res r' a -> ResourceT r' t m b)
              -> (forall r'. ResourceContext r r' => ResourceT r' t m b)
              -> ResourceT r t m b
ifThenElseRes (Res (Just x)) y _ = resourceContext $ y (Res x)
ifThenElseRes (Res Nothing) _ n = resourceContext n

-- | Same as 'ifThenElseRes', but you can return resources wrapped in a 'Res'.
ifThenElseRes' :: (Monad m, ResourceContext rp r)
               => Res r (Maybe a)
               -> (forall r'. ResourceContext r r' => Res r' a -> ResourceT r' t m (b, Res r' c))
               -> (forall r'. ResourceContext r r' => ResourceT r' t m (b, Res r' c))
               -> ResourceT r t m (b, Res r c)
ifThenElseRes' (Res (Just x)) y _ = resourceContextRes' $ y (Res x)
ifThenElseRes' (Res Nothing) _ n = resourceContextRes' n

-- | 'ifThenElseRes' without an alternative action.
whenRes :: (Monad m, ResourceContext rp r)
        => Res r (Maybe a)
        -> (forall r'. ResourceContext r r' => Res r' a -> ResourceT r' t m b)
        -> ResourceT r t m (Maybe b)
whenRes res f = ifThenElseRes res (fmap Just . f) (return Nothing)

-- | 'ifThenElseRes\'' without an alternative action.
whenRes' :: (Monad m, ResourceContext rp r)
         => Res r (Maybe a)
         -> (forall r'. ResourceContext r r' => Res r' a -> ResourceT r' t m (b, Res r' c))
         -> ResourceT r t m (Maybe b, Res r (Maybe c))
whenRes' res f = ifThenElseRes' res (fmap (\(a, b) -> (Just a, fmap Just b)) . f) (return (Nothing, pure Nothing))

-- | Switch between two actions based on the availability of a 'DynRes'. Note that this will continuosly allocate
-- and deallocate the internal resources every time the @DynRes@ switches between @Just@ and @Nothing@. If you don't want this,
-- allocate them outside of the @ifThenElseDynRes@ call.
ifThenElseDynRes :: ReplaceableResourceContext rp r t m
                 => DynRes r t (Maybe a)
                 -> (forall r'. ResourceContext r r' => DynRes r' t a -> ResourceT r' t m b)
                 -> (forall r'. ResourceContext r r' => ResourceT r' t m b)
                 -> ResourceT r t m (Dynamic t b)
ifThenElseDynRes (DynRes dyn) y n = do mdyn <- maybeDyn dyn
                                       initial <- sample (current mdyn)
                                       (m0, m') <- runWithReplaceContext (maybe n (y . DynRes) initial)
                                                                         (maybe n (y . DynRes) <$> updated mdyn)
                                       holdDyn m0 m'

-- | Same as 'ifThenElseDynRes', but you can return resources wrapped in a 'DynRes'.
ifThenElseDynRes' :: ReplaceableResourceContext rp r t m
                  => DynRes r t (Maybe a)
                  -> (forall r'. ResourceContext r r' => DynRes r' t a -> ResourceT r' t m (b, DynRes r' t c))
                  -> (forall r'. ResourceContext r r' => ResourceT r' t m (b, DynRes r' t c))
                  -> ResourceT r t m (Dynamic t b, DynRes r t c)
ifThenElseDynRes' (DynRes dyn) y n = do mdyn <- maybeDyn dyn
                                        initial <- sample (current mdyn)
                                        (m0, m', d) <- runWithReplaceDynRes' (maybe n (y . DynRes) initial)
                                                                             (maybe n (y . DynRes) <$> updated mdyn)
                                        m <- holdDyn m0 m'
                                        return (m, d)


-- | 'ifThenElseDynRes' without an alternative action.
whenDynRes :: ReplaceableResourceContext rp r t m
           => DynRes r t (Maybe a)
           -> (forall r'. ResourceContext r r' => DynRes r' t a -> ResourceT r' t m b)
           -> ResourceT r t m (Dynamic t (Maybe b))
whenDynRes dyn f = ifThenElseDynRes dyn (fmap Just . f) (return Nothing)

-- | 'ifThenElseDynRes\'' without an alternative action.
whenDynRes' :: ReplaceableResourceContext rp r t m
            => DynRes r t (Maybe a)
            -> (forall r'. ResourceContext r r' => DynRes r' t a -> ResourceT r' t m (b, DynRes r' t c))
            -> ResourceT r t m (Dynamic t (Maybe b), DynRes r t (Maybe c))
whenDynRes' dyn f = ifThenElseDynRes' dyn (fmap (\(a, b) -> (Just a, fmap Just b)) . f) (return (Nothing, pure Nothing))

withDynResReplace :: ReplaceableResourceContext rp r t m
                  => DynRes r t a
                  -> (forall r'. ResourceContext r r' => Res r' a -> ResourceT r' t m (Res r' b))
                  -> ResourceT r t m (DynRes r t b)
withDynResReplace (DynRes dyn) f = sample (current dyn) >>= \dyn0 -> runWithReplaceRes (f (Res dyn0)) (f . Res <$> updated dyn)

withDynResReplace' :: ReplaceableResourceContext rp r t m
                   => DynRes r t a
                   -> (forall r'. ResourceContext r r' => Res r' a -> ResourceT r' t m (b, Res r' c))
                   -> ResourceT r t m (b, Event t b, DynRes r t c)
withDynResReplace' (DynRes dyn) f = sample (current dyn) >>= \dyn0 -> runWithReplaceRes' (f (Res dyn0)) (f . Res <$> updated dyn)

-- | This is a version of 'runWithReplace' that can be used inside of a resource context and can allocate internal
-- resources.
runWithReplaceContext :: ReplaceableResourceContext rp r t m
                      => (forall r'. ResourceContext r r' => ResourceT r' t m a)
                      -> (forall r'. ResourceContext r r' => Event t (ResourceT r' t m b))
                      -> ResourceT r t m (a, Event t b)
runWithReplaceContext r0 ev = unsafeRunWithReplace (resourceContext r0) (fmapResourceContext ev)

-- | Same as 'runWithReplaceContext', but in this case you can return the internal resources in the form of a 'DynRes'.
runWithReplaceRes :: ReplaceableResourceContext rp r t m
                  => (forall r'. ResourceContext r r' => ResourceT r' t m (Res r' a))
                  -> (forall r'. ResourceContext r r' => Event t (ResourceT r' t m (Res r' a)))
                  -> ResourceT r t m (DynRes r t a)
runWithReplaceRes r0 ev = unsafeRunWithReplace (resourceContextRes r0) (fmapResourceContextRes ev) >>= \(res0, res') -> (holdDynRes res0 res')

-- | Combination of 'runWithReplaceContext' and 'runWithReplaceRes'.
runWithReplaceRes' :: ReplaceableResourceContext rp r t m
                   => (forall r'. ResourceContext r r' => ResourceT r' t m (a, Res r' c))
                   -> (forall r'. ResourceContext r r' => Event t (ResourceT r' t m (b, Res r' c)))
                   -> ResourceT r t m (a, Event t b, DynRes r t c)
runWithReplaceRes' r0 ev = do ((x0, s0), res') <- unsafeRunWithReplace (resourceContextRes' r0) (fmapResourceContextRes' ev)
                              s <- (holdDynRes s0 (fmapCheap snd res'))
                              return (x0, fmap fst res', s)

-- | Same as 'runWithReplaceRes', but in this case the resources to be returned are already 'DynRes'.
runWithReplaceDynRes :: ReplaceableResourceContext rp r t m
                     => (forall r'. ResourceContext r r' => ResourceT r' t m (DynRes r' t a))
                     -> (forall r'. ResourceContext r r' => Event t (ResourceT r' t m (DynRes r' t a)))
                     -> ResourceT r t m (DynRes r t a)
runWithReplaceDynRes r0 ev = unsafeRunWithReplace (resourceContextDynRes r0) (fmapResourceContextDynRes ev) >>= \(res0, res') -> join <$> holdDynRes (pure res0) (fmapCheap pure res')

-- | 'runWithReplaceDynRes'
dynResReplace :: ReplaceableResourceContext rp r t m
              => (forall r'. ResourceContext r r' => ResourceT r' t m (DynRes r' t a))
              -> (forall r'. ResourceContext r r' => Event t (ResourceT r' t m (DynRes r' t a)))
              -> ResourceT r t m (DynRes r t a)
dynResReplace = runWithReplaceDynRes

-- | Combination of 'runWithReplaceContext' and 'runWithReplaceDynRes'.
runWithReplaceDynRes' :: ReplaceableResourceContext rp r t m
                      => (forall r'. ResourceContext r r' => ResourceT r' t m (a, DynRes r' t c))
                      -> (forall r'. ResourceContext r r' => Event t (ResourceT r' t m (b, DynRes r' t c)))
                      -> ResourceT r t m (a, Event t b, DynRes r t c)
runWithReplaceDynRes' r0 ev = do ((x0, s0), res') <- unsafeRunWithReplace (resourceContextDynRes' r0) (fmapResourceContextDynRes' ev)
                                 s <- join <$> holdDynRes (pure s0) (fmapCheap (pure . snd) res')
                                 return (x0, fmap fst res', s)

unsafeTraverseIntMapWithKeyWithAdjust :: (Adjustable t m, MonadHold t m, MonadFix m, PerformEvent t m, MonadAllocate (Performable m))
                                      => (I.Key -> v -> ResourceT r t m v')
                                      -> I.IntMap v
                                      -> Event t (I.PatchIntMap v)
                                      -> ResourceT r t m (I.IntMap v', Event t (I.PatchIntMap v'))
unsafeTraverseIntMapWithKeyWithAdjust f im0 im' =
    do sInit0 <- getNewSupply
       let (sExtra, sim0) = I.mapAccum (\s v -> let (s', se) = splitSupply s in (se, (s', v))) sInit0 im0
       rec (resm0, resm') <- lift $ traverseIntMapWithKeyWithAdjust (\k (s, v) -> runRFrame s $ f k v) sim0 (fmap PatchIntMap sim')
           let statem0 = fmap snd resm0
           (statem', ev') <- mapAccumMB (\states (PatchIntMap patch) ->
                                    do (states', (as, ds)) <- flip runStateT (return (), return ()) $
                                                    I.mergeA I.preserveMissing
                                                             (I.traverseMaybeMissing $ \_ intPatch ->
                                                                       case intPatch of
                                                                            Just state2@(_, (rid2, rframe2)) ->
                                                                                 do modify $ \(as, ds) -> (as >> allocateRFrame rid2 rframe2, ds)
                                                                                    return (Just state2)
                                                                            Nothing -> return Nothing)
                                                             (I.zipWithMaybeAMatched $ \_ (_, (rid1, rframe1)) intPatch ->
                                                                       do nested1 <- lift . sample $ nestedRFrames rframe1
                                                                          case intPatch of
                                                                               Just state2@(_, (rid2, rframe2)) ->
                                                                                  do modify $ \(as, ds) -> ( as >> allocateRFrame rid2 rframe2
                                                                                                           , deallocateRFrame rid1 nested1 >> ds )
                                                                                     return (Just state2)
                                                                               Nothing ->
                                                                                  do modify $ \(as, ds) -> (as, deallocateRFrame rid1 nested1 >> ds)
                                                                                     return Nothing)
                                                             states patch
                                       return (states', ds >> as)
                                            ) statem0 (fmap (fmap snd) resm')
           sim' <- mapAccumM_ (\se0 (PatchIntMap patchm) -> sample statem' >>= \statemCurrent -> return $
                        I.mapAccumWithKey (\se k p -> case (I.lookup k statemCurrent, se, p) of
                                                           (Nothing, _, Nothing) -> (se, Nothing)
                                                           (Just (s, _), _, Nothing) | length se < 100 -> (s : se, Nothing)
                                                                                                | otherwise -> (se, Nothing)
                                                           (Nothing, s : se'@(_ : _), Just v) -> (se', Just (s, v))
                                                           (Nothing, s : [], Just v) -> let (s', se') = splitSupply s in ([se'], Just (s', v))
                                                           (Nothing, [], Just _) -> error "supply"
                                                           (Just (s, _), _, Just v) -> (se, Just (s, v)))
                                          se0 patchm)
                                [sExtra] im'
                                           
       performEvent_ ev'
       ownRid <- getFreshId
       ResourceT . modify $ \(supply, rframe) -> ( supply
                                                 , rframe { allocations = mempty
                                                          , nestedAllocations = do allocateRFrame ownRid rframe
                                                                                   mapM_ (\(_, (rid0, rframe0)) -> allocateRFrame rid0 rframe0)
                                                                                         statem0
                                                          , nestedRFrames = do ownNestedRFrames <- nestedRFrames rframe
                                                                               states' <- fmap I.elems statem'
                                                                               nestedRFrames' <- mapM (\(_, (rid', rframe')) ->
                                                                                                        fmap (rid' :) $ nestedRFrames rframe')
                                                                                                      states'
                                                                               return $ concat nestedRFrames' ++ (ownRid : ownNestedRFrames)
                                                          }
                                                 )

       return (fmap fst resm0, fmap (fmap fst) resm')

traverseIntMapWithKeyWithAdjustContext :: ReplaceableResourceContext rp r t m
                                       => (forall r'. ResourceContext r r' => I.Key -> v -> ResourceT r' t m v')
                                       -> I.IntMap v
                                       -> Event t (I.PatchIntMap v)
                                       -> ResourceT r t m (I.IntMap v', Event t (I.PatchIntMap v'))
traverseIntMapWithKeyWithAdjustContext f = unsafeTraverseIntMapWithKeyWithAdjust (\k v -> resourceContext (f k v))

traverseIntMapWithKeyWithAdjustRes :: ReplaceableResourceContext rp r t m
                                   => (forall r'. ResourceContext r r' => I.Key -> v -> ResourceT r' t m (Res r' v'))
                                   -> I.IntMap v
                                   -> Event t (I.PatchIntMap v)
                                   -> ResourceT r t m (DynRes r t (I.IntMap v'))
traverseIntMapWithKeyWithAdjustRes f im0 im' = do (resm0, resm') <- unsafeTraverseIntMapWithKeyWithAdjust (\k v -> fmap unRes $ resourceContextRes (f k v)) im0 im'
                                                  accumDynRes (\rm0 patch -> fromMaybe (unRes rm0) . Patch.apply patch <$> rm0)
                                                              (Res resm0) resm'

traverseIntMapWithKeyWithAdjustRes' :: ReplaceableResourceContext rp r t m
                                    => (forall r'. ResourceContext r r' => I.Key -> v -> ResourceT r' t m (v', Res r' vr'))
                                    -> I.IntMap v
                                    -> Event t (I.PatchIntMap v)
                                    -> ResourceT r t m (I.IntMap v', Event t (PatchIntMap v'), DynRes r t (I.IntMap vr'))
traverseIntMapWithKeyWithAdjustRes' f im0 im' = do (resm0, resm') <- unsafeTraverseIntMapWithKeyWithAdjust (\k v -> fmap (\(v', Res r) -> (v', r)) $ resourceContextRes' (f k v)) im0 im'
                                                   dynres <- accumDynRes (\rm0 patch -> fromMaybe (unRes rm0) . Patch.apply patch <$> rm0)
                                                                         (Res (snd <$> resm0)) (fmap snd <$> resm')
                                                   return (fst <$> resm0, fmap fst <$> resm', dynres)

traverseIntMapWithKeyWithAdjustDynRes :: ReplaceableResourceContext rp r t m
                                      => (forall r'. ResourceContext r r' => I.Key -> v -> ResourceT r' t m (DynRes r' t v'))
                                      -> I.IntMap v
                                      -> Event t (I.PatchIntMap v)
                                      -> ResourceT r t m (DynRes r t (I.IntMap v'))
traverseIntMapWithKeyWithAdjustDynRes f im0 im' = do (resm0, resm') <- unsafeTraverseIntMapWithKeyWithAdjust (\k v -> fmap unDynRes $ resourceContextDynRes (f k v)) im0 im'
                                                     DynRes . joinDynThroughIntMap <$> accumDyn (\rm0 patch -> fromMaybe rm0 $ Patch.apply patch rm0)
                                                                                                resm0 resm'
-- | 'traverseIntMapWithKeyWithAdjustDynRes'
dynResIntMap :: ReplaceableResourceContext rp r t m
             => (forall r'. ResourceContext r r' => I.Key -> v -> ResourceT r' t m (DynRes r' t v'))
             -> I.IntMap v
             -> Event t (I.PatchIntMap v)
             -> ResourceT r t m (DynRes r t (I.IntMap v'))
dynResIntMap = traverseIntMapWithKeyWithAdjustDynRes

traverseIntMapWithKeyWithAdjustDynRes' :: ReplaceableResourceContext rp r t m
                                       => (forall r'. ResourceContext r r' => I.Key -> v -> ResourceT r' t m (v', DynRes r' t vr'))
                                       -> I.IntMap v
                                       -> Event t (I.PatchIntMap v)
                                       -> ResourceT r t m (I.IntMap v', Event t (PatchIntMap v'), DynRes r t (I.IntMap vr'))
traverseIntMapWithKeyWithAdjustDynRes' f im0 im' = do (resm0, resm') <- unsafeTraverseIntMapWithKeyWithAdjust (\k v -> fmap (\(v', DynRes r) -> (v', r)) $ resourceContextDynRes' (f k v))
                                                                                                              im0 im'
                                                      dynres <- DynRes . joinDynThroughIntMap <$>
                                                                        accumDyn (\rm0 patch -> fromMaybe rm0 $ Patch.apply patch rm0)
                                                                                 (snd <$> resm0) (fmap snd <$> resm')
                                                      return (fst <$> resm0, fmap fst <$> resm', dynres)

newtype RunRFrameResult t m v a = RunRFrameResult (v a, (Supply, (RFrameId, RFrame (Allocation m) m t)))
newtype RunRFrameArgument v a = RunRFrameArgument (Supply, v a)
newtype DMapResResult v vr a = DMapResResult (v a, vr a)

fstDMapResResult :: DMapResResult v vr a -> v a
fstDMapResResult (DMapResResult (v, _)) = v

sndDMapResResult :: DMapResResult v vr a -> vr a
sndDMapResResult (DMapResResult (_, r)) = r

unsafeTraverseDMapWithKeyWithAdjustWithMove :: (Adjustable t m, MonadHold t m, MonadFix m, PerformEvent t m, MonadAllocate (Performable m), GCompare k)
                                            => (forall a. k a -> v a -> ResourceT r t m (v' a))
                                            -> D.DMap k v
                                            -> Event t (D.PatchDMapWithMove k v)
                                            -> ResourceT r t m (D.DMap k v', Event t (D.PatchDMapWithMove k v'))
unsafeTraverseDMapWithKeyWithAdjustWithMove f im0 im' =
    do sInit0 <- getNewSupply
       let (sExtra, sim0) = D.mapAccumLWithKey (\s _ v -> let (s', se) = splitSupply s in (se, RunRFrameArgument (s', v))) sInit0 im0
       rec (resm0, resm') <- lift $ traverseDMapWithKeyWithAdjustWithMove (\k (RunRFrameArgument (s, v)) -> fmap RunRFrameResult . runRFrame s $ f k v)
                                                                          sim0 sim'
           let statem0 = weakenDMapWith (\(RunRFrameResult (_, s)) -> s) resm0
               patchStatem' = weakenPatchDMapWithMoveWith (\(RunRFrameResult (_, s)) -> s) <$> resm'
           (statem', ev') <- mapAccumMB (\states wpatch@(M.PatchMapWithMove patch) ->
                                    do (as, ds) <- flip execStateT (return (), return ()) $ flip M.traverseWithKey patch $
                                            \k p@(M.NodeInfo from to) -> case (M.lookup k states, from, to) of
                                                                            (Just (_, (rid1, rframe1)), M.From_Delete, Nothing) ->
                                                                                do nested1 <- lift . sample $ nestedRFrames rframe1
                                                                                   modify $ \(as, ds) -> (as, deallocateRFrame rid1 nested1 >> ds)
                                                                                   return p
                                                                            (Just (_, (rid1, rframe1)), M.From_Insert (_, (rid2, rframe2)), _) ->
                                                                                do nested1 <- lift . sample $ nestedRFrames rframe1
                                                                                   modify $ \(as, ds) -> ( as >> allocateRFrame rid2 rframe2
                                                                                                         , deallocateRFrame rid1 nested1 >> ds )
                                                                                   return p
                                                                            (Nothing, M.From_Insert (_, (rid2, rframe2)), _) ->
                                                                                do modify $ \(as, ds) -> (as >> allocateRFrame rid2 rframe2, ds)
                                                                                   return p
                                                                            _ -> return p
                                       return (fromMaybe states $ Patch.apply wpatch states, ds >> as)
                                            ) statem0 patchStatem'
           sim' <- mapAccumM_ (\se0 (D.PatchDMapWithMove patchm) -> sample statem' >>= \statemCurrent -> return . (\(s, m) -> (s, D.PatchDMapWithMove m)) $
                        D.mapAccumRWithKey (\se k (D.NodeInfo from to) ->
                                                case (M.lookup (mkSome k) statemCurrent, se, from) of
                                                     (Nothing, _, D.From_Delete) -> (se, D.NodeInfo D.From_Delete to)
                                                     (Just (s, _), _, D.From_Delete) | ComposeMaybe (Just _) <- to -> (se, D.NodeInfo D.From_Delete to)
                                                                                     | length se < 100 -> (s : se, D.NodeInfo D.From_Delete to)
                                                                                     | otherwise -> (se, D.NodeInfo D.From_Delete to)
                                                     (Nothing, s : se'@(_ : _), D.From_Insert v) -> (se', D.NodeInfo (D.From_Insert $ RunRFrameArgument (s, v)) to)
                                                     (Nothing, s : [], D.From_Insert v) -> let (s', se') = splitSupply s
                                                                                           in ([se'], D.NodeInfo (D.From_Insert (RunRFrameArgument (s', v))) to)
                                                     (Nothing, [], D.From_Insert _) -> error "supply"
                                                     (Just (s, _), _, D.From_Insert v) -> (se, D.NodeInfo (D.From_Insert $ RunRFrameArgument (s, v)) to)
                                                     (Nothing, _, D.From_Move k') -> (se, D.NodeInfo (D.From_Move k') to)
                                                     (Just _, _, D.From_Move k') -> (se, D.NodeInfo (D.From_Move k') to)
                                                     -- (Just (s, _), _, D.From_Move k') -> (s : se, M.NodeInfo (M.From_Move k') to)
                                           ) se0 patchm)
                                [sExtra] im'
                                           
       performEvent_ ev'
       ownRid <- getFreshId
       ResourceT . modify $ \(supply, rframe) -> ( supply
                                                 , rframe { allocations = mempty
                                                          , nestedAllocations = do allocateRFrame ownRid rframe
                                                                                   _ <- M.traverseWithKey (\_ (_, (rid0, rframe0)) -> allocateRFrame rid0 rframe0)
                                                                                                          statem0
                                                                                   return ()
                                                          , nestedRFrames = do ownNestedRFrames <- nestedRFrames rframe
                                                                               states' <- M.elems <$> statem'
                                                                               nestedRFrames' <- mapM (\(_, (rid', rframe')) ->
                                                                                                        fmap (rid' :) $ nestedRFrames rframe')
                                                                                                      states'
                                                                               return $ concat nestedRFrames' ++ (ownRid : ownNestedRFrames)
                                                          }
                                                 )

       return ( D.map (\(RunRFrameResult (x, _)) -> x) resm0
              , fmap (D.mapPatchDMapWithMove (\(RunRFrameResult (x, _)) -> x)) resm'
              )

traverseDMapWithKeyWithAdjustWithMoveContext :: (ReplaceableResourceContext rp r t m, GCompare k)
                                             => (forall r' a. ResourceContext r r' => k a -> v a -> ResourceT r' t m (v' a))
                                             -> D.DMap k v
                                             -> Event t (D.PatchDMapWithMove k v)
                                             -> ResourceT r t m (D.DMap k v', Event t (D.PatchDMapWithMove k v'))
traverseDMapWithKeyWithAdjustWithMoveContext f = unsafeTraverseDMapWithKeyWithAdjustWithMove (\k v -> resourceContext (f k v))

traverseDMapWithKeyWithAdjustWithMoveRes :: (ReplaceableResourceContext rp r t m, GCompare k)
                                         => (forall r' a. ResourceContext r r' => k a -> v a -> ResourceT r' t m (Res r' (v' a)))
                                         -> D.DMap k v
                                         -> Event t (D.PatchDMapWithMove k v)
                                         -> ResourceT r t m (DynRes r t (D.DMap k v'))
traverseDMapWithKeyWithAdjustWithMoveRes f im0 im' = do (resm0, resm') <- unsafeTraverseDMapWithKeyWithAdjustWithMove (\k v -> fmap unRes $ resourceContextRes (f k v)) im0 im'
                                                        accumDynRes (\rm0 patch -> fromMaybe (unRes rm0) . Patch.apply patch <$> rm0)
                                                                    (Res resm0) resm'

traverseDMapWithKeyWithAdjustWithMoveRes' :: (ReplaceableResourceContext rp r t m, GCompare k)
                                          => (forall r' a. ResourceContext r r' => k a -> v a -> ResourceT r' t m (v' a, Res r' (vr' a)))
                                          -> D.DMap k v
                                          -> Event t (D.PatchDMapWithMove k v)
                                          -> ResourceT r t m (D.DMap k v', Event t (D.PatchDMapWithMove k v'), DynRes r t (D.DMap k vr'))
traverseDMapWithKeyWithAdjustWithMoveRes' f im0 im' = do (resm0, resm') <- unsafeTraverseDMapWithKeyWithAdjustWithMove (\k v -> fmap (\(v', Res r) -> DMapResResult (v', r)) $ resourceContextRes' (f k v)) im0 im'
                                                         dynres <- accumDynRes (\rm0 patch -> fromMaybe (unRes rm0) . Patch.apply patch <$> rm0)
                                                                               (Res (D.map sndDMapResResult resm0)) (D.mapPatchDMapWithMove sndDMapResResult <$> resm')
                                                         return (D.map fstDMapResResult resm0, D.mapPatchDMapWithMove fstDMapResResult <$> resm', dynres)

traverseDMapWithKeyWithAdjustWithMoveDynRes :: (ReplaceableResourceContext rp r t m, GCompare k)
                                            => (forall r' a. ResourceContext r r' => k a -> v a -> ResourceT r' t m (DynRes r' t (v' a)))
                                            -> D.DMap k v
                                            -> Event t (D.PatchDMapWithMove k v)
                                            -> ResourceT r t m (DynRes r t (D.DMap k v'))
traverseDMapWithKeyWithAdjustWithMoveDynRes f im0 im' = do (resm0, resm') <- unsafeTraverseDMapWithKeyWithAdjustWithMove (\k v -> fmap (Compose . unDynRes) $ resourceContextDynRes (f k v)) im0 im'
                                                           DynRes . (>>= distributeDMapOverDynPureG getCompose) <$> accumDyn (\rm0 patch -> fromMaybe rm0 $ Patch.apply patch rm0)
                                                                                                                             resm0 resm'
-- | 'traverseDMapWithKeyWithAdjustWithMoveDynRes'
dynResDMap :: (ReplaceableResourceContext rp r t m, GCompare k)
           => (forall r' a. ResourceContext r r' => k a -> v a -> ResourceT r' t m (DynRes r' t (v' a)))
           -> D.DMap k v
           -> Event t (D.PatchDMapWithMove k v)
           -> ResourceT r t m (DynRes r t (D.DMap k v'))
dynResDMap = traverseDMapWithKeyWithAdjustWithMoveDynRes


traverseDMapWithKeyWithAdjustWithMoveDynRes' :: (ReplaceableResourceContext rp r t m, GCompare k)
                                             => (forall r' a. ResourceContext r r' => k a -> v a -> ResourceT r' t m (v' a, DynRes r' t (vr' a)))
                                             -> D.DMap k v
                                             -> Event t (D.PatchDMapWithMove k v)
                                             -> ResourceT r t m (D.DMap k v', Event t (D.PatchDMapWithMove k v'), DynRes r t (D.DMap k vr'))
traverseDMapWithKeyWithAdjustWithMoveDynRes' f im0 im' = do (resm0, resm') <- unsafeTraverseDMapWithKeyWithAdjustWithMove (\k v -> fmap (\(v', DynRes r) -> DMapResResult (v', Compose r)) $ resourceContextDynRes' (f k v))
                                                                                                                          im0 im'
                                                            dynres <- DynRes . (>>= distributeDMapOverDynPureG getCompose) <$>
                                                                              accumDyn (\rm0 patch -> fromMaybe rm0 $ Patch.apply patch rm0)
                                                                                       (D.map sndDMapResResult resm0) (D.mapPatchDMapWithMove sndDMapResResult <$> resm')
                                                            return (D.map fstDMapResResult resm0, D.mapPatchDMapWithMove fstDMapResResult <$> resm', dynres)

instance (MonadHold t m, MonadFix m, PerformEvent t m, MonadAllocate (Performable m), Adjustable t m) => Adjustable t (ResourceT () t m) where
    runWithReplace = unsafeRunWithReplace
    traverseIntMapWithKeyWithAdjust = unsafeTraverseIntMapWithKeyWithAdjust
    traverseDMapWithKeyWithAdjustWithMove = unsafeTraverseDMapWithKeyWithAdjustWithMove

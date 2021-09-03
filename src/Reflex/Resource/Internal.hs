{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, UndecidableInstances, PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo, RankNTypes, TypeApplications, TypeFamilies, StandaloneDeriving, ConstraintKinds, FunctionalDependencies, QuantifiedConstraints #-}
{-# LANGUAGE EmptyDataDecls #-}

module Reflex.Resource.Internal where

import Control.Applicative
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

import Reflex.Resource.Allocate (MonadAllocate(..))

-- | A handle/reference to a resource or set of resources guaranteed to exist within the context @r@, or
-- any sort of value referring to those resources. Use the 'Monad' instance to combine different resources,
-- use the 'UnRes' type to combine them with other pure values you can then extract.
newtype Res r a = Res { unRes :: a }

-- | A wrapped pure value.
newtype UnRes a = UnRes { unUnRes :: a }

-- | Cast a 'Res' from a parent context.
castRes :: ResourceContext rp r => Res rp a -> Res r a
castRes (Res x) = Res x

-- | Wrap a pure value inside of a 'Res'/'UnRes'. The value can't be modified but it can be
-- extracted from the 'Res'. Use 'pure' if you don't need to extract it.
toRes :: a -> Res r (UnRes a)
toRes = Res . UnRes

-- | Unwrap a 'Res'/'UnRes'.
fromRes :: Res r (UnRes a) -> a
fromRes (Res (UnRes x)) = x

-- | Map a function to a wrapped 'UnRes'.
mapUnRes :: (a -> b) -> Res r (UnRes a) -> Res r (UnRes b)
mapUnRes f (Res (UnRes x)) = Res (UnRes (f x))

-- | Cast a 'Res' to a temporary context.
tmpRes :: Res r a -> Res (TmpResourceContext r) a
tmpRes (Res x) = Res x

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

-- | Create an action that samples a 'DynRes'.
dynResSampler :: (Monad m, Reflex t, MonadSample t pm) => DynRes r t a -> ResourceT' r t pm m (Res r (pm a))
dynResSampler (DynRes x) = return . Res $ sample (current x)

-- | Create a 'DynRes' from a pure 'Dynamic'. Use 'toDynRes' if you want to
-- be able to unwrap the @Dynamic@.
dynResDyn :: Dynamic t a -> DynRes r t a
dynResDyn = DynRes

unwrapDynRes :: Res r (DynRes r t a) -> DynRes r t a
unwrapDynRes (Res x) = x

-- | Switch between the resources contained in a 'Dynamic'.
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


data RFrame pm m t = RFrame { allocations :: Allocation pm m    -- ^ Pending allocations.
                            , initialization :: pm ()           -- ^ Actions to perform to initialize this frame.
                            , finalization :: Behavior t (pm ())
                            , liftPerformable :: (forall a. pm a -> Performable m a)
                            }

newtype ResourceT' r t pm m a = ResourceT { unResourceT :: StateT (RFrame pm m t) m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

type ResourceT r t m = ResourceT' r t (Performable m) m

data InternalResourceContext r

-- | A temporary context. The resources allocated inside of it only exist while the resources in the
-- context @r@ are being initialized.
data TmpResourceContext r

-- | A scope in which resources can be allocated.
class ResourceContext parent r | r -> parent
instance {-# OVERLAPPABLE #-} (ResourceContext r1 r2, ResourceContext r2 r3) => ResourceContext r1 r3
instance ResourceContext r (InternalResourceContext r)

instance MonadTrans (ResourceT' r t pm) where
    lift = ResourceT . lift

hoistResourceT :: (Allocation pm m ~ Allocation pm n, Functor n)
               => (forall x. pm x -> Performable m x)
               -> (forall x. m x -> n x)
               -> ResourceT' r t pm m a
               -> ResourceT' r t pm n a
hoistResourceT liftp f (ResourceT m) = ResourceT . StateT $ \rframe ->
    fmap (\(x, rframe') -> (x, rframe' { liftPerformable = liftPerformable rframe, allocations = allocations rframe' }))
         (f $ runStateT m (rframe { liftPerformable = liftp, allocations = allocations rframe }))

instance MonadSample t m => MonadSample t (ResourceT' r t pm m) where
    sample = lift . sample

instance MonadHold t m => MonadHold t (ResourceT' r t pm m) where
    hold x = lift . hold x
    holdDyn x = lift . holdDyn x
    holdIncremental x = lift . holdIncremental x
    buildDynamic x = lift . buildDynamic x
    headE = lift . headE
    now = lift now

instance PerformEvent t m => PerformEvent t (ResourceT' r t pm m) where
    type instance Performable (ResourceT' r t pm m) = Performable m
    performEvent_ = lift . performEvent_
    performEvent = lift . performEvent

runRFrame :: (Monad m, Reflex t, Monad pm, MonadAllocate pm m)
          => (forall x. pm x -> Performable m x)
          -> ResourceT' r t pm m a
          -> m (a, (pm (), Behavior t (pm ())))
runRFrame liftp r = do (x, rframe) <- runStateT (unResourceT $ r <* newRFrame)
                                                emptyRFrame
                       return (x, (initialization rframe, finalization rframe))
    where emptyRFrame = RFrame mempty (return ()) (constant (return ())) liftp

-- | Allocate the current frame and start a new one.
newRFrame :: (Reflex t, Monad m, MonadAllocate pm m, Monad pm)
          => ResourceT' r t pm m ()
newRFrame = do rframe <- ResourceT get
               (inm, finm) <- lift $ createFrame (allocations rframe)
               ResourceT . put $ rframe { allocations = mempty
                                        , initialization = initialization rframe >> inm
                                        , finalization = fmap (finm >>) (finalization rframe)
                                        }

-- | Run a 'ResourceT' computation, returning the result of the computation, the initialization action
-- and a sampling action that returns the final deallocations. @'Performable' m@ actions that access
-- resources must not be scheduled before the initialization or after the finalization.
runResourceT :: (Reflex t, Monad m, MonadAllocate (Performable m) m, MonadSample t m', Monad (Performable m))
             => ResourceT r t m a
             -> m (a, Performable m (), m' (Performable m ()))
runResourceT = runResourceT' id

runResourceT' :: (Reflex t, Monad m, MonadAllocate pm m, MonadSample t m', Monad pm)
              => (forall x. pm x -> Performable m x)
              -> ResourceT' r t pm m a
              -> m (a, pm (), m' (pm ()))
runResourceT' liftp r = do (x, (inm, finm)) <- runRFrame liftp r
                           return (x, inm, sample finm)

-- | Run a 'ResourceT' computation in a 'ResourceContext', allowing you to return resources.
-- The returned value should not be used after the finalization action is executed.
runResourceTContext :: (Reflex t, Monad m, MonadAllocate (Performable m) m, MonadSample t m', Monad (Performable m))
                    => (forall r'. ResourceContext () r' => ResourceT r' t m (Res r' a))
                    -> m (a, Performable m (), m' (Performable m ()))
runResourceTContext f = runResourceT (unRes <$> f @(InternalResourceContext ()))

runResourceTContext' :: (Reflex t, Monad m, MonadAllocate pm m, MonadSample t m', Monad pm)
                     => (forall x. pm x -> Performable m x)
                     -> (forall r'. ResourceContext () r' => ResourceT' r' t pm m (Res r' a))
                     -> m (a, pm (), m' (pm ()))
runResourceTContext' liftp f = runResourceT' liftp (unRes <$> f @(InternalResourceContext ()))


-- | Run a 'ResourceT' computation in a context in which resources can be allocated.
resourceContext :: (forall r'. ResourceContext r r' => ResourceT' r' t pm m a) -> ResourceT' r t pm m a
resourceContext r = ResourceT (unResourceT (r @(InternalResourceContext _)))

fmapResourceContext :: Functor f => (forall r'. ResourceContext r r' => f (ResourceT' r' t pm m a)) -> f (ResourceT' r t pm m a)
fmapResourceContext r = fmap (\r' -> ResourceT (unResourceT r')) (r @(InternalResourceContext _))

-- | Same as 'resourceContext', but allows you to return resources wrapped in the 'Res' monad.
resourceContextRes :: Monad m => (forall r'. ResourceContext r r' => ResourceT' r' t pm m (Res r' a)) -> ResourceT' r t pm m (Res r a)
resourceContextRes r = ResourceT (unResourceT (Res . unRes <$> (r @(InternalResourceContext _))))

fmapResourceContextRes :: (Functor f, Monad m) => (forall r'. ResourceContext r r' => f (ResourceT' r' t pm m (Res r' a))) -> f (ResourceT' r t pm m (Res r a))
fmapResourceContextRes r = fmap (\r' -> ResourceT (unResourceT (Res . unRes <$> r'))) (r @(InternalResourceContext _))

-- | Combination of 'resourceContext' and 'resourceContextRes'.
resourceContextRes' :: Monad m => (forall r'. ResourceContext r r' => ResourceT' r' t pm m (a, Res r' b)) -> ResourceT' r t pm m (a, Res r b)
resourceContextRes' r = ResourceT (unResourceT (fmap (\(x, s) -> (x, Res $ unRes s))  (r @(InternalResourceContext _))))

fmapResourceContextRes' :: (Functor f, Monad m) => (forall r'. ResourceContext r r' => f (ResourceT' r' t pm m (a, Res r' b))) -> f (ResourceT' r t pm m (a, Res r b))
fmapResourceContextRes' r = fmap (\r' -> ResourceT (unResourceT (fmap (\(x, s) -> (x, Res $ unRes s)) r'))) (r @(InternalResourceContext _))

-- | Same as 'resourceContext', but allows you to return resources wrapped in the 'DynRes' monad.
resourceContextDynRes :: Monad m => (forall r'. ResourceContext r r' => ResourceT' r' t pm m (DynRes r' t a)) -> ResourceT' r t pm m (DynRes r t a)
resourceContextDynRes r = ResourceT (unResourceT (DynRes . unDynRes <$> (r @(InternalResourceContext _))))

fmapResourceContextDynRes :: (Functor f, Monad m) => (forall r'. ResourceContext r r' => f (ResourceT' r' t pm m (DynRes r' t a))) -> f (ResourceT' r t pm m (DynRes r t a))
fmapResourceContextDynRes r = fmap (\r' -> ResourceT (unResourceT (DynRes . unDynRes <$> r'))) (r @(InternalResourceContext _))

-- | Combination of 'resourceContext' and 'resourceContextDynRes'.
resourceContextDynRes' :: Monad m => (forall r'. ResourceContext r r' => ResourceT' r' t pm m (a, DynRes r' t b)) -> ResourceT' r t pm m (a, DynRes r t b)
resourceContextDynRes' r = ResourceT (unResourceT (fmap (\(x, s) -> (x, DynRes $ unDynRes s))  (r @(InternalResourceContext _))))

fmapResourceContextDynRes' :: (Functor f, Monad m) => (forall r'. ResourceContext r r' => f (ResourceT' r' t pm m (a, DynRes r' t b))) -> f (ResourceT' r t pm m (a, DynRes r t b))
fmapResourceContextDynRes' r = fmap (\r' -> ResourceT (unResourceT (fmap (\(x, s) -> (x, DynRes $ unDynRes s)) r'))) (r @(InternalResourceContext _))

{-
performEventRes_ :: PerformEvent t m => Res r (Event t (Performable m ())) -> ResourceT' r t pm m ()
performEventRes_ (Res x) = performEvent_ x

performEventRes :: PerformEvent t m => Res r (Event t (Performable m a)) -> ResourceT' r t pm m (Res r (Event t a))
performEventRes (Res x) = Res <$> performEvent x
-}

-- | Allocate a new resource.
allocate :: (ResourceContext rp r, MonadAllocate pm m, Monad m)
         => m (a, Allocation pm m)
         -> ResourceT' r t pm m (Res r a)
allocate mkAllocation = do (x, newAllocations) <- lift mkAllocation
                           ResourceT . modify $ \rframe ->
                                let allocations' = allocations rframe <> newAllocations
                                in rframe { allocations = allocations' }
                           return (Res x)

-- | Use a temporary context to allocate some resources. The resources of the first context are deallocated after
-- the initialization of the second context.
withTmpContext :: (Reflex t, MonadSample t m, Monad pm, MonadAllocate pm m, ResourceContext rp r, PerformEvent t m)
               => (forall r' t'. ResourceContext r r' => ResourceT' r' t' pm m (Res r' x))
                  -- ^ Context in which the temporary resources are created.
               -> (forall r'. ResourceContext r r' => Res (TmpResourceContext r') x -> ResourceT' r' t pm m (Res r' b))
                  -- ^ Context in which the temporary resources are used.
               -> ResourceT' r t pm m (Res r b)
withTmpContext t f = do rf <- ResourceT get
                        (Res act, (tinm, tbfinm)) <- lift . runRFrame (liftPerformable rf) $ (t @(InternalResourceContext _))
                        (Res x, (inm, bfinm)) <- lift . runRFrame (liftPerformable rf) $ (f @(InternalResourceContext _)) (Res act)
                        tfinm <- sample tbfinm
                        newRFrame
                        ResourceT . modify $ \rframe -> rframe { initialization = do initialization rframe
                                                                                     tinm
                                                                                     inm
                                                                                     tfinm
                                                               , finalization = (>>) <$> bfinm <*> finalization rframe
                                                               }
                        return (Res x)

performLifted_ :: PerformEvent t m => Event t (pm ()) -> ResourceT' r t pm m ()
performLifted_ ev = ResourceT get >>= \rframe -> performEvent_ (fmapCheap (liftPerformable rframe) ev)

unsafeRunWithReplace :: (Adjustable t m, MonadHold t m, MonadFix m, PerformEvent t m, MonadAllocate pm m, Monad pm)
                     => ResourceT' r t pm m a
                     -> Event t (ResourceT' r t pm m b)
                     -> ResourceT' r t pm m (a, Event t b)
unsafeRunWithReplace r0 ev =
    do rf <- ResourceT get
       ((x, state0@(in0, _)), res') <- lift $ runWithReplace (runRFrame (liftPerformable rf) r0)
                                                             (runRFrame (liftPerformable rf) <$> ev)
       (state', ev') <- mapAccumMB (\(_, bfin1) state2@(in2, _) ->
                                        do fin1 <- sample bfin1
                                           return (state2, fin1 >> in2))
                                   state0
                                   (fmapCheap snd res')
       performLifted_ ev'
       newRFrame
       ResourceT . modify $ \rframe -> rframe { initialization = initialization rframe >> in0
                                              , finalization = do (_, bfin') <- state'
                                                                  fin' <- bfin'
                                                                  finm <- finalization rframe
                                                                  return $ fin' >> finm
                                              }
       return (x, fmap fst res')

-- | Various constraints necessary for the Adjustable-like operations.
type ReplaceableResourceContext rp r t pm m = ( Adjustable t m, MonadHold t m
                                              , MonadFix m , PerformEvent t m
                                              , MonadAllocate pm m, ResourceContext rp r
                                              , Monad pm
                                              )

-- | Select between two actions based on the value of a 'Res'.
withEitherRes :: ResourceContext rp r
              => Res r (Either a b)
              -> (forall r'. ResourceContext r r' => Res r' a -> ResourceT' r' t pm m c)
              -> (forall r'. ResourceContext r r' => Res r' b -> ResourceT' r' t pm m c)
              -> ResourceT' r t pm m c
withEitherRes (Res (Left x)) l _ = resourceContext $ l (Res x)
withEitherRes (Res (Right x)) _ r = resourceContext $ r (Res x)

-- | Same as 'withEitherRes', but you can return resources wrapped in a 'Res'.
withEitherRes' :: (Monad m, ResourceContext rp r)
               => Res r (Either a b)
               -> (forall r'. ResourceContext r r' => Res r' a -> ResourceT' r' t pm m (c, Res r' d))
               -> (forall r'. ResourceContext r r' => Res r' b -> ResourceT' r' t pm m (c, Res r' d))
               -> ResourceT' r t pm m (c, Res r d)
withEitherRes' (Res (Left x)) l _ = resourceContextRes' $ l (Res x)
withEitherRes' (Res (Right x)) _ r = resourceContextRes' $ r (Res x)

-- | Select between two actions based on the availability of a 'Res'.
withMaybeRes :: ResourceContext rp r
             => Res r (Maybe a)
             -> (forall r'. ResourceContext r r' => Res r' a -> ResourceT' r' t pm m b)
             -> (forall r'. ResourceContext r r' => ResourceT' r' t pm m b)
             -> ResourceT' r t pm m b
withMaybeRes (Res (Just x)) y _ = resourceContext $ y (Res x)
withMaybeRes (Res Nothing) _ n = resourceContext n

-- | Same as 'withMaybeRes', but you can return resources wrapped in a 'Res'.
withMaybeRes' :: (Monad m, ResourceContext rp r)
              => Res r (Maybe a)
              -> (forall r'. ResourceContext r r' => Res r' a -> ResourceT' r' t pm m (b, Res r' c))
              -> (forall r'. ResourceContext r r' => ResourceT' r' t pm m (b, Res r' c))
              -> ResourceT' r t pm m (b, Res r c)
withMaybeRes' (Res (Just x)) y _ = resourceContextRes' $ y (Res x)
withMaybeRes' (Res Nothing) _ n = resourceContextRes' n

-- | 'withMaybeRes' without an alternative action.
whenRes :: (Monad m, ResourceContext rp r)
        => Res r (Maybe a)
        -> (forall r'. ResourceContext r r' => Res r' a -> ResourceT' r' t pm m b)
        -> ResourceT' r t pm m (Maybe b)
whenRes res f = withMaybeRes res (fmap Just . f) (return Nothing)

-- | 'withMaybeRes\'' without an alternative action.
whenRes' :: (Monad m, ResourceContext rp r)
         => Res r (Maybe a)
         -> (forall r'. ResourceContext r r' => Res r' a -> ResourceT' r' t pm m (b, Res r' c))
         -> ResourceT' r t pm m (Maybe b, Res r (Maybe c))
whenRes' res f = withMaybeRes' res (fmap (\(a, b) -> (Just a, fmap Just b)) . f) (return (Nothing, pure Nothing))

-- | Switch between two actions based on the availability of a 'DynRes'. Note that this will continuosly allocate
-- and deallocate the internal resources every time the @DynRes@ switches between @Just@ and @Nothing@. If you don't want this,
-- allocate them outside of the @withMaybeDynRes@ call.
withMaybeDynRes :: ReplaceableResourceContext rp r t pm m
                => DynRes r t (Maybe a)
                -> (forall r'. ResourceContext r r' => DynRes r' t a -> ResourceT' r' t pm m b)
                -> (forall r'. ResourceContext r r' => ResourceT' r' t pm m b)
                -> ResourceT' r t pm m (Dynamic t b)
withMaybeDynRes (DynRes dyn) y n = do mdyn <- maybeDyn dyn
                                      initial <- sample (current mdyn)
                                      (m0, m') <- runWithReplaceContext (maybe n (y . DynRes) initial)
                                                                        (maybe n (y . DynRes) <$> updated mdyn)
                                      holdDyn m0 m'

-- | Same as 'withMaybeDynRes', but you can return resources wrapped in a 'DynRes'.
withMaybeDynRes' :: ReplaceableResourceContext rp r t pm m
                 => DynRes r t (Maybe a)
                 -> (forall r'. ResourceContext r r' => DynRes r' t a -> ResourceT' r' t pm m (b, DynRes r' t c))
                 -> (forall r'. ResourceContext r r' => ResourceT' r' t pm m (b, DynRes r' t c))
                 -> ResourceT' r t pm m (Dynamic t b, DynRes r t c)
withMaybeDynRes' (DynRes dyn) y n = do mdyn <- maybeDyn dyn
                                       initial <- sample (current mdyn)
                                       (m0, m', d) <- runWithReplaceDynRes' (maybe n (y . DynRes) initial)
                                                                            (maybe n (y . DynRes) <$> updated mdyn)
                                       m <- holdDyn m0 m'
                                       return (m, d)

-- | Switch between two actions based on the value of a 'DynRes'. Note that this will continuosly allocate
-- and deallocate the internal resources every time the @DynRes@ switches between @Left@ and @Right@. If you don't want this,
-- allocate them outside of the @withEitherDynRes@ call.
withEitherDynRes :: ReplaceableResourceContext rp r t pm m
                 => DynRes r t (Either a b)
                 -> (forall r'. ResourceContext r r' => DynRes r' t a -> ResourceT' r' t pm m c)
                 -> (forall r'. ResourceContext r r' => DynRes r' t b -> ResourceT' r' t pm m c)
                 -> ResourceT' r t pm m (Dynamic t c)
withEitherDynRes (DynRes dyn) l r = do edyn <- eitherDyn dyn
                                       initial <- sample (current edyn)
                                       (m0, m') <- runWithReplaceContext (either (l . DynRes) (r . DynRes) initial)
                                                                         (either (l . DynRes) (r . DynRes) <$> updated edyn)
                                       holdDyn m0 m'

-- | Same as 'withEitherDynRes', but you can return resources wrapped in a 'DynRes'.
withEitherDynRes' :: ReplaceableResourceContext rp r t pm m
                 => DynRes r t (Either a b)
                 -> (forall r'. ResourceContext r r' => DynRes r' t a -> ResourceT' r' t pm m (c, DynRes r' t d))
                 -> (forall r'. ResourceContext r r' => DynRes r' t b -> ResourceT' r' t pm m (c, DynRes r' t d))
                 -> ResourceT' r t pm m (Dynamic t c, DynRes r t d)
withEitherDynRes' (DynRes dyn) l r = do edyn <- eitherDyn dyn
                                        initial <- sample (current edyn)
                                        (m0, m', d) <- runWithReplaceDynRes' (either (l . DynRes) (r . DynRes) initial)
                                                                             (either (l . DynRes) (r . DynRes) <$> updated edyn)
                                        m <- holdDyn m0 m'
                                        return (m, d)


-- | 'withMaybeDynRes' without an alternative action.
whenDynRes :: ReplaceableResourceContext rp r t pm m
           => DynRes r t (Maybe a)
           -> (forall r'. ResourceContext r r' => DynRes r' t a -> ResourceT' r' t pm m b)
           -> ResourceT' r t pm m (Dynamic t (Maybe b))
whenDynRes dyn f = withMaybeDynRes dyn (fmap Just . f) (return Nothing)

-- | 'withMaybeDynRes\'' without an alternative action.
whenDynRes' :: ReplaceableResourceContext rp r t pm m
            => DynRes r t (Maybe a)
            -> (forall r'. ResourceContext r r' => DynRes r' t a -> ResourceT' r' t pm m (b, DynRes r' t c))
            -> ResourceT' r t pm m (Dynamic t (Maybe b), DynRes r t (Maybe c))
whenDynRes' dyn f = withMaybeDynRes' dyn (fmap (\(a, b) -> (Just a, fmap Just b)) . f) (return (Nothing, pure Nothing))

-- | Similar to 'runWithReplaceContext', but uses a 'DynRes' instead of an event.
withDynResReplace :: ReplaceableResourceContext rp r t pm m
                  => DynRes r t a
                  -> (forall r'. ResourceContext r r' => Res r' a -> ResourceT' r' t pm m (Res r' b))
                  -> ResourceT' r t pm m (DynRes r t b)
withDynResReplace (DynRes dyn) f = sample (current dyn) >>= \dyn0 -> runWithReplaceRes (f (Res dyn0)) (f . Res <$> updated dyn)

withDynResReplace' :: ReplaceableResourceContext rp r t pm m
                   => DynRes r t a
                   -> (forall r'. ResourceContext r r' => Res r' a -> ResourceT' r' t pm m (b, Res r' c))
                   -> ResourceT' r t pm m (b, Event t b, DynRes r t c)
withDynResReplace' (DynRes dyn) f = sample (current dyn) >>= \dyn0 -> runWithReplaceRes' (f (Res dyn0)) (f . Res <$> updated dyn)

-- | This is a version of 'runWithReplace' that can be used inside of a resource context and can allocate internal
-- resources.
runWithReplaceContext :: ReplaceableResourceContext rp r t pm m
                      => (forall r'. ResourceContext r r' => ResourceT' r' t pm m a)
                      -> (forall r'. ResourceContext r r' => Event t (ResourceT' r' t pm m b))
                      -> ResourceT' r t pm m (a, Event t b)
runWithReplaceContext r0 ev = unsafeRunWithReplace (resourceContext r0) (fmapResourceContext ev)

-- | Same as 'runWithReplaceContext', but in this case you can return the internal resources in the form of a 'DynRes'.
runWithReplaceRes :: ReplaceableResourceContext rp r t pm m
                  => (forall r'. ResourceContext r r' => ResourceT' r' t pm m (Res r' a))
                  -> (forall r'. ResourceContext r r' => Event t (ResourceT' r' t pm m (Res r' a)))
                  -> ResourceT' r t pm m (DynRes r t a)
runWithReplaceRes r0 ev = unsafeRunWithReplace (resourceContextRes r0) (fmapResourceContextRes ev) >>= \(res0, res') -> (holdDynRes res0 res')

-- | Combination of 'runWithReplaceContext' and 'runWithReplaceRes'.
runWithReplaceRes' :: ReplaceableResourceContext rp r t pm m
                   => (forall r'. ResourceContext r r' => ResourceT' r' t pm m (a, Res r' c))
                   -> (forall r'. ResourceContext r r' => Event t (ResourceT' r' t pm m (b, Res r' c)))
                   -> ResourceT' r t pm m (a, Event t b, DynRes r t c)
runWithReplaceRes' r0 ev = do ((x0, s0), res') <- unsafeRunWithReplace (resourceContextRes' r0) (fmapResourceContextRes' ev)
                              s <- (holdDynRes s0 (fmapCheap snd res'))
                              return (x0, fmap fst res', s)

-- | Same as 'runWithReplaceRes', but in this case the resources to be returned are already 'DynRes'.
runWithReplaceDynRes :: ReplaceableResourceContext rp r t pm m
                     => (forall r'. ResourceContext r r' => ResourceT' r' t pm m (DynRes r' t a))
                     -> (forall r'. ResourceContext r r' => Event t (ResourceT' r' t pm m (DynRes r' t a)))
                     -> ResourceT' r t pm m (DynRes r t a)
runWithReplaceDynRes r0 ev = unsafeRunWithReplace (resourceContextDynRes r0) (fmapResourceContextDynRes ev) >>= \(res0, res') -> join <$> holdDynRes (pure res0) (fmapCheap pure res')

-- | 'runWithReplaceDynRes'
dynResReplace :: ReplaceableResourceContext rp r t pm m
              => (forall r'. ResourceContext r r' => ResourceT' r' t pm m (DynRes r' t a))
              -> (forall r'. ResourceContext r r' => Event t (ResourceT' r' t pm m (DynRes r' t a)))
              -> ResourceT' r t pm m (DynRes r t a)
dynResReplace = runWithReplaceDynRes

-- | Combination of 'runWithReplaceContext' and 'runWithReplaceDynRes'.
runWithReplaceDynRes' :: ReplaceableResourceContext rp r t pm m
                      => (forall r'. ResourceContext r r' => ResourceT' r' t pm m (a, DynRes r' t c))
                      -> (forall r'. ResourceContext r r' => Event t (ResourceT' r' t pm m (b, DynRes r' t c)))
                      -> ResourceT' r t pm m (a, Event t b, DynRes r t c)
runWithReplaceDynRes' r0 ev = do ((x0, s0), res') <- unsafeRunWithReplace (resourceContextDynRes' r0) (fmapResourceContextDynRes' ev)
                                 s <- join <$> holdDynRes (pure s0) (fmapCheap (pure . snd) res')
                                 return (x0, fmap fst res', s)

unsafeTraverseIntMapWithKeyWithAdjust :: (Adjustable t m, MonadHold t m, MonadFix m, PerformEvent t m, MonadAllocate pm m, Monad pm)
                                      => (I.Key -> v -> ResourceT' r t pm m v')
                                      -> I.IntMap v
                                      -> Event t (I.PatchIntMap v)
                                      -> ResourceT' r t pm m (I.IntMap v', Event t (I.PatchIntMap v'))
unsafeTraverseIntMapWithKeyWithAdjust f im0 im' =
    do rf <- ResourceT get
       (resm0, resm') <- lift $ traverseIntMapWithKeyWithAdjust (\k v -> runRFrame (liftPerformable rf) $ f k v) im0 im'
       let statem0 = fmap snd resm0
       (statem', ev') <- mapAccumMB (\states (PatchIntMap patch) ->
                                do (states', (as, ds)) <- flip runStateT (return (), return ()) $
                                                I.mergeA I.preserveMissing
                                                         (I.traverseMaybeMissing $ \_ intPatch ->
                                                                   case intPatch of
                                                                        Just state2@(in2, _) ->
                                                                             do modify $ \(as, ds) -> (as >> in2, ds)
                                                                                return (Just state2)
                                                                        Nothing -> return Nothing)
                                                         (I.zipWithMaybeAMatched $ \_ (_, bfin1) intPatch ->
                                                                   do fin1 <- lift . sample $ bfin1
                                                                      case intPatch of
                                                                           Just state2@(in2, _) ->
                                                                              do modify $ \(as, ds) -> (as >> in2, fin1 >> ds)
                                                                                 return (Just state2)
                                                                           Nothing ->
                                                                              do modify $ \(as, ds) -> (as, fin1 >> ds)
                                                                                 return Nothing)
                                                         states patch
                                   return (states', ds >> as)
                                        ) statem0 (fmap (fmap snd) resm')
                                       
       performLifted_ ev'
       newRFrame
       ResourceT . modify $ \rframe -> rframe { initialization = do initialization rframe
                                                                    mapM_ (\(in0, _) -> in0)
                                                                          statem0
                                              , finalization = do finm <- finalization rframe
                                                                  states' <- fmap I.elems statem'
                                                                  finm' <- mapM (\(_, fin') -> fin')
                                                                                states'
                                                                  return $ sequence_ finm' >> finm
                                              }

       return (fmap fst resm0, fmap (fmap fst) resm')

traverseIntMapWithKeyWithAdjustContext :: ReplaceableResourceContext rp r t pm m
                                       => (forall r'. ResourceContext r r' => I.Key -> v -> ResourceT' r' t pm m v')
                                       -> I.IntMap v
                                       -> Event t (I.PatchIntMap v)
                                       -> ResourceT' r t pm m (I.IntMap v', Event t (I.PatchIntMap v'))
traverseIntMapWithKeyWithAdjustContext f = unsafeTraverseIntMapWithKeyWithAdjust (\k v -> resourceContext (f k v))

traverseIntMapWithKeyWithAdjustRes :: ReplaceableResourceContext rp r t pm m
                                   => (forall r'. ResourceContext r r' => I.Key -> v -> ResourceT' r' t pm m (Res r' v'))
                                   -> I.IntMap v
                                   -> Event t (I.PatchIntMap v)
                                   -> ResourceT' r t pm m (DynRes r t (I.IntMap v'))
traverseIntMapWithKeyWithAdjustRes f im0 im' = do (resm0, resm') <- unsafeTraverseIntMapWithKeyWithAdjust (\k v -> fmap unRes $ resourceContextRes (f k v)) im0 im'
                                                  accumDynRes (\rm0 patch -> fromMaybe (unRes rm0) . Patch.apply patch <$> rm0)
                                                              (Res resm0) resm'

traverseIntMapWithKeyWithAdjustRes' :: ReplaceableResourceContext rp r t pm m
                                    => (forall r'. ResourceContext r r' => I.Key -> v -> ResourceT' r' t pm m (v', Res r' vr'))
                                    -> I.IntMap v
                                    -> Event t (I.PatchIntMap v)
                                    -> ResourceT' r t pm m (I.IntMap v', Event t (PatchIntMap v'), DynRes r t (I.IntMap vr'))
traverseIntMapWithKeyWithAdjustRes' f im0 im' = do (resm0, resm') <- unsafeTraverseIntMapWithKeyWithAdjust (\k v -> fmap (\(v', Res r) -> (v', r)) $ resourceContextRes' (f k v)) im0 im'
                                                   dynres <- accumDynRes (\rm0 patch -> fromMaybe (unRes rm0) . Patch.apply patch <$> rm0)
                                                                         (Res (snd <$> resm0)) (fmap snd <$> resm')
                                                   return (fst <$> resm0, fmap fst <$> resm', dynres)

traverseIntMapWithKeyWithAdjustDynRes :: ReplaceableResourceContext rp r t pm m
                                      => (forall r'. ResourceContext r r' => I.Key -> v -> ResourceT' r' t pm m (DynRes r' t v'))
                                      -> I.IntMap v
                                      -> Event t (I.PatchIntMap v)
                                      -> ResourceT' r t pm m (DynRes r t (I.IntMap v'))
traverseIntMapWithKeyWithAdjustDynRes f im0 im' = do (resm0, resm') <- unsafeTraverseIntMapWithKeyWithAdjust (\k v -> fmap unDynRes $ resourceContextDynRes (f k v)) im0 im'
                                                     DynRes . joinDynThroughIntMap <$> accumDyn (\rm0 patch -> fromMaybe rm0 $ Patch.apply patch rm0)
                                                                                                resm0 resm'
-- | 'traverseIntMapWithKeyWithAdjustDynRes'
dynResIntMap :: ReplaceableResourceContext rp r t pm m
             => (forall r'. ResourceContext r r' => I.Key -> v -> ResourceT' r' t pm m (DynRes r' t v'))
             -> I.IntMap v
             -> Event t (I.PatchIntMap v)
             -> ResourceT' r t pm m (DynRes r t (I.IntMap v'))
dynResIntMap = traverseIntMapWithKeyWithAdjustDynRes

traverseIntMapWithKeyWithAdjustDynRes' :: ReplaceableResourceContext rp r t pm m
                                       => (forall r'. ResourceContext r r' => I.Key -> v -> ResourceT' r' t pm m (v', DynRes r' t vr'))
                                       -> I.IntMap v
                                       -> Event t (I.PatchIntMap v)
                                       -> ResourceT' r t pm m (I.IntMap v', Event t (PatchIntMap v'), DynRes r t (I.IntMap vr'))
traverseIntMapWithKeyWithAdjustDynRes' f im0 im' = do (resm0, resm') <- unsafeTraverseIntMapWithKeyWithAdjust (\k v -> fmap (\(v', DynRes r) -> (v', r)) $ resourceContextDynRes' (f k v))
                                                                                                              im0 im'
                                                      dynres <- DynRes . joinDynThroughIntMap <$>
                                                                        accumDyn (\rm0 patch -> fromMaybe rm0 $ Patch.apply patch rm0)
                                                                                 (snd <$> resm0) (fmap snd <$> resm')
                                                      return (fst <$> resm0, fmap fst <$> resm', dynres)

newtype RunRFrameResult t m v a = RunRFrameResult (v a, (m (), Behavior t (m ())))
newtype DMapResResult v vr a = DMapResResult (v a, vr a)

fstDMapResResult :: DMapResResult v vr a -> v a
fstDMapResResult (DMapResResult (v, _)) = v

sndDMapResResult :: DMapResResult v vr a -> vr a
sndDMapResResult (DMapResResult (_, r)) = r

unsafeTraverseDMapWithKeyWithAdjustWithMove :: (Adjustable t m, MonadHold t m, MonadFix m, PerformEvent t m, MonadAllocate pm m, Monad pm, GCompare k)
                                            => (forall a. k a -> v a -> ResourceT' r t pm m (v' a))
                                            -> D.DMap k v
                                            -> Event t (D.PatchDMapWithMove k v)
                                            -> ResourceT' r t pm m (D.DMap k v', Event t (D.PatchDMapWithMove k v'))
unsafeTraverseDMapWithKeyWithAdjustWithMove f im0 im' =
    do rf <- ResourceT get
       (resm0, resm') <- lift $ traverseDMapWithKeyWithAdjustWithMove (\k v -> fmap RunRFrameResult . runRFrame (liftPerformable rf) $ f k v)
                                                                      im0 im'
       let statem0 = weakenDMapWith (\(RunRFrameResult (_, s)) -> s) resm0
           patchStatem' = weakenPatchDMapWithMoveWith (\(RunRFrameResult (_, s)) -> s) <$> resm'
       (statem', ev') <- mapAccumMB (\states wpatch@(M.PatchMapWithMove patch) ->
                                do (as, ds) <- flip execStateT (return (), return ()) $ flip M.traverseWithKey patch $
                                        \k p@(M.NodeInfo from to) -> case (M.lookup k states, from, to) of
                                                                        (Just (_, bfin1), M.From_Delete, Nothing) ->
                                                                            do fin1 <- lift $ sample bfin1
                                                                               modify $ \(as, ds) -> (as, fin1 >> ds)
                                                                               return p
                                                                        (Just (_, bfin1), M.From_Insert (in2, _), _) ->
                                                                            do fin1 <- lift $ sample bfin1
                                                                               modify $ \(as, ds) -> (as >> in2, fin1 >> ds)
                                                                               return p
                                                                        (Nothing, M.From_Insert (in2, _), _) ->
                                                                            do modify $ \(as, ds) -> (as >> in2, ds)
                                                                               return p
                                                                        _ -> return p
                                   return (fromMaybe states $ Patch.apply wpatch states, ds >> as)
                                        ) statem0 patchStatem'
       performLifted_ ev'
       newRFrame
       ResourceT . modify $ \rframe -> rframe { initialization = do initialization rframe
                                                                    M.traverseWithKey (\_ (in0, _) -> in0) statem0
                                                                    return ()
                                              , finalization = do finm <- finalization rframe
                                                                  states' <- fmap M.elems statem'
                                                                  finm' <- mapM (\(_, fin') -> fin')
                                                                                states'
                                                                  return $ sequence_ finm' >> finm
                                              }

       return ( D.map (\(RunRFrameResult (x, _)) -> x) resm0
              , fmap (D.mapPatchDMapWithMove (\(RunRFrameResult (x, _)) -> x)) resm'
              )

traverseDMapWithKeyWithAdjustWithMoveContext :: (ReplaceableResourceContext rp r t pm m, GCompare k)
                                             => (forall r' a. ResourceContext r r' => k a -> v a -> ResourceT' r' t pm m (v' a))
                                             -> D.DMap k v
                                             -> Event t (D.PatchDMapWithMove k v)
                                             -> ResourceT' r t pm m (D.DMap k v', Event t (D.PatchDMapWithMove k v'))
traverseDMapWithKeyWithAdjustWithMoveContext f = unsafeTraverseDMapWithKeyWithAdjustWithMove (\k v -> resourceContext (f k v))

traverseDMapWithKeyWithAdjustWithMoveRes :: (ReplaceableResourceContext rp r t pm m, GCompare k)
                                         => (forall r' a. ResourceContext r r' => k a -> v a -> ResourceT' r' t pm m (Res r' (v' a)))
                                         -> D.DMap k v
                                         -> Event t (D.PatchDMapWithMove k v)
                                         -> ResourceT' r t pm m (DynRes r t (D.DMap k v'))
traverseDMapWithKeyWithAdjustWithMoveRes f im0 im' = do (resm0, resm') <- unsafeTraverseDMapWithKeyWithAdjustWithMove (\k v -> fmap unRes $ resourceContextRes (f k v)) im0 im'
                                                        accumDynRes (\rm0 patch -> fromMaybe (unRes rm0) . Patch.apply patch <$> rm0)
                                                                    (Res resm0) resm'

traverseDMapWithKeyWithAdjustWithMoveRes' :: (ReplaceableResourceContext rp r t pm m, GCompare k)
                                          => (forall r' a. ResourceContext r r' => k a -> v a -> ResourceT' r' t pm m (v' a, Res r' (vr' a)))
                                          -> D.DMap k v
                                          -> Event t (D.PatchDMapWithMove k v)
                                          -> ResourceT' r t pm m (D.DMap k v', Event t (D.PatchDMapWithMove k v'), DynRes r t (D.DMap k vr'))
traverseDMapWithKeyWithAdjustWithMoveRes' f im0 im' = do (resm0, resm') <- unsafeTraverseDMapWithKeyWithAdjustWithMove (\k v -> fmap (\(v', Res r) -> DMapResResult (v', r)) $ resourceContextRes' (f k v)) im0 im'
                                                         dynres <- accumDynRes (\rm0 patch -> fromMaybe (unRes rm0) . Patch.apply patch <$> rm0)
                                                                               (Res (D.map sndDMapResResult resm0)) (D.mapPatchDMapWithMove sndDMapResResult <$> resm')
                                                         return (D.map fstDMapResResult resm0, D.mapPatchDMapWithMove fstDMapResResult <$> resm', dynres)

traverseDMapWithKeyWithAdjustWithMoveDynRes :: (ReplaceableResourceContext rp r t pm m, GCompare k)
                                            => (forall r' a. ResourceContext r r' => k a -> v a -> ResourceT' r' t pm m (DynRes r' t (v' a)))
                                            -> D.DMap k v
                                            -> Event t (D.PatchDMapWithMove k v)
                                            -> ResourceT' r t pm m (DynRes r t (D.DMap k v'))
traverseDMapWithKeyWithAdjustWithMoveDynRes f im0 im' = do (resm0, resm') <- unsafeTraverseDMapWithKeyWithAdjustWithMove (\k v -> fmap (Compose . unDynRes) $ resourceContextDynRes (f k v)) im0 im'
                                                           DynRes . (>>= distributeDMapOverDynPureG getCompose) <$> accumDyn (\rm0 patch -> fromMaybe rm0 $ Patch.apply patch rm0)
                                                                                                                             resm0 resm'
-- | 'traverseDMapWithKeyWithAdjustWithMoveDynRes'
dynResDMap :: (ReplaceableResourceContext rp r t pm m, GCompare k)
           => (forall r' a. ResourceContext r r' => k a -> v a -> ResourceT' r' t pm m (DynRes r' t (v' a)))
           -> D.DMap k v
           -> Event t (D.PatchDMapWithMove k v)
           -> ResourceT' r t pm m (DynRes r t (D.DMap k v'))
dynResDMap = traverseDMapWithKeyWithAdjustWithMoveDynRes


traverseDMapWithKeyWithAdjustWithMoveDynRes' :: (ReplaceableResourceContext rp r t pm m, GCompare k)
                                             => (forall r' a. ResourceContext r r' => k a -> v a -> ResourceT' r' t pm m (v' a, DynRes r' t (vr' a)))
                                             -> D.DMap k v
                                             -> Event t (D.PatchDMapWithMove k v)
                                             -> ResourceT' r t pm m (D.DMap k v', Event t (D.PatchDMapWithMove k v'), DynRes r t (D.DMap k vr'))
traverseDMapWithKeyWithAdjustWithMoveDynRes' f im0 im' = do (resm0, resm') <- unsafeTraverseDMapWithKeyWithAdjustWithMove (\k v -> fmap (\(v', DynRes r) -> DMapResResult (v', Compose r)) $ resourceContextDynRes' (f k v))
                                                                                                                          im0 im'
                                                            dynres <- DynRes . (>>= distributeDMapOverDynPureG getCompose) <$>
                                                                              accumDyn (\rm0 patch -> fromMaybe rm0 $ Patch.apply patch rm0)
                                                                                       (D.map sndDMapResResult resm0) (D.mapPatchDMapWithMove sndDMapResResult <$> resm')
                                                            return (D.map fstDMapResResult resm0, D.mapPatchDMapWithMove fstDMapResResult <$> resm', dynres)

instance (MonadHold t m, MonadFix m, PerformEvent t m, MonadAllocate pm m, Monad pm, Adjustable t m) => Adjustable t (ResourceT' () t pm m) where
    runWithReplace = unsafeRunWithReplace
    traverseIntMapWithKeyWithAdjust = unsafeTraverseIntMapWithKeyWithAdjust
    traverseDMapWithKeyWithAdjustWithMove = unsafeTraverseDMapWithKeyWithAdjustWithMove


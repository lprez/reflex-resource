{-# LANGUAGE RankNTypes #-}

module Reflex.Resource.DynRes (
    constDynRes,
    holdDynRes,
    mapDynResM,
    forDynResM,
    maybeDynRes,
    eitherDynRes,
    factorDynRes,
    holdUniqDynRes,
    holdUniqDynResBy,
    foldDynRes,
    foldDynResM,
    foldDynResMaybe,
    foldDynResMaybeM,
    joinDynResThroughMap,
    joinDynResThroughIntMap,
    splitDynResPure,
    distributeMapOverDynResPure,
    distributeIntMapOverDynResPure,
    distributeDMapOverDynResPure,
    distributeListOverDynRes,
    distributeListOverDynResWith,
    zipDynRes,
    zipDynResWith,
    demuxDynRes,
    demuxedDynRes,
    accumDynRes,
    accumMDynRes,
    mapAccumDynRes,
    mapAccumMDynRes,
    mapAccumMaybeDynRes,
    mapAccumMaybeMDynRes
) where

import Control.Monad.Fix
import Data.GADT.Compare (GEq, GCompare)
import Data.Functor.Compose
import Data.Functor.Identity
import qualified Data.Map as M
import qualified Data.IntMap as I
import qualified Data.Dependent.Map as D
import qualified Data.Dependent.Sum as DSum
import Reflex.Class
import Reflex.Dynamic

import Reflex.Resource.Internal

constDynRes :: Reflex t => Res r a -> DynRes r t a
constDynRes (Res x) = DynRes (constDyn x)

mapDynResM :: (Reflex t, MonadHold t m) => (forall r' m'. MonadSample t m' => Res r' a -> m' (Res r' b)) -> DynRes r t a -> m (DynRes r t b)
mapDynResM f (DynRes d) = DynRes <$> mapDynM (fmap unRes . f . Res) d

forDynResM :: (Reflex t, MonadHold t m) => DynRes r t a -> (forall r' m'. MonadSample t m' => Res r' a -> m' (Res r' b)) -> m (DynRes r t b)
forDynResM x f = mapDynResM f x

maybeDynRes :: (Reflex t, MonadFix m, MonadHold t m) => DynRes r t (Maybe a) -> m (DynRes r t (Maybe (DynRes r t a)))
maybeDynRes (DynRes d) = DynRes . fmap (fmap DynRes) <$> maybeDyn d

eitherDynRes :: (Reflex t, MonadFix m, MonadHold t m) => DynRes r t (Either a b) -> m (DynRes r t (Either (DynRes r t a) (DynRes r t b)))
eitherDynRes (DynRes d) = DynRes . fmap f <$> eitherDyn d
    where f (Left l) = Left $ DynRes l
          f (Right r) = Right $ DynRes r

factorDynRes :: (Reflex t, MonadHold t m, GEq k) => DynRes r t (DSum.DSum k v) -> m (DynRes r t (DSum.DSum k (Compose (DynRes r t) v)))
factorDynRes (DynRes d) = DynRes . fmap f <$> factorDyn d
    where f (dtag DSum.:=> Compose v) = dtag DSum.:=> Compose (DynRes v)

-- useless
-- scanDynRes :: (Reflex t, MonadHold t m, MonadFix m) => (forall r'. Res r' a -> b) -> (forall r'. Res r' a -> b -> b) -> DynRes r t a -> m (DynRes r t b)
-- scanDynRes g f (DynRes d) = fmap DynRes <$> scanDyn (g . Res) (\x -> f (Res x)) d

-- scanDynResMaybe :: (Reflex t, MonadHold t m, MonadFix m) => (forall r'. Res r' a -> b) -> (forall r'. Res r' a -> b -> Maybe b) -> DynRes r t a -> m (DynRes r t b)
-- scanDynResMaybe g f (DynRes d) = fmap DynRes <$> scanDyn (g . Res) (\x -> f (Res x)) d

holdUniqDynRes :: (Reflex t, MonadHold t m, MonadFix m, Eq a) => DynRes r t a -> m (DynRes r t a)
holdUniqDynRes (DynRes d) = DynRes <$> holdUniqDyn d

holdUniqDynResBy :: (Reflex t, MonadHold t m, MonadFix m) => (a -> a -> Bool) -> DynRes r t a -> m (DynRes r t a)
holdUniqDynResBy f (DynRes d) = DynRes <$> holdUniqDynBy f d

-- -- unsafe
-- improvingMaybeDynRes :: (Reflex t, MonadHold t m, MonadFix m) => DynRes r t (Maybe a) -> m (DynRes r t (Maybe a))

foldDynRes :: (Reflex t, MonadHold t m, MonadFix m) => (forall r'. a -> Res r' b -> Res r' b) -> Res r b -> Event t a -> m (DynRes r t b)
foldDynRes f (Res x) ev = DynRes <$> foldDyn (\a -> unRes . f a . Res) x ev

foldDynResM :: (Reflex t, MonadHold t m, MonadFix m) => (forall r'. a -> Res r' b -> PushM t (Res r' b)) -> Res r b -> Event t a -> m (DynRes r t b)
foldDynResM f (Res x) ev = DynRes <$> foldDynM (\a -> fmap unRes . f a . Res) x ev

foldDynResMaybe :: (Reflex t, MonadHold t m, MonadFix m) => (forall r'. a -> Res r' b -> Maybe (Res r' b)) -> Res r b -> Event t a -> m (DynRes r t b)
foldDynResMaybe f (Res x) ev = DynRes <$> foldDynMaybe (\a -> fmap unRes . f a . Res) x ev

foldDynResMaybeM :: (Reflex t, MonadHold t m, MonadFix m) => (forall r'. a -> Res r' b -> PushM t (Maybe (Res r' b))) -> Res r b -> Event t a -> m (DynRes r t b)
foldDynResMaybeM f (Res x) ev = DynRes <$> foldDynMaybeM (\a -> fmap (fmap unRes) . f a . Res) x ev

joinDynResThroughMap :: (Reflex t, Ord k) => DynRes r t (M.Map k (DynRes r t a)) -> DynRes r t (M.Map k a)
joinDynResThroughMap (DynRes d) = DynRes $ joinDynThroughMap (fmap unDynRes <$> d)

joinDynResThroughIntMap :: Reflex t => DynRes r t (I.IntMap (DynRes r t a)) -> DynRes r t (I.IntMap a)
joinDynResThroughIntMap (DynRes d) = DynRes $ joinDynThroughIntMap (fmap unDynRes <$> d)

splitDynResPure :: Reflex t => DynRes r t (a, b) -> (DynRes r t a, DynRes r t b)
splitDynResPure (DynRes d) = (\(d1, d2) -> (DynRes d1, DynRes d2)) $ splitDynPure d

distributeMapOverDynResPure :: (Reflex t, Ord k) => M.Map k (DynRes r t v) -> DynRes r t (M.Map k v)
distributeMapOverDynResPure m = DynRes $ distributeMapOverDynPure (unDynRes <$> m)

distributeIntMapOverDynResPure :: Reflex t => I.IntMap (DynRes r t v) -> DynRes r t (I.IntMap v)
distributeIntMapOverDynResPure m = DynRes $ distributeIntMapOverDynPure (unDynRes <$> m)

distributeDMapOverDynResPure :: (Reflex t, GCompare k) => D.DMap k (DynRes r t) -> DynRes r t (D.DMap k Identity)
distributeDMapOverDynResPure m = DynRes $ distributeDMapOverDynPure (D.map unDynRes m)

distributeListOverDynRes :: Reflex t => [DynRes r t a] -> DynRes r t [a]
distributeListOverDynRes m = DynRes $ distributeListOverDyn (unDynRes <$> m)

distributeListOverDynResWith :: Reflex t => ([a] -> b) -> [DynRes r t a] -> DynRes r t b
distributeListOverDynResWith f m = DynRes $ distributeListOverDynWith f (unDynRes <$> m)

zipDynRes :: Reflex t => DynRes r t a -> DynRes r t b -> DynRes r t (a, b)
zipDynRes (DynRes d1) (DynRes d2) = DynRes $ zipDyn d1 d2

zipDynResWith :: Reflex t => (a -> b -> c) -> DynRes r t a -> DynRes r t b -> DynRes r t c
zipDynResWith f (DynRes d1) (DynRes d2) = DynRes $ zipDynWith f d1 d2

demuxDynRes :: (Reflex t, Ord k) => DynRes r t k -> Res r (Demux t k)
demuxDynRes (DynRes d) = Res $ demux d

demuxedDynRes :: (Reflex t, Eq k) => Res r (Demux t k) -> k -> DynRes r t Bool
demuxedDynRes (Res d) k = DynRes $ demuxed d k

-- -- useless
-- switchDynRes :: Reflex t => DynRes r t (UnRes (Event t a)) -> Event t a
-- switchPromptlyDynRes :: Reflex t => DynRes r t (UnRes (Event t a)) -> Event t a
-- tagPromptlyDynRes :: Reflex t => DynRes t (UnRes a) -> Event t b -> Event t a
-- attachPromptlyDynRes :: Reflex t => DynRes t (UnRes a) -> Event t b -> Event t (a, b)
-- attachPromptlyDynResWith :: Reflex t => (a -> b -> c) -> DynRes r t (UnRes a) -> Event t b -> Event t c
-- attachPromptlyDynResWith :: Reflex t => (forall r'. Res r' a -> b -> c) -> DynRes r t a -> Event t b -> Event t c

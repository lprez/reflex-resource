{-# LANGUAGE RankNTypes, TypeApplications, FlexibleContexts #-}

module Reflex.Resource (
    MonadUseResource,
    ResourceContext,
    InitialContext,
    ResourceContext0,
    -- * ResourceT monad
    ResourceT',
    ResourceT,
    runResourceT,
    runResourceTContext,
    runResourceT',
    runResourceTContext',
    allocate,
    resourceContext,
    -- resourceContextRes,
    -- resourceContextRes',
    -- resourceContextDynRes,
    -- resourceContextDynRes',
    hoistResourceT,
    -- ** PerformEvent
    PerformableResT,
    performEventRes_,
    performEventRes,
    -- ** Adjustable-like operations
    ReplaceableResourceContext,
    runWithReplaceContext,
    runWithReplaceRes,
    runWithReplaceRes',
    runWithReplaceDynRes,
    runWithReplaceDynRes',
    traverseIntMapWithKeyWithAdjustContext,
    traverseIntMapWithKeyWithAdjustRes,
    traverseIntMapWithKeyWithAdjustRes',
    traverseIntMapWithKeyWithAdjustDynRes,
    traverseIntMapWithKeyWithAdjustDynRes',
    traverseDMapWithKeyWithAdjustWithMoveContext,
    traverseDMapWithKeyWithAdjustWithMoveRes,
    traverseDMapWithKeyWithAdjustWithMoveRes',
    traverseDMapWithKeyWithAdjustWithMoveDynRes,
    traverseDMapWithKeyWithAdjustWithMoveDynRes',
    dynResReplace,
    dynResIntMap,
    dynResDMap,
    -- ** Temporary context
    TmpResourceContext,
    withTmpContext,
    -- * Res monad
    Res,
    castRes,
    tmpRes,
    UnRes,
    toRes,
    fromRes,
    mapUnRes,
    -- *** Maybe resources
    withMaybeRes,
    withMaybeRes',
    whenRes,
    whenRes',
    -- *** Either resources
    withEitherRes,
    withEitherRes',
    -- * DynRes monad
    DynRes,
    dynRes,
    dynResDyn,
    castDynRes,
    -- sampling,
    unwrapDynRes,
    toDynRes,
    fromDynRes,
    sampleUnRes,
    -- *** Maybe resources
    withMaybeDynRes,
    withMaybeDynRes',
    whenDynRes,
    whenDynRes',
    -- *** Either resources
    withEitherDynRes,
    withEitherDynRes',
    -- *** Adjustable-like operations
    withDynResReplace,
    withDynResReplace',
    -- *** Dynamic-like operations
    module Reflex.Resource.DynRes,
) where

import Reflex.Resource.DynRes
import Reflex.Resource.Internal hiding (resourceContext)
import qualified Reflex.Resource.Internal as Internal


-- | Run a 'ResourceT' computation in a context in which resources can be allocated.
resourceContext :: (forall r'. ResourceContext () r' => ResourceT' r' t pm m a) -> ResourceT' () t pm m a
resourceContext r = ResourceT (unResourceT (r @(InternalResourceContext _)))

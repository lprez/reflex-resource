module Reflex.Resource (
    ResourceContext,
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
    -- -- ** PerformEvent
    -- performEventRes_,
    -- performEventRes,
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
import Reflex.Resource.Internal

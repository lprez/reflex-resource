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
    allocateTmp,

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

    -- * Using resources
    --
    -- | The functions provided in this section allow you to use the values inside of the 'Res'
    -- monad and are potentially unsafe. For instance, writing the value to an @IORef@ and reading it
    -- in a later Reflex frame is not safe.
    
    -- *** Inside of Performable
    dynResSampler,
    performEventRes_,
    performEventRes,
    -- *** Direct
    --
    -- | Note that these functions access the values contained in the 'Res' before
    -- their associated initialization action has been run. Whether this makes sense or not
    -- depends on the type of resources used and how you're using them.
    withRes,
    withRes_,
    liftRes,
    liftRes_,
    unsafeUnDynRes
) where

import Reflex.Resource.DynRes
import Reflex.Resource.Internal

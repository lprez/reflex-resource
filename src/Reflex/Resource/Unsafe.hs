{-|

This module exports some functions that allow you to use the values inside of 'Res' and 'DynRes'.
They're not meant to be used in normal code, their purpose is to turn an unsafe API that accesses
resources into a safe one. Operations that produce values that do not refer to any resource
and continue to be valid when the resources being used are finalized can return them as pure
values, and can be executed inside of a generic MonadUseResource monad,
while operations that produce resources or values that depend on them
must return 'Res' values that match the context of the 'ResourceT' monad (same @r@), and
the resource arguments that are referenced to by the return value must also have a
matching context.

-}

module Reflex.Resource.Unsafe (
    unRes,
    unDynRes
) where


import Reflex.Resource.Internal

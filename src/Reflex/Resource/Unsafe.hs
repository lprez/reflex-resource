{-|

This module exports some functions that allow you to use the values inside of 'Res' and 'DynRes'.
They're not meant to be used in normal code, their purpose is to turn an unsafe API that accesses
resources into a safe one.

-}

module Reflex.Resource.Unsafe (
    unRes,
    unDynRes
) where

import Reflex.Resource.Internal (Res(unRes), DynRes(unDynRes))

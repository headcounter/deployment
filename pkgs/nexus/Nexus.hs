{-|
Nexus is a small library which is some kind of a base library for various
functionality needed in the Headcounter deployment.

This module just exports various submodules, so it can be easily used via
qualified imports like this:

@
    import qualified Nexus

    someFunction :: a -> b -> c
    someFunction = Nexus.foo 1 2 3
@

-}
module Nexus
    ( module Nexus.NatInt32
    , module Nexus.Socket
    ) where

import Nexus.NatInt32
import Nexus.Socket

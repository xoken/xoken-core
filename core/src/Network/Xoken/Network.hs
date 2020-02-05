{-|
Module      : Network.Xoken.Network
Copyright   : Xoken Labs
License     : Open BSV License

Stability   : experimental
Portability : POSIX

This module provides basic types used for the Bitcoin networking protocol
together with 'Data.Serialize' instances for efficiently serializing and
de-serializing them.
-}
module Network.Xoken.Network
    ( module Common
    , module Message
    ) where

import Network.Xoken.Network.Common as Common
import Network.Xoken.Network.Message as Message

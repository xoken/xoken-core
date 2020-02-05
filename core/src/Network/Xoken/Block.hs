{-|
Module      : Network.Xoken.Block
Copyright   : Xoken Labs
License     : Open BSV License

Stability   : experimental
Portability : POSIX

Most functions relating to blocks are exported by this module.
-}
module Network.Xoken.Block
    ( module Network.Xoken.Block.Common
      -- * Block Header Chain
    , BlockWork
    , BlockHeaders(..)
    , BlockNode(..)
    , HeaderMemory(..)
    , BlockMap
    , getAncestor
    , isGenesis
    , initialChain
    , genesisMap
    , genesisNode
    -- , genesisBlock
    , connectBlocks
    , connectBlock
    , parentBlock
    , splitPoint
    , blockLocator
      -- * Merkle Blocks
    , MerkleBlock(..)
    , MerkleRoot
    , FlagBits
    , PartialMerkleTree
    , buildMerkleRoot
    , buildPartialMerkle
    , merkleBlockTxs
    , testMerkleRoot
    ) where

import Network.Xoken.Block.Common
import Network.Xoken.Block.Headers
import Network.Xoken.Block.Merkle

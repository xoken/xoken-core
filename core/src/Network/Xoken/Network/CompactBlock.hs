{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Network.Xoken.Network.CompactBlock
Copyright   : Xoken Labs
License     : Open BSV License

Stability   : experimental
Portability : POSIX

Common data types and functions to handle blocks from the block chain.
-}
module Network.Xoken.Network.CompactBlock
    ( CompactBlock(..)
    , SendCompact(..)
    , GetBlockTxns(..)
    , BlockTxns(..)
    , DiffIndexed(..)
    , PrefilledTx(..)
    , getCompactBlockSipKey
    , txHashToShortId
    , txHashToShortId'
    ) where

import qualified Codec.Serialise as CBOR
import Control.Monad (forM_, liftM2, mzero, replicateM)
import Crypto.MAC.SipHash
import Data.Aeson (FromJSON, ToJSON, Value(String), parseJSON, toJSON, withText)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (chr, ord)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize, decode, encode, get, put)
import Data.Serialize.Get
import Data.Serialize.Put
import Data.String (IsString, fromString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word32)
import Data.Word
import GHC.Generics
import Network.Xoken.Block.Common
import Network.Xoken.Crypto.Hash
import Network.Xoken.Network.Common
import Network.Xoken.Transaction.Common
import Network.Xoken.Util
import Numeric (showHex)
import qualified Text.Read as R

-- | Data type representing a variable-length integer. The 'CompactSize' type
-- is a restricted version of VarInt and only supports 1, 3 bytes. (0xFD followed by the length as uint16_t )
newtype CompactSize =
    CompactSize
        { getCompactSize :: Word16
        }
    deriving (Eq, Show, Read)

instance Serialize CompactSize where
    get = CompactSize <$> (getWord8 >>= go)
      where
        go 0xfd = fromIntegral <$> getWord16le
        go x = fromIntegral <$> return x
    put (CompactSize x)
        | x < 0xfd = putWord8 $ fromIntegral x
        | x <= 0xffff = do
            putWord8 0xfd
            putWord16le $ fromIntegral x

getCompactSizeBytesUsed :: Word16 -> Int
getCompactSizeBytesUsed vi
    | vi < 0xfd = 1
    | vi <= 0xffff = 2

putCompactSize :: Integral a => a -> Put
putCompactSize = put . CompactSize . fromIntegral

-- | The short transaction IDs calculated from the transactions which were not provided explicitly in prefilledtxn
type ShortTxID = Word64

getCBShortTxID :: Get Word64
getCBShortTxID = do
    x <- getBytes 6
    return $ readCBInt x

readCBInt :: B.ByteString -> Word64
readCBInt bs =
    (byte 0) .|. (byte 1 `shiftL` 8) .|. (byte 2 `shiftL` 16) .|. (byte 3 `shiftL` 24) .|. (byte 4 `shiftL` 32) .|.
    (byte 5 `shiftL` 40)
  where
    byte n = fromIntegral $ bs `B.index` n

putCBShortTxID :: Putter Word64
putCBShortTxID val = do
    let x = encodeCBWord64 val
    putByteString x

encodeCBWord64 :: Word64 -> B.ByteString
encodeCBWord64 x = B.pack $ map fromIntegral wl
  where
    wl =
        [ (x .&. 0xFF)
        , (x .&. 0xFF00) `shiftR` 8
        , (x .&. 0xFF0000) `shiftR` 16
        , (x .&. 0xFF000000) `shiftR` 24
        , (x .&. 0xFF00000000) `shiftR` 32
        , (x .&. 0xFF0000000000) `shiftR` 40
        ]

getCompactBlockSipKey :: BlockHeader -> Word64 -> SipKey 
getCompactBlockSipKey hdr nn = 
    let keyhash = sha256 $ encode hdr `C.append` (runPut $ putWord64le nn)
        bs = encode keyhash
        k0 =
            case runGet getWord64le bs of
                Left e -> Prelude.error e
                Right a -> a
        k1 =
            case runGet getWord64le $ B.drop 8 bs of
                Left e -> Prelude.error e
                Right a -> a
    in SipKey k0 k1

txHashToShortId :: BlockHeader -> Word64 -> TxHash -> Word64
txHashToShortId hdr nn txid = txHashToShortId' txid (getCompactBlockSipKey hdr nn)

txHashesToShortIds :: BlockHeader -> Word64 -> [TxHash] -> Word64
txHashesToShortIds hdr nn txids = let skey = getCompactBlockSipKey hdr nn in fmap (\txid -> txHashToShortId' txid skey)

txHashToShortId' :: TxHash -> SipKey -> Word64
txHashToShortId' txid skey = 
    let (SipHash val) = hashWith 2 4 skey $ encode txid
    in val .&. 0x0000FFFFFFFFFFFF

-- | A PrefilledTx structure is used in HeaderAndShortIDs to provide a list of a few transactions explicitly.
data PrefilledTx =
    PrefilledTx
        { pfIndex :: !Word16 -- differentially indexed
        , pfTx :: !Tx
        }
    deriving (Eq, Show, Read, Generic, Hashable)

instance Serialize PrefilledTx where
    get = do
        (CompactSize index) <- get
        tx <- get
        return $ PrefilledTx index tx
    put (PrefilledTx index tx) = do
        putCompactSize index
        put tx

-- | This structure is used to relay a block header, the short transactions IDs used for matching
-- | already-available transactions, and a select few transactions which we expect a peer may be missing.
data CompactBlock =
    CompactBlock
        { cbHeader :: !BlockHeader
        , cbNonce :: Word64
        , cbShortIDsLength :: !Word16
        , cbShortIDs :: [Word64]
        , cbPrefilledTxnLength :: !Word16
        , cbPrefilledTxns :: [PrefilledTx]
        }
    deriving (Eq, Show, Read, Generic)

instance Serialize CompactBlock where
    get = do
        header <- get
        nonce <- getWord64le
        (CompactSize sidlen) <- get
        sids <- replicateM (fromIntegral sidlen) getCBShortTxID
        (CompactSize pftxlen) <- get
        pftxns <- replicateM (fromIntegral pftxlen) get
        return $ CompactBlock header nonce sidlen sids pftxlen (fromDiffIndices pftxns)
    put (CompactBlock header nonce sidlen sids pftxlen pftxns) = do
        put header
        putWord64le nonce
        putCompactSize sidlen
        forM_ sids putCBShortTxID
        putCompactSize pftxlen
        forM_ (toDiffIndices pftxns) put

-- | The "high-bandwidth" mode, is enabled by setting the first boolean to 1 in a sendcmpct message.
-- | The "low-bandwidth" mode is enabled by setting the first boolean to 0 
data SendCompact =
    SendCompact
        { mode :: Word8
        , version :: Word64 -- set to 1
        }
    deriving (Eq, Show, Read, Generic)

instance Serialize SendCompact where
    get = do
        mode <- get
        version <- get
        return $ SendCompact mode version
    put (SendCompact mode version) = do
        put mode
        put version

--
-- | This is used to list transaction indexes in a block being requested.
data GetBlockTxns =
    GetBlockTxns
        { gbBlockhash :: !BlockHash
        , gbIndexesLength :: !Word16
        , gbIndexes :: ![Word16] -- differentially encoded
        }
    deriving (Eq, Show, Read, Generic)

instance Serialize GetBlockTxns where
    get = do
        bhash <- get
        (CompactSize indlen) <- get
        indexes <-
            replicateM
                (fromIntegral indlen)
                (do (CompactSize idx) <- get
                    return idx)
        return $ GetBlockTxns bhash indlen (fromDiffIndices indexes)
    put (GetBlockTxns bhash indlen indexes) = do
        put bhash
        putCompactSize indlen
        forM_ (toDiffIndices indexes) putCompactSize

--
-- | structure is used to provide some of the transactions in a block, as requested.
data BlockTxns =
    BlockTxns
        { btBlockhash :: !BlockHash
        , btTransactionsLength :: !Word16
        , btTransactions :: ![Tx]
        }
    deriving (Eq, Show, Read, Generic)

instance Serialize BlockTxns where
    get = do
        bhash <- get
        (CompactSize txnlen) <- get
        txns <- replicateM (fromIntegral txnlen) get
        return $ BlockTxns bhash txnlen txns
    put (BlockTxns bhash txnlen txns) = do
        put bhash
        putCompactSize txnlen
        forM_ txns put
--

class DiffIndexed a where
    toDiffIndices :: [a] -> [a]
    fromDiffIndices :: [a] -> [a]

instance DiffIndexed Word16 where
    toDiffIndices [] = []
    toDiffIndices (x:xs) = x:(toDiffIndicesWith xs x)
        where toDiffIndicesWith [] _ = []
              toDiffIndicesWith (x:xs) n = (x - n - 1):(toDiffIndicesWith xs x)
    fromDiffIndices [] = []
    fromDiffIndices (x:xs) = x:(fromDiffIndicesWith xs x)
        where fromDiffIndicesWith [] _ = []
              fromDiffIndicesWith (x:xs) n = let ind = (n + x + 1) in ind:(fromDiffIndicesWith xs ind)

instance DiffIndexed PrefilledTx where
    toDiffIndices [] = []
    toDiffIndices (x@(PrefilledTx i t):xs) = x:(toDiffIndicesWith xs i)
        where toDiffIndicesWith [] _ = []
              toDiffIndicesWith (x:xs) n = (x {pfIndex = (pfIndex x) - n - 1}):(toDiffIndicesWith xs i) 
    fromDiffIndices [] = []
    fromDiffIndices (x@(PrefilledTx i t):xs) = x:(fromDiffIndicesWith xs i)
        where fromDiffIndicesWith [] _ = []
              fromDiffIndicesWith (x:xs) n = let ind = (n + (pfIndex x) + 1) in (x {pfIndex = ind}):(fromDiffIndicesWith xs ind)

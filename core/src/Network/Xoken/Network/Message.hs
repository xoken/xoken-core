{- |
Module      : Network.Xoken.Network.Message
Copyright   : Xoken Labs
License     : Open BSV License

Stability   : experimental
Portability : POSIX

Peer-to-peer network message serialization.
-}
module Network.Xoken.Network.Message
--  Network Message
    (
    Message (..),
    MessageHeader (..),
    msgType,
    putMessage,
    getMessage,
    getDeflatedBlock,
    getConfirmedTx,
    getConfirmedTxBatch,
) where

import Control.Applicative
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Serialize (Serialize, encode, get, put)
import Data.Serialize.Get (Get, getByteString, getWord32be, getWord32le, getWord64le, isolate, lookAhead, lookAheadM)
import Data.Serialize.Put (Putter, putByteString, putWord32be, putWord32le, putWord64le)
import Data.Word (Word32, Word64)
import Network.Xoken.Block.Common
import Network.Xoken.Block.Merkle
import Network.Xoken.Constants
import Network.Xoken.Crypto.Hash
import Network.Xoken.Network.Common
import Network.Xoken.Network.CompactBlock
import Network.Xoken.Transaction.Common

{- | Data type representing the header of a 'Message'. All messages sent between
 nodes contain a message header.
-}

data MessageHeader = MessageHeader
    { headMagic :: !Word32
    , -- | message type
      headCmd :: !MessageCommand
    , -- | length of payload
      headPayloadSize :: !Word32
    , -- | checksum of payload
      headChecksum :: !CheckSum32
    }

instance Serialize MessageHeader where
    get = MessageHeader <$> getWord32be <*> get <*> getWord32le <*> get
    put (MessageHeader m c l chk) = do
        putWord32be m
        put c
        putWord32le l
        put chk

{- | The 'Message' type is used to identify all the valid messages that can be
 sent between bitcoin peers. Only values of type 'Message' will be accepted
 by other bitcoin peers as bitcoin protocol messages need to be correctly
 serialized with message headers. Serializing a 'Message' value will
 include the 'MessageHeader' with the correct checksum value automatically.
 No need to add the 'MessageHeader' separately.
-}
data Message
    = MVersion !Version
    | MVerAck
    | MAddr !Addr
    | MInv !Inv
    | MGetData !GetData
    | MNotFound !NotFound
    | MGetBlocks !GetBlocks
    | MGetHeaders !GetHeaders
    | MTx !Tx
    | MConfTx ![Tx]
    | MBlock !DefBlock
    | MMerkleBlock !MerkleBlock
    | MHeaders !Headers
    | MGetAddr
    | MPing !Ping
    | MPong !Pong
    | MAlert !Alert
    | MMempool
    | MReject !Reject
    | MSendHeaders
    | MSendCompact !SendCompact
    | MCompactBlock !CompactBlock
    | MGetBlockTxns !GetBlockTxns
    | MBlockTxns !BlockTxns
    | MOther !ByteString !ByteString
    deriving (Eq, Show)

-- | Get 'MessageCommand' assocated with a message.
msgType :: Message -> MessageCommand
msgType (MVersion _) = MCVersion
msgType MVerAck = MCVerAck
msgType (MAddr _) = MCAddr
msgType (MInv _) = MCInv
msgType (MGetData _) = MCGetData
msgType (MNotFound _) = MCNotFound
msgType (MGetBlocks _) = MCGetBlocks
msgType (MGetHeaders _) = MCGetHeaders
msgType (MTx _) = MCTx
msgType (MConfTx _) = MCConfTx
msgType (MBlock _) = MCBlock
msgType (MMerkleBlock _) = MCMerkleBlock
msgType (MHeaders _) = MCHeaders
msgType (MPing _) = MCPing
msgType (MPong _) = MCPong
msgType (MAlert _) = MCAlert
msgType MMempool = MCMempool
msgType (MReject _) = MCReject
msgType MSendHeaders = MCSendHeaders
msgType MGetAddr = MCGetAddr
msgType (MSendCompact _) = MCSendCompact
msgType (MCompactBlock _) = MCCompactBlock
msgType (MGetBlockTxns _) = MCGetBlockTxns
msgType (MBlockTxns _) = MCBlockTxns
msgType (MOther c _) = MCOther c

getDeflatedBlock :: Get (Maybe DefBlock)
getDeflatedBlock = lookAheadM $ Just <$> get

getConfirmedTx :: Get (Maybe Tx)
getConfirmedTx = lookAheadM $ Just <$> get

getConfirmedTxBatch :: Get ([Tx])
getConfirmedTxBatch = fmap catMaybes $ some getConfirmedTx

-- | Deserializer for network messages.
getMessage :: Network -> Get Message
getMessage net = do
    (MessageHeader mgc' cmd' len' chk') <- get
    (mgc, cmd, len, chk) <- do
                unless (mgc' == getNetworkMagic net) (fail $ "get: Invalid network magic bytes: " ++ show mgc')
                if (len' == 0xFFFFFFFF) -- (cmd' == "extmsg") && 
                    then do
                        -- skip checksum calc as its expected to be invalid anyways
                        cmdExtStr <- getByteString 12
                        let cmdExt = stringToCommand cmdExtStr
                        lenExt <- getWord64le
                        return (mgc', cmdExt, fromIntegral lenExt, chk')
                    else do
                        bs <- lookAhead $ getByteString $ fromIntegral len'
                        unless (checkSum32 bs == chk') (fail $ "get: Invalid message checksum: " ++ show chk')
                        return (mgc', cmd', fromIntegral len', chk')

    if len > 0
        then isolate (fromIntegral len) $
            case cmd of
                MCVersion -> MVersion <$> get
                MCAddr -> MAddr <$> get
                MCInv -> MInv <$> get
                MCGetData -> MGetData <$> get
                MCNotFound -> MNotFound <$> get
                MCGetBlocks -> MGetBlocks <$> get
                MCGetHeaders -> MGetHeaders <$> get
                MCTx -> MTx <$> get
                -- MCBlock -> MBlock <$> get
                MCMerkleBlock -> MMerkleBlock <$> get
                MCHeaders -> MHeaders <$> get
                MCPing -> MPing <$> get
                MCPong -> MPong <$> get
                MCAlert -> MAlert <$> get
                MCReject -> MReject <$> get
                MCOther c -> MOther c <$> getByteString (fromIntegral len)
                MCSendCompact -> MSendCompact <$> get
                MCCompactBlock -> MCompactBlock <$> get
                MCGetBlockTxns -> MGetBlockTxns <$> get
                MCBlockTxns -> MBlockTxns <$> get
        else case cmd of
            MCGetAddr -> return MGetAddr
            MCVerAck -> return MVerAck
            MCMempool -> return MMempool
            MCSendHeaders -> return MSendHeaders
            _ -> fail $ "get: Invalid command " ++ show cmd

-- | Serializer for network messages.
putMessage :: Network -> Putter Message
putMessage net msg = do
    let (cmd, payload) =
            case msg of
                MVersion m -> (MCVersion, encode m)
                MVerAck -> (MCVerAck, BS.empty)
                MAddr m -> (MCAddr, encode m)
                MInv m -> (MCInv, encode m)
                MGetData m -> (MCGetData, encode m)
                MNotFound m -> (MCNotFound, encode m)
                MGetBlocks m -> (MCGetBlocks, encode m)
                MGetHeaders m -> (MCGetHeaders, encode m)
                MTx m -> (MCTx, encode m)
                -- MBlock m -> (MCBlock, encode m)
                MMerkleBlock m -> (MCMerkleBlock, encode m)
                MHeaders m -> (MCHeaders, encode m)
                MGetAddr -> (MCGetAddr, BS.empty)
                MPing m -> (MCPing, encode m)
                MPong m -> (MCPong, encode m)
                MAlert m -> (MCAlert, encode m)
                MMempool -> (MCMempool, BS.empty)
                MReject m -> (MCReject, encode m)
                MSendHeaders -> (MCSendHeaders, BS.empty)
                MSendCompact m -> (MCSendCompact, encode m)
                MCompactBlock m -> (MCCompactBlock, encode m)
                MGetBlockTxns m -> (MCGetBlockTxns, encode m)
                MBlockTxns m -> (MCBlockTxns, encode m)
                MOther c p -> (MCOther c, p)
        chk = checkSum32 payload
        len = fromIntegral $ BS.length payload
        header = MessageHeader (getNetworkMagic net) cmd len chk
    put header
    putByteString payload

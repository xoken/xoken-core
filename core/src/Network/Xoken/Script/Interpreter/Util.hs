{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Xoken.Script.Interpreter.Util where

import           Data.Int                       ( Int64 )
import           Data.EnumBitSet                ( T
                                                , toEnums
                                                )
import           Data.List                      ( unfoldr )
import           Data.Word                      ( Word8
                                                , Word32
                                                , Word64
                                                )
import           Data.Bits                      ( Bits
                                                , (.&.)
                                                , (.|.)
                                                , shiftL
                                                , shiftR
                                                )
import           Crypto.Secp256k1               ( Sig
                                                , PubKey
                                                , msg
                                                , verifySig
                                                )
import           Data.Serialize                 ( encode )
import           Data.ByteString.Short          ( fromShort )
import qualified Data.ByteString               as BS
import           Network.Xoken.Script.Common
import           Network.Xoken.Script.SigHash
import           Network.Xoken.Constants
import           Network.Xoken.Transaction.Common
import           Network.Xoken.Crypto.Hash
import           Network.Xoken.Crypto.Signature

-- uses reversed MPI without length
-- MPI is 4B length and big endian number with most significant bit for sign
class BigNum a where
   bin2num :: BS.ByteString -> a
   bin2num' :: Bool -> Int -> BS.ByteString -> Maybe a
   num2bin :: a -> BS.ByteString
   num2binpad :: a -> Word32 -> Maybe BS.ByteString

newtype BN = BN Integer
   deriving (Eq, Ord, Enum, Num, Real, Integral, Bits, Show)

instance BigNum BN where

  bin2num bytes = case BS.unsnoc bytes of
    Just (rest, byte)
      | byte .&. 0x80 /= 0 -> -roll (BS.snoc rest (byte .&. 0x7f))
      | otherwise          -> roll bytes
    _ -> 0

  bin2num' require_minimal max_size bytes =
    if size > fromIntegral max_size || (require_minimal && not minimal)
      then Nothing
      else Just $ bin2num bytes
   where
    size    = BS.length bytes
    minimal = isMinimallyEncoded bytes max_size

  num2bin n = BS.pack $ add_sign (n < 0) (unroll $ abs n)

  num2binpad n s = case length bytes `compare` size of
    LT -> go $ replicate (size - length bytes) 0 ++ bytes
    EQ -> go bytes
    GT -> Nothing
   where
    size  = fromIntegral s
    bytes = unroll $ abs n
    go    = Just . BS.pack . add_sign (n < 0)

roll :: (Integral a, Bits a) => BS.ByteString -> a
roll = BS.foldr unstep 0 where unstep b a = a `shiftL` 8 .|. fromIntegral b

unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step where
  step 0 = Nothing
  step i = Just (fromIntegral i, i `shiftR` 8)

add_sign :: Bool -> [Word8] -> [Word8]
add_sign _ [] = []
add_sign negative bytes | byte .&. 0x80 /= 0 = bytes ++ [sign_bit]
                        | otherwise          = init bytes ++ [byte .|. sign_bit]
 where
  byte     = last bytes
  sign_bit = if negative then 0x80 else 0

type ScriptFlags = T Word ScriptFlag

instance Show ScriptFlags where
  show = show . toEnums

data ScriptFlag
  = VERIFY_NONE
  | VERIFY_P2SH
  | VERIFY_STRICTENC
  | VERIFY_DERSIG
  | VERIFY_LOW_S
  | VERIFY_NULLDUMMY
  | VERIFY_SIGPUSHONLY
  | VERIFY_MINIMALDATA
  | VERIFY_DISCOURAGE_UPGRADABLE_NOPS
  | VERIFY_CLEANSTACK
  | VERIFY_CHECKLOCKTIMEVERIFY
  | VERIFY_CHECKSEQUENCEVERIFY
  | VERIFY_MINIMALIF
  | VERIFY_NULLFAIL
  | VERIFY_COMPRESSED_PUBKEYTYPE
  | ENABLE_SIGHASH_FORKID
  | GENESIS
  | UTXO_AFTER_GENESIS
  deriving (Show, Enum)

type Signature = BS.ByteString
type SighashForkid = Bool

data BaseSignatureChecker = BaseSignatureChecker
  { checkSig :: Sig -> SigHash -> PubKey -> Script -> SighashForkid -> Bool
  , checkLockTime :: BN -> Bool
  , checkSequence :: BN -> Bool
  }

txSigChecker net tx nIn amount inputIndex = BaseSignatureChecker
  { checkSig      = checkSigFull net tx amount inputIndex
  , checkLockTime = checkLockTimeFull tx nIn
  , checkSequence = checkSequenceFull tx nIn
  }

checkSigFull
  :: Network
  -> Tx
  -> Word64
  -> Int
  -> Sig
  -> SigHash
  -> PubKey
  -> Script
  -> SighashForkid
  -> Bool
checkSigFull net tx amount inputIndex sig sighash pubkey script forkid =
  verifyHashSig hash sig pubkey
  where hash = txSigHash net tx script amount inputIndex sighash

checkLockTimeFull :: Tx -> Int -> BN -> Bool
checkLockTimeFull tx nIn n =
  ((tx_n < threshold && n < threshold) || (tx_n >= threshold && n >= threshold))
    && (n <= tx_n)
    && (sequenceFinal /= txInSequence (txIn tx !! nIn))
 where
  tx_n          = fromIntegral $ txLockTime tx
  threshold     = 500000000
  sequenceFinal = 0xffffffff

checkSequenceFull :: Tx -> Int -> BN -> Bool
checkSequenceFull tx nIn n =
  (txVersion tx >= 2)
    && (txToSequence .&. sequenceLockTimeDisableFlag == 0)
    && (  (txToSequenceMasked < flag && nSequenceMasked < bnflag)
       || (txToSequenceMasked >= flag && nSequenceMasked >= bnflag)
       )
    && (nSequenceMasked <= fromIntegral txToSequenceMasked)
 where
  txToSequence = fromIntegral $ txInSequence (txIn tx !! nIn) :: Int
  nLockTimeMask = fromIntegral flag .|. sequenceLockTimeMask :: Word32
  txToSequenceMasked = txToSequence .&. fromIntegral nLockTimeMask :: Int
  nSequenceMasked = n .&. fromIntegral nLockTimeMask :: BN
  flag = sequenceLockTimeTypeFlag
  bnflag = fromIntegral flag
  sequenceLockTimeDisableFlag = 2 ^ 31
  sequenceLockTimeTypeFlag = 2 ^ 22
  sequenceLockTimeMask = 0x0000ffff

cleanupScriptCode :: [ScriptOp] -> Signature -> SigHash -> Bool -> [ScriptOp]
cleanupScriptCode script sigBS sighash forkidEnabled
  | not forkidEnabled || not (hasForkIdFlag sighash) = filter notSigPush script
  | otherwise = script
 where
  notSigPush (OP_PUSHDATA bs _) = not $ sigBS `BS.isPrefixOf` bs
  notSigPush _                  = True

sigHash :: Signature -> SigHash
sigHash = toEnum . fromIntegral . BS.last

int2BS :: Integral a => a -> BS.ByteString
int2BS = num2bin . BN . fromIntegral

isMinimallyEncoded :: BS.ByteString -> Int -> Bool
isMinimallyEncoded bs max_size =
  size <= max_size && case BS.unpack $ BS.drop (size - 2) bs of
    [before_last, last] -> last .&. 0x7f /= 0 || before_last .&. 0x80 /= 0
    _                   -> True
  where size = BS.length bs

isZero :: BS.ByteString -> Bool
isZero bs =
  size == 0 || (BS.all (== 0) init && BS.unpack last `elem` [[0], [0x80]])
 where
  size         = BS.length bs
  (init, last) = BS.splitAt (size - 1) bs

isPush :: ScriptOp -> Bool
isPush = (<= OP_16)

isPushOnly :: Script -> Bool
isPushOnly = all isPush . scriptOps

isP2SH :: Script -> Bool
isP2SH (Script ops@(OP_HASH160 : OP_PUSHDATA bs OPCODE : rest)) =
  BS.length bs == 0x14 && length ops == 23 && last rest == OP_EQUAL
isP2SH _ = False

ifso :: Maybe a -> Bool -> Maybe a
ifso y x = if x then y else Nothing
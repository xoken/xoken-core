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
import           Data.ByteString.Short          ( fromShort )
import qualified Data.ByteString               as BS
import           Network.Xoken.Script.Common
import           Network.Xoken.Script.SigHash
import           Network.Xoken.Constants
import           Network.Xoken.Transaction.Common
import           Network.Xoken.Crypto.Hash

class BigNum a where
   bin2num :: BS.ByteString -> a
   num2bin :: a -> BS.ByteString
   num2binpad :: a -> Word32 -> Maybe BS.ByteString
   num2u32 :: a -> Maybe Word32

newtype BN = BN Integer
   deriving (Eq, Ord, Enum, Num, Real, Integral, Bits, Show)

instance BigNum BN where

  bin2num bytes = case BS.uncons bytes of
    Just (byte, rest)
      | byte .&. 0x80 /= 0 -> -roll (BS.cons (byte .&. 0x7f) rest)
      | otherwise          -> roll bytes
    _ -> 0

  num2bin n = BS.pack $ add_sign sign_bit bytes   where
    bytes    = unroll $ abs n
    sign_bit = if n < 0 then 0x80 else 0

  num2binpad n s = case length bytes `compare` size of
    LT -> go $ replicate (size - length bytes) 0 ++ bytes
    EQ -> go bytes
    GT -> Nothing
   where
    size     = fromIntegral s
    bytes    = unroll $ abs n
    go       = Just . BS.pack . add_sign sign_bit
    sign_bit = if n < 0 then 0x80 else 0

  num2u32 n
    | n >= 0 && n <= fromIntegral (maxBound :: Int) = Just $ fromIntegral n
    | True = Nothing

roll :: (Integral a, Bits a) => BS.ByteString -> a
roll = BS.foldr unstep 0 where unstep b a = a `shiftL` 8 .|. fromIntegral b

unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step where
  step 0 = Nothing
  step i = Just (fromIntegral i, i `shiftR` 8)

add_sign :: Word8 -> [Word8] -> [Word8]
add_sign sign_bit bytes@(byte : rest) | byte .&. 0x80 /= 0 = sign_bit : bytes
                                      | otherwise = (byte .|. sign_bit) : rest
add_sign _ [] = []

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

type EnabledSighashForkid = Bool

data BaseSignatureChecker = BaseSignatureChecker
  { checkSig :: Sig -> PubKey -> Script -> EnabledSighashForkid -> Bool
  , checkLockTime :: Int64 -> Bool
  , checkSequence :: Int64 -> Bool
  }

txSigChecker net tx = BaseSignatureChecker { checkSig      = checkSigFull net tx
                                           , checkLockTime = undefined
                                           , checkSequence = undefined
                                           }

checkSigFull
  :: Network -> Tx -> Sig -> PubKey -> Script -> EnabledSighashForkid -> Bool
checkSigFull net tx sig pubkey script forkid =
  case msg $ fromShort $ getHash256 hash of
    Just m -> verifySig pubkey sig m
    _      -> False
  where hash = txSigHash net tx script undefined undefined (sigHash sig)

cleanupScriptCode :: [ScriptOp] -> Sig -> ScriptFlags -> [ScriptOp]
cleanupScriptCode = undefined

sigHash :: Sig -> SigHash
sigHash sig = undefined
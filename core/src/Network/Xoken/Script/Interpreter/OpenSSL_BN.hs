{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Xoken.Script.Interpreter.OpenSSL_BN where

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
import qualified Data.ByteString               as BS

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
add_sign sign_bit [] = if sign_bit /= 0 then [sign_bit] else []
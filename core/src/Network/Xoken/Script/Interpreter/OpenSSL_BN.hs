{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Xoken.Script.Interpreter.OpenSSL_BN where

import           Data.Word                      ( Word32 )
import qualified Data.ByteString               as BS
import qualified Data.Serialize                as S
import qualified Data.Bits                     as B

class (Eq a, Ord a) => BigNum a where
   num2bin :: a -> a -> Maybe BS.ByteString
   bin2num :: BS.ByteString -> Maybe a
   num2u32 :: a -> Maybe Word32

newtype BN = BN Integer
   deriving (Eq, Ord, Enum, Num, Real, Integral, B.Bits, Show, S.Serialize)

instance BigNum BN where
  num2u32 n
    | n >= 0 && n <= BN (toInteger (maxBound :: Int)) = Just $ fromIntegral n
    | True = Nothing

int2bin :: Int -> BS.ByteString
int2bin = S.encode . BN . fromIntegral

module Network.Xoken.Script.OpenSSL_BN where

import qualified Data.ByteString as BS

class (Eq a, Ord a) => BigNum a where
   num2bin :: a -> a -> Maybe BS.ByteString
   bin2num :: BS.ByteString -> Maybe a

newtype BN = BN Integer deriving (Eq, Ord)

instance BigNum BN 
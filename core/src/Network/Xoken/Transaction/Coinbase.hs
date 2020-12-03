{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Xoken.Transaction.Coinbase
Copyright   : Xoken Labs
License     : Open BSV License

Stability   : experimental
Portability : POSIX

Code related to creating coinbase transactions.
-}
module Network.Xoken.Transaction.Coinbase
  ( makeCoinbaseTx
  , makeCoinbaseMsg
  , putBlockHeight
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Serialize as S
import Data.Word (Word32, Word64)
import Network.Xoken.Address
import Network.Xoken.Transaction.Common

makeCoinbaseTx :: Word32 -> Address -> Tx
makeCoinbaseTx ht addr =
  let txin = TxIn nullOutPoint inputBS maxBound
      txout = TxOut 5000000000 (addressToScriptBS addr)
      inputBS = makeCoinbaseMsg $ fromIntegral ht
   in Tx 2 [txin] [txout] 0

makeCoinbaseMsg :: Word64 -> ByteString
makeCoinbaseMsg ht = runPut $ putBlockHeight ht

putBlockHeight x
  | x <= 0xff
            --putWord8 0x02
   = do
    putWord8 0x01
    putWord8 $ fromIntegral x
  | x <= 0xffff
            --putWord8 0x03
   = do
    putWord8 0x02
    putWord16le $ fromIntegral x
  | x <= 0xffffff
            --putWord8 0x03
   = do
    let w24 = B.init $ runPut $ putWord32le $ fromIntegral x
    putWord8 0x03
    putByteString w24
  | x <= 0xffffffff
            --putWord8 0x05
   = do
    putWord8 0x04
    putWord32le $ fromIntegral x
  | otherwise
            --putWord8 0x09
   = do
    putWord8 0x08
    putWord64le $ fromIntegral x

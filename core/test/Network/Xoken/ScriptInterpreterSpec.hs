module Network.Xoken.ScriptInterpreterSpec
  ( spec
  )
where

import           Data.Word                      ( Word8 )
import qualified Data.Serialize                as S
import qualified Data.ByteString               as BS
import qualified Data.Sequence                 as Seq
import           Test.Hspec
import           Network.Xoken.Script.Common
import           Network.Xoken.Script.Interpreter

spec :: Spec
spec = do
  describe "interpret whole script" $ do
    it "returns [] given []" $ test [] []
    it "returns [1] given [OP_1]" $ test [OP_1] [1]
    it "returns [] given [OP_1, OP_DROP] " $ test [OP_1, OP_DROP] []
    it "returns [1, 1] given [OP_1, OP_DUP]" $ test [OP_1, OP_DUP] [1, 1]
    it "returns [2] given [OP_1, OP_2, OP_NIP]" $ test [OP_1, OP_2, OP_NIP] [2]
    it "returns [1, 2, 1] given [OP_1, OP_2, OP_OVER]"
      $ test [OP_1, OP_2, OP_OVER] [1, 2, 1]
    it "returns [3] given [OP_1, OP_2, OP_ADD]" $ test [OP_1, OP_2, OP_ADD] [3]
    it "returns [-1] given [OP_3, OP_4, OP_SUB]"
      $ test [OP_3, OP_4, OP_SUB] [-1]
    it "returns [0] given [OP_1, OP_2, OP_3, OP_WITHIN]"
      $ test [OP_1, OP_2, OP_3, OP_WITHIN] [0]
    it "returns [1] given [OP_2, OP_1, OP_3, OP_WITHIN]"
      $ test [OP_2, OP_1, OP_3, OP_WITHIN] [1]
    it "returns [16] given [OP_1, OP_4, OP_LSHIFT]"
      $ test [OP_1, OP_4, OP_LSHIFT] [16]
    it "returns [8] given [OP_16, OP_2DIV]" $ test [OP_16, OP_2DIV] [8]
  describe "interpret failure" $ do
    it "returns StackUnderflow given [OP_DROP]"
      $          interpret (Script [OP_DROP])
      `shouldBe` (Seq.empty, Just StackUnderflow)
    it "returns NoDecoding given [OP_PUSHDATA BS.empty OPDATA1]"
      $          interpret (Script [OP_PUSHDATA BS.empty OPDATA1])
      `shouldBe` (Seq.empty, Just $ NoDecoding 1 BS.empty)
    it "returns NoDecoding given [OP_PUSHDATA BS.empty OPDATA2]"
      $          interpret (Script [OP_PUSHDATA BS.empty OPDATA2])
      `shouldBe` (Seq.empty, Just $ NoDecoding 2 BS.empty)
    it "returns NoDecoding given [OP_PUSHDATA BS.empty OPDATA4]"
      $          interpret (Script [OP_PUSHDATA BS.empty OPDATA4])
      `shouldBe` (Seq.empty, Just $ NoDecoding 4 BS.empty)
    it "returns NotEnoughBytes given [OP_PUSHDATA (BS.pack [2, 0]) OPDATA1]"
      $          interpret (Script [OP_PUSHDATA (BS.pack [2, 0]) OPDATA1])
      `shouldBe` (Seq.empty, Just $ NotEnoughBytes { expected = 2, actual = 1 })
    it
        "returns TooMuchToLShift given\
        \[ OP_1\
        \, OP_PUSHDATA (S.encode $ succ (fromIntegral (maxBound :: Int) :: Integer)) OPCODE\
        \, OP_LSHIFT\
        \]"
      $          interpret
                   (Script
                     [ OP_1
                     , OP_PUSHDATA
                       (S.encode $ succ (fromIntegral (maxBound :: Int) :: Integer))
                       OPCODE
                     , OP_LSHIFT
                     ]
                   )
      `shouldBe` (Seq.empty, Just $ TooMuchToLShift 9223372036854775808)

test ops expected_elems =
  interpret (Script ops) `shouldBe` (Seq.fromList expected_elems, Nothing)



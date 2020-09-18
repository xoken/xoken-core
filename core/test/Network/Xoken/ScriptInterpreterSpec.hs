module Network.Xoken.ScriptInterpreterSpec
  ( spec
  )
where

import           Data.List                      ( intercalate )
import           Data.Word                      ( Word8 )
import           Data.ByteString.Builder        ( toLazyByteString
                                                , byteStringHex
                                                )
import qualified Data.ByteString               as BS
import qualified Data.Sequence                 as Seq
import           Test.Hspec
import           Network.Xoken.Script.Common
import           Network.Xoken.Script.Interpreter
import           Network.Xoken.Script.Interpreter.Commands
import           Network.Xoken.Script.Interpreter.OpenSSL_BN

spec :: Spec
spec = do
  describe "interpret whole script" $ do
    test []                            []
    test [OP_1]                        [1]
    test [OP_1, OP_2]                  [1, 2]
    test [OP_1, OP_DROP]               []
    test [OP_1, OP_DUP]                [1, 1]
    test [OP_1, OP_2, OP_NIP]          [2]
    test [OP_1, OP_2, OP_OVER]         [1, 2, 1]
    test [OP_1, OP_0, OP_PICK]         [1, 1]
    test [OP_1, OP_2, OP_1, OP_PICK]   [1, 2, 1]
    test [OP_1, OP_0, OP_ROLL]         [1]
    test [OP_1, OP_2, OP_1, OP_ROLL]   [2, 1]
    test [OP_1, OP_TOALTSTACK, OP_FROMALTSTACK] [1]
    test [OP_1, OP_TOALTSTACK, OP_2, OP_FROMALTSTACK] [2, 1]
    test [OP_1, OP_2, OP_ADD]          [3]
    test [OP_3, OP_4, OP_SUB]          [-1]
    test [OP_1, OP_2, OP_3, OP_WITHIN] [0]
    test [OP_2, OP_1, OP_3, OP_WITHIN] [1]
    test [OP_1, OP_4, OP_LSHIFT]       [16]
    test [OP_16, OP_2DIV]              [8]
  describe "interpret failure" $ do
    it "returns StackUnderflow given [OP_DROP]"
      $          interpret (Script [OP_DROP])
      `shouldBe` (empty_env, Just StackUnderflow)
    it "returns NoDecoding given [OP_PUSHDATA BS.empty OPDATA1]"
      $          interpret (Script [OP_PUSHDATA BS.empty OPDATA1])
      `shouldBe` (empty_env, Just $ NoDecoding 1 BS.empty)
    it "returns NoDecoding given [OP_PUSHDATA BS.empty OPDATA2]"
      $          interpret (Script [OP_PUSHDATA BS.empty OPDATA2])
      `shouldBe` (empty_env, Just $ NoDecoding 2 BS.empty)
    it "returns NoDecoding given [OP_PUSHDATA BS.empty OPDATA4]"
      $          interpret (Script [OP_PUSHDATA BS.empty OPDATA4])
      `shouldBe` (empty_env, Just $ NoDecoding 4 BS.empty)
    it "returns NotEnoughBytes given [OP_PUSHDATA (BS.pack [2, 0]) OPDATA1]"
      $          interpret (Script [OP_PUSHDATA (BS.pack [2, 0]) OPDATA1])
      `shouldBe` (empty_env, Just $ NotEnoughBytes { expected = 2, actual = 1 })
    unbalancedConditional [OP_IF]
    unbalancedConditional [OP_0, OP_IF, OP_ELSE, OP_ELSE, OP_ENDIF]
    unbalancedConditional [OP_ELSE]
    unbalancedConditional [OP_ENDIF]
    unbalancedConditional
      [OP_1, OP_IF, OP_IF, OP_ENDIF, OP_ELSE, OP_2, OP_ENDIF]
    unbalancedConditional
      [OP_0, OP_IF, OP_1, OP_ELSE, OP_IF, OP_ENDIF, OP_ENDIF]
    it "returns InvalidAltstackOperation given [OP_FROMALTSTACK]"
      $          interpret (Script [OP_FROMALTSTACK])
      `shouldBe` (empty_env, Just InvalidAltstackOperation)
    it "returns InvalidNumberRange given [OP_1, OP_1NEGATE, OP_LSHIFT]"
      $          interpret (Script [OP_1, OP_1NEGATE, OP_LSHIFT])
      `shouldBe` (empty_env, Just InvalidNumberRange)
    it "returns InvalidOperandSize given [OP_0, OP_1, OP_AND]"
      $          interpret (Script [OP_0, OP_1, OP_AND])
      `shouldBe` (empty_env, Just InvalidOperandSize)
  describe "interpret control flow" $ do
    test [OP_0, OP_IF, OP_ENDIF]                      []
    test [OP_0, OP_IF, OP_ELSE, OP_ENDIF]             []
    test [OP_1, OP_IF, OP_0, OP_ENDIF]                [0]
    test [OP_0, OP_IF, OP_1, OP_ELSE, OP_2, OP_ENDIF] [2]
    test [OP_1, OP_IF, OP_1, OP_ELSE, OP_2, OP_ENDIF] [1]
    test [OP_0, OP_IF, OP_IF, OP_ENDIF, OP_ELSE, OP_2, OP_ENDIF] [2]
    test [OP_1, OP_IF, OP_1, OP_ELSE, OP_IF, OP_ENDIF, OP_ENDIF] [1]
  describe "BN conversion" $ do
    it "encode 0" $ bin 0 `shouldBe` BS.pack []
    it "encode 1" $ bin 1 `shouldBe` BS.pack [1]
    it "encode -1" $ bin (-1) `shouldBe` BS.pack [129]
    it "encode 256" $ bin 256 `shouldBe` BS.pack [0, 1]
    it "encode -256" $ bin (-256) `shouldBe` BS.pack [128, 1]
    it "decode 0" $ num (BS.pack []) `shouldBe` 0
    it "decode 1" $ num (BS.pack [1]) `shouldBe` 1
    it "decode -1" $ num (BS.pack [129]) `shouldBe` -1
    it "decode 256" $ num (BS.pack [0, 1]) `shouldBe` 256
    it "decode -256" $ num (BS.pack [128, 1]) `shouldBe` -256
  describe "Data manipulation" $ do
    testBS []               [OP_0, OP_0, OP_CAT] [[]]
    testBS [[0x12], [0x34]] [OP_CAT]             [[0x12, 0x34]]
    testBS [[0x12, 0x34]]   [OP_1, OP_SPLIT]     [[0x12], [0x34]]
  describe "Bitwise logic" $ do
    testBS [[0x12, 0x34]]   [OP_INVERT] [[0xED, 0xCB]]
    testBS [[0x12], [0x34]] [OP_AND]    [[0x10]]
    testBS [[0x12], [0x34]] [OP_OR]     [[0x36]]
    testBS [[0x12], [0x34]] [OP_XOR]    [[0x26]]
    test [OP_1, OP_1, OP_EQUAL] [1]
    test [OP_1, OP_2, OP_EQUAL] [0]

test :: [ScriptOp] -> [BN] -> SpecWith (Arg Expectation)
test ops expected_elems =
  it ("returns " ++ show expected_elems ++ " given " ++ show ops)
    $          interpret (Script ops)
    `shouldBe` ( empty_env { stack = Seq.fromList $ bin <$> expected_elems }
               , Nothing
               )

testBS :: [[Word8]] -> [ScriptOp] -> [[Word8]] -> SpecWith (Arg Expectation)
testBS push_elems ops expected_elems =
  it ("returns " ++ show (hex <$> expected) ++ " given " ++ show_ops)
    $          (hex <$> stack (fst $ interpret $ Script all_ops))
    `shouldBe` Seq.fromList (hex <$> expected)
 where
  packed   = BS.pack <$> push_elems
  expected = BS.pack <$> expected_elems
  all_ops  = map (\xs -> OP_PUSHDATA xs OPCODE) packed ++ ops
  showpush xs = "OP_PUSHDATA " ++ show (hex xs)
  show_ops =
    "[" ++ intercalate "," (map showpush packed ++ map show ops) ++ "]"

unbalancedConditional :: [ScriptOp] -> SpecWith (Arg Expectation)
unbalancedConditional ops =
  it ("returns UnbalancedConditional given " ++ show ops)
    $          snd (interpret (Script ops))
    `shouldBe` (Just UnbalancedConditional)

hex = toLazyByteString . byteStringHex
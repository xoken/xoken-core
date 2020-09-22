module Network.Xoken.ScriptInterpreterSpec
  ( spec
  )
where

import           Data.List                      ( intercalate )
import           Data.Word                      ( Word8 )
import           Data.EnumBitSet                ( fromEnums )
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

env = empty_env
  { script_flags = fromEnums
                     [ GENESIS
                     , UTXO_AFTER_GENESIS
                     , VERIFY_MINIMALIF
                     , VERIFY_DISCOURAGE_UPGRADABLE_NOPS
                     ]
  }
interpret = interpretWith env

spec :: Spec
spec = do
  describe "interpret whole script" $ do
    test []                          []
    test [OP_1]                      [1]
    test [OP_1, OP_2]                [1, 2]
    test [OP_1, OP_DROP]             []
    test [OP_1, OP_DUP]              [1, 1]
    test [OP_1, OP_2, OP_NIP]        [2]
    test [OP_1, OP_2, OP_OVER]       [1, 2, 1]
    test [OP_1, OP_0, OP_PICK]       [1, 1]
    test [OP_1, OP_2, OP_1, OP_PICK] [1, 2, 1]
    test [OP_1, OP_0, OP_ROLL]       [1]
    test [OP_1, OP_2, OP_1, OP_ROLL] [2, 1]
    test [OP_1, OP_TOALTSTACK, OP_FROMALTSTACK] [1]
    test [OP_1, OP_TOALTSTACK, OP_2, OP_FROMALTSTACK] [2, 1]
    testNoFlags Nothing         [OP_NOP1]
    testNoFlags (Just OpReturn) [OP_RETURN]
  describe "interpret failure" $ do
    terminatesWith StackUnderflow          [OP_DROP]
    terminatesWith (NoDecoding 1 BS.empty) [OP_PUSHDATA BS.empty OPDATA1]
    terminatesWith (NoDecoding 2 BS.empty) [OP_PUSHDATA BS.empty OPDATA2]
    terminatesWith (NoDecoding 4 BS.empty) [OP_PUSHDATA BS.empty OPDATA4]
    terminatesWith (NotEnoughBytes { expected = 2, actual = 1 })
                   [OP_PUSHDATA (BS.pack [2, 0]) OPDATA1]
    terminatesWith UnbalancedConditional [OP_IF]
    terminatesWith UnbalancedConditional
                   [OP_0, OP_IF, OP_ELSE, OP_ELSE, OP_ENDIF]
    terminatesWith UnbalancedConditional [OP_ELSE]
    terminatesWith UnbalancedConditional [OP_ENDIF]
    terminatesWith UnbalancedConditional
                   [OP_1, OP_IF, OP_IF, OP_ENDIF, OP_ELSE, OP_2, OP_ENDIF]
    terminatesWith UnbalancedConditional
                   [OP_0, OP_IF, OP_1, OP_ELSE, OP_IF, OP_ENDIF, OP_ENDIF]
    terminatesWith UnbalancedConditional    [OP_0, OP_RETURN, OP_ELSE]
    terminatesWith InvalidAltstackOperation [OP_FROMALTSTACK]
    terminatesWith InvalidNumberRange       [OP_1, OP_1NEGATE, OP_LSHIFT]
    terminatesWith InvalidOperandSize       [OP_0, OP_1, OP_AND]
    terminatesWith MinimalIf                [OP_2, OP_IF, OP_ENDIF]
    terminatesWith MinimalIf                [opPushData (BS.pack [1, 2]), OP_IF]
    terminatesWith DiscourageUpgradableNOPs [OP_NOP1]
  describe "interpret control flow" $ do
    test [OP_0, OP_IF, OP_ENDIF]                      []
    test [OP_0, OP_IF, OP_ELSE, OP_ENDIF]             []
    test [OP_1, OP_IF, OP_0, OP_ENDIF]                [0]
    test [OP_0, OP_IF, OP_1, OP_ELSE, OP_2, OP_ENDIF] [2]
    test [OP_1, OP_IF, OP_1, OP_ELSE, OP_2, OP_ENDIF] [1]
    test [OP_0, OP_IF, OP_IF, OP_ENDIF, OP_ELSE, OP_2, OP_ENDIF] [2]
    test [OP_1, OP_IF, OP_1, OP_ELSE, OP_IF, OP_ENDIF, OP_ENDIF] [1]
    test [OP_RETURN, OP_ELSE]                         []
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
    testBS []           [OP_0, OP_0, OP_CAT] [0x0]
    testBS [0x12, 0x34] [OP_CAT]             [0x1234]
    testBS [0x1234]     [OP_1, OP_SPLIT]     [0x12, 0x34]
  describe "Bitwise logic" $ do
    testBS [0x1234]     [OP_INVERT] [0xEDCB]
    testBS [0x12, 0x34] [OP_AND]    [0x10]
    testBS [0x12, 0x34] [OP_OR]     [0x36]
    testBS [0x12, 0x34] [OP_XOR]    [0x26]
    test [OP_1, OP_1, OP_EQUAL] [1]
    test [OP_1, OP_2, OP_EQUAL] [0]
  describe "Arithmetic" $ do
    test [OP_1, OP_2, OP_ADD]          [3]
    test [OP_3, OP_4, OP_SUB]          [-1]
    test [OP_1, OP_2, OP_3, OP_WITHIN] [0]
    test [OP_2, OP_1, OP_3, OP_WITHIN] [1]
    test [OP_1, OP_4, OP_LSHIFT]       [16]
    test [OP_16, OP_2DIV]              [8]
    terminatesWith DivByZero [OP_0, OP_0, OP_DIV]
    terminatesWith ModByZero [OP_0, OP_0, OP_MOD]
  describe "Crypto" $ do
    testBS [0x1234] [OP_RIPEMD160] [0xC39867E393CB061B837240862D9AD318C176A96D]
    testBS [0x1234] [OP_SHA1]      [0xFFA76D854A2969E7B9D83868D455512FCE0FD74D]
    testBS
      [0x1234]
      [OP_SHA256]
      [0x3A103A4E5729AD68C02A678AE39ACCFBC0AE208096437401B7CEAB63CCA0622F]
    testBS [0x1234] [OP_HASH160] [0xC9440A6FF7E66BE6CE4A3524A7C44F292A631533]
    testBS
      [0x1234]
      [OP_HASH256]
      [0xA23421F2BA909C885A3077BB6F8EB4312487797693BBCFE7E311F797E3C5B8FA]

test :: [ScriptOp] -> [BN] -> SpecWith (Arg Expectation)
test ops expected_elems =
  it ("returns " ++ show expected_elems ++ " given " ++ show ops)
    $          interpret (Script ops)
    `shouldBe` (env { stack = Seq.fromList $ bin <$> expected_elems }, Nothing)

ftestBS
  :: (a -> BS.ByteString)
  -> [a]
  -> [ScriptOp]
  -> [a]
  -> SpecWith (Arg Expectation)
ftestBS f push_elems ops expected_elems =
  it ("returns " ++ show (hex <$> expected) ++ " given " ++ show_ops)
    $          (hex <$> stack (fst $ interpret $ Script all_ops))
    `shouldBe` Seq.fromList (hex <$> expected)
 where
  packed   = f <$> push_elems
  expected = f <$> expected_elems
  all_ops  = map opPushData packed ++ ops
  showpush xs = "OP_PUSHDATA " ++ show (hex xs)
  show_ops =
    "[" ++ intercalate "," (map showpush packed ++ map show ops) ++ "]"

testBS :: [Integer] -> [ScriptOp] -> [Integer] -> SpecWith (Arg Expectation)
testBS = ftestBS (BS.reverse . BS.pack . unroll)

terminatesWith :: InterpreterError -> [ScriptOp] -> SpecWith (Arg Expectation)
terminatesWith error ops =
  it ("returns " ++ show error ++ " given " ++ show ops)
    $          snd (interpret (Script ops))
    `shouldBe` (Just error)

testNoFlags
  :: Maybe InterpreterError -> [ScriptOp] -> SpecWith (Arg Expectation)
testNoFlags r ops =
  it ("returns [] given " ++ show ops ++ " and no flags")
    $          interpretWith empty_env (Script ops)
    `shouldBe` (empty_env, r)

hex = toLazyByteString . byteStringHex

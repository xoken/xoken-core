module Network.Xoken.ScriptInterpreterSpec
  ( spec
  )
where

import           Data.Bits                      ( shiftL
                                                , shiftR
                                                )
import           Data.Foldable                  ( toList )
import           Data.List                      ( intercalate )
import           Data.Word                      ( Word8 )
import           Data.EnumBitSet                ( fromEnums
                                                , (.|.)
                                                )
import           Data.ByteString.Builder        ( toLazyByteString
                                                , byteStringHex
                                                )
import qualified Data.ByteString               as BS
import qualified Data.Sequence                 as Seq
import           Test.Hspec
import           Test.QuickCheck
import           Network.Xoken.Script.Common
import           Network.Xoken.Script.Interpreter
import           Network.Xoken.Script.Interpreter.Commands
import           Network.Xoken.Script.Interpreter.Util
import           Network.Xoken.Test

sigChecker :: BaseSignatureChecker
sigChecker = txSigChecker net txTo nIn amount inputIndex where
  net        = undefined
  txTo       = undefined
  nIn        = undefined
  amount     = undefined
  inputIndex = undefined

env :: Script -> Env
env script = (empty_env script sigChecker)
  { script_flags = fromEnums [GENESIS, UTXO_AFTER_GENESIS, VERIFY_MINIMALIF]
                   .|. mandatoryScriptFlags
                   .|. standardScriptFlags
  }

interpret :: Script -> (Env, Maybe InterpreterError)
interpret = interpretWith . env

spec :: Spec
spec = do
  describe "pushdata" $ do
    let opcode_xs  = BS.pack $ replicate 0x4b 1
        opdata1_xs = BS.pack $ replicate 0xff 1
        opdata2_xs = BS.pack $ replicate 0xffff 1

    testNoFlags Nothing [OP_PUSHDATA opcode_xs OPCODE]
    testNoFlags Nothing [OP_PUSHDATA opcode_xs OPDATA1]
    testNoFlags Nothing [OP_PUSHDATA opcode_xs OPDATA2]
    testNoFlags Nothing [OP_PUSHDATA opcode_xs OPDATA4]

    testPack [] [OP_PUSHDATA opcode_xs OPCODE] [BS.unpack opcode_xs]
    terminatesWith MinimalData [OP_PUSHDATA opcode_xs OPDATA1]
    terminatesWith MinimalData [OP_PUSHDATA opcode_xs OPDATA2]
    terminatesWith MinimalData [OP_PUSHDATA opcode_xs OPDATA4]

    testNoFlags Nothing [OP_PUSHDATA opdata1_xs OPDATA1]
    testNoFlags Nothing [OP_PUSHDATA opdata1_xs OPDATA2]
    testNoFlags Nothing [OP_PUSHDATA opdata1_xs OPDATA4]

    testPack [] [OP_PUSHDATA opdata1_xs OPDATA1] [BS.unpack opdata1_xs]
    terminatesWith MinimalData [OP_PUSHDATA opdata1_xs OPDATA2]
    terminatesWith MinimalData [OP_PUSHDATA opdata1_xs OPDATA4]

    testNoFlags (Just PushSize) [OP_PUSHDATA opdata2_xs OPDATA2]
    testNoFlags (Just PushSize) [OP_PUSHDATA opdata2_xs OPDATA4]

    testPack [] [OP_PUSHDATA opdata2_xs OPDATA2] [BS.unpack opdata2_xs]
    terminatesWith MinimalData [OP_PUSHDATA opdata2_xs OPDATA4]
  describe "stack" $ do
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
    terminatesWith InvalidAltstackOperation [OP_FROMALTSTACK]
    test [OP_1, OP_TOALTSTACK, OP_FROMALTSTACK]       [1]
    test [OP_1, OP_TOALTSTACK, OP_2, OP_FROMALTSTACK] [2, 1]
    terminatesWith StackUnderflow [OP_DROP]
  describe "interpreter extra" $ do
    testNoFlags Nothing [OP_NOP1]
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
    testNoFlags (Just OpReturn) [OP_RETURN]
    terminatesWith UnbalancedConditional [OP_IF]
    terminatesWith UnbalancedConditional
                   [OP_0, OP_IF, OP_ELSE, OP_ELSE, OP_ENDIF]
    terminatesWith UnbalancedConditional [OP_ELSE]
    terminatesWith UnbalancedConditional [OP_ENDIF]
    terminatesWith UnbalancedConditional
                   [OP_1, OP_IF, OP_IF, OP_ENDIF, OP_ELSE, OP_2, OP_ENDIF]
    terminatesWith UnbalancedConditional
                   [OP_0, OP_IF, OP_1, OP_ELSE, OP_IF, OP_ENDIF, OP_ENDIF]
    terminatesWith UnbalancedConditional [OP_0, OP_RETURN, OP_ELSE]
    terminatesWith MinimalIf             [OP_2, OP_IF, OP_ENDIF]
    terminatesWith MinimalIf             [opPushData (BS.pack [1, 2]), OP_IF]
  describe "BN conversion" $ do
    it "encode 0" $ bin 0 `shouldBe` BS.pack []
    it "encode 1" $ bin 1 `shouldBe` BS.pack [1]
    it "encode -1" $ bin (-1) `shouldBe` BS.pack [129]
    it "encode 256" $ bin 256 `shouldBe` BS.pack [0, 1]
    it "encode -256" $ bin (-256) `shouldBe` BS.pack [0, 129]
    it "decode 0" $ num (BS.pack []) `shouldBe` 0
    it "decode 1" $ num (BS.pack [1]) `shouldBe` 1
    it "decode -1" $ num (BS.pack [129]) `shouldBe` -1
    it "decode 256" $ num (BS.pack [0, 1]) `shouldBe` 256
    it "decode -256" $ num (BS.pack [0, 129]) `shouldBe` -256
    terminatesWith PushSize [OP_1, OP_1NEGATE, OP_NUM2BIN]
  describe "Data manipulation" $ do
    testBS []           [OP_0, OP_0, OP_CAT] [0x0]
    testBS [0x12, 0x34] [OP_CAT]             [0x1234]
    testBS [0x1234]     [OP_1, OP_SPLIT]     [0x12, 0x34]
    terminatesWith InvalidSplitRange
                   [opPushData $ rawNumToBS 0x1234, OP_1NEGATE, OP_SPLIT]
    terminatesWith InvalidSplitRange
                   [opPushData $ rawNumToBS 0x1234, OP_3, OP_SPLIT]
  describe "Bitwise logic" $ do
    testBS [0x1234]     [OP_INVERT] [0xEDCB]
    testBS [0x12, 0x34] [OP_AND]    [0x10]
    testBS [0x12, 0x34] [OP_OR]     [0x36]
    testBS [0x12, 0x34] [OP_XOR]    [0x26]
    test [OP_1, OP_1, OP_EQUAL] [1]
    test [OP_1, OP_2, OP_EQUAL] [0]
    terminatesWith InvalidOperandSize [OP_0, OP_1, OP_AND]
  describe "Arithmetic" $ do
    it "performs OP_1ADD on OP_1..16" $ unary_arithmetic OP_1ADD succ
    it "performs OP_1SUB on OP_1..16" $ unary_arithmetic OP_1SUB pred
    it "performs OP_NEGATE on OP_1..16" $ unary_arithmetic OP_NEGATE negate
    it "performs OP_ABS on OP_1..16" $ unary_arithmetic OP_ABS abs
    it "performs OP_NOT on OP_0"
      $ success_with_elem_check [OP_0, OP_NOT]
      $ \elems -> num <$> elems `shouldBe` [1]
    it "performs OP_NOT on OP_1..16" $ unary_arithmetic OP_NOT (truth . (== 0))
    it "performs OP_0NOTEQUAL on OP_0"
      $ success_with_elem_check [OP_0, OP_0NOTEQUAL]
      $ \elems -> num <$> elems `shouldBe` [0]
    it "performs OP_0NOTEQUAL on OP_1..16"
      $ unary_arithmetic OP_0NOTEQUAL (truth . (/= 0))
    it "performs OP_ADD on OP_1..16" $ binary_arithmetic OP_ADD (+)
    it "performs OP_SUB on OP_1..16" $ binary_arithmetic OP_SUB (-)
    it "performs OP_MUL on OP_1..16" $ binary_arithmetic OP_MUL (*)
    it "performs OP_DIV on OP_1..16" $ binary_arithmetic OP_DIV div
    it "performs OP_MOD on OP_1..16" $ binary_arithmetic OP_MOD mod
    it "performs OP_LSHIFT on OP_1..16" $ test_shift OP_LSHIFT shiftL
    it "performs OP_RSHIFT on OP_1..16" $ test_shift OP_RSHIFT shiftR
    testPack [[], [64]]     [OP_LSHIFT] [[]]
    testPack [[], [64]]     [OP_RSHIFT] [[]]
    testPack [[1], [64]]    [OP_LSHIFT] [[0]]
    testPack [[1], [64]]    [OP_RSHIFT] [[0]]
    testPack [[1, 2], [64]] [OP_LSHIFT] [[0, 0]]
    testPack [[1, 2], [64]] [OP_RSHIFT] [[0, 0]]
    testPack [[1], [7]]     [OP_LSHIFT] [[128]]
    testPack [[1], [8]]     [OP_LSHIFT] [[0]]
    terminatesWith DivByZero          [OP_0, OP_0, OP_DIV]
    terminatesWith ModByZero          [OP_0, OP_0, OP_MOD]
    terminatesWith InvalidNumberRange [OP_1, OP_1NEGATE, OP_LSHIFT]
    it "performs OP_BOOLAND on OP_1..16"
      $ binary_arithmetic OP_BOOLAND (\a b -> truth (a /= 0 && b /= 0))
    it "performs OP_BOOLOR on OP_1..16"
      $ binary_arithmetic OP_BOOLOR (\a b -> truth (a /= 0 || b /= 0))
    it "performs OP_RSHIFT on OP_1..16" $ binary_arithmetic OP_RSHIFT shiftR
    it "performs OP_NUMEQUAL on OP_1..16"
      $ binary_arithmetic OP_NUMEQUAL (btruth (==))
    it "performs OP_NUMEQUALVERIFY on OP_1..16"
      $ property
      $ forAll arbitraryIntScriptOp
      $ \a_op -> forAll arbitraryIntScriptOp $ \b_op -> do
          let (env, error) = interpret (Script [a_op, b_op, OP_NUMEQUALVERIFY])
          if a_op == b_op
            then do
              error `shouldBe` Nothing
              elems env `shouldBe` []
            else error `shouldBe` Just NumEqualVerify
    it "performs OP_NUMNOTEQUAL on OP_1..16"
      $ binary_arithmetic OP_NUMNOTEQUAL (btruth (/=))
    it "performs OP_LESSTHAN on OP_1..16"
      $ binary_arithmetic OP_LESSTHAN (btruth (<))
    it "performs OP_GREATERTHAN on OP_1..16"
      $ binary_arithmetic OP_GREATERTHAN (btruth (>))
    it "performs OP_LESSTHANOREQUAL on OP_1..16"
      $ binary_arithmetic OP_LESSTHANOREQUAL (btruth (<=))
    it "performs OP_GREATERTHANOREQUAL on OP_1..16"
      $ binary_arithmetic OP_GREATERTHANOREQUAL (btruth (>=))
    it "performs OP_MIN on OP_1..16" $ binary_arithmetic OP_MIN min
    it "performs OP_MAX on OP_1..16" $ binary_arithmetic OP_MAX max
    it "performs OP_WITHIN on OP_1..16"
      $ property
      $ forAll arbitraryIntScriptOp
      $ \x_op -> forAll arbitraryIntScriptOp $ \min_op ->
          forAll arbitraryIntScriptOp $ \max_op -> do
            let (env, error) =
                  interpret (Script [x_op, min_op, max_op, OP_WITHIN])
            error `shouldBe` Nothing
            case
                (scriptOpToInt x_op, scriptOpToInt min_op, scriptOpToInt max_op)
              of
                (Right x, Right min, Right max) ->
                  num <$> elems env `shouldBe` [truth (min <= x && x < max)]
                _ -> pure ()
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
test ops expected_nums =
  it ("returns " ++ show expected_nums ++ " given " ++ show ops) $ do
    elems env `shouldBe` bin <$> expected_nums
    error `shouldBe` Nothing
  where (env, error) = interpret (Script ops)

elems = toList . stack

unary_arithmetic op f = property $ forAll arbitraryIntScriptOp $ \x_op ->
  success_with_elem_check [x_op, op] $ \elems -> case scriptOpToInt x_op of
    Right x -> num <$> elems `shouldBe` [fromIntegral $ f x]
    _       -> pure ()

binary_arithmetic op f = property $ forAll arbitraryIntScriptOp $ \a_op ->
  forAll arbitraryIntScriptOp $ \b_op ->
    success_with_elem_check [a_op, b_op, op] $ \elems ->
      case (scriptOpToInt a_op, scriptOpToInt b_op) of
        (Right a, Right b) -> num <$> elems `shouldBe` [fromIntegral $ f a b]
        _                  -> pure ()

test_shift op f = property $ forAll arbitraryBS $ \bs ->
  forAll arbitraryIntScriptOp $ \n_op ->
    success_with_elem_check [opPushData bs, n_op, op] $ \elems ->
      case scriptOpToInt n_op of
        Right n ->
          elems
            `shouldBe` [ if n >= size * 8
                           then BS.pack (replicate size 0)
                           else f bs n
                       ]
          where size = BS.length bs
        _ -> pure ()

success_with_elem_check ops f = do
  let (env, error) = interpret (Script ops)
  error `shouldBe` Nothing
  f (elems env)

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
testBS = ftestBS rawNumToBS

testPack :: [[Word8]] -> [ScriptOp] -> [[Word8]] -> SpecWith (Arg Expectation)
testPack = ftestBS (BS.pack)

terminatesWith :: InterpreterError -> [ScriptOp] -> SpecWith (Arg Expectation)
terminatesWith error ops =
  it ("returns " ++ show error ++ " given " ++ show ops)
    $          snd (interpret (Script ops))
    `shouldBe` (Just error)

testNoFlags
  :: Maybe InterpreterError -> [ScriptOp] -> SpecWith (Arg Expectation)
testNoFlags r ops =
  it ("returns [] given " ++ show ops ++ " and no flags")
    $          snd (interpretWith $ empty_env (Script ops) sigChecker)
    `shouldBe` r

rawNumToBS = BS.reverse . BS.pack . unroll
hex = toLazyByteString . byteStringHex

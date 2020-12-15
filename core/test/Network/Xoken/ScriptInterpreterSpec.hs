module Network.Xoken.ScriptInterpreterSpec
  ( spec
  )
where

import           Data.Bits                      ( shiftL
                                                , shiftR
                                                )
import           Data.Foldable                  ( toList )
import           Data.List                      ( intercalate )
import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Data.EnumBitSet                ( fromEnums
                                                , (.|.)
                                                , get
                                                , empty
                                                )
import           Data.ByteString.Builder        ( toLazyByteString
                                                , byteStringHex
                                                )
import qualified Data.Bits                     as B
import qualified Data.ByteString               as BS
import qualified Data.Sequence                 as Seq
import           Test.Hspec
import           Test.QuickCheck
import           Network.Xoken.Script.Common
import           Network.Xoken.Script.Interpreter
import           Network.Xoken.Script.Interpreter.Commands
import           Network.Xoken.Script.Interpreter.Util
import           Network.Xoken.Test

default_ctx = Ctx
  { script_flags     = fromEnums [GENESIS, UTXO_AFTER_GENESIS, VERIFY_MINIMALIF]
                       .|. mandatoryScriptFlags
                       .|. standardScriptFlags
  , consensus        = True
  , sig_checker_data = undefined
  }

opPush = opPushData . rawNumToBS

spec :: Spec
spec = do
  describe "verify script" $ do
    it "empty scripts" $ forAll arbitrary $ \ctx ->
      verifyScriptWith ctx empty_env (Script []) (Script [])
        `shouldBe` Just EvalFalse
  describe "interpreter dependencies" $ do
    it "still has wrong shiftR"
      $          (BS.pack [1, 2, 3] `shiftR` 8)
      `shouldBe` BS.pack [0, 2, 3]
    it "shifts with fixed_shiftR" $ do
      (BS.pack [1, 2, 3] `fixed_shiftR` 8) `shouldBe` BS.pack [0, 1, 2]
      (BS.pack [1, 2, 3] `shiftL` 8) `shouldBe` BS.pack [2, 3, 0]
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
    it "performs OP_TOALTSTACK on arbitrary data" $ forAll arbitrary $ \ctx ->
      forAll (listOf arbitraryBS) $ \elems ->
        forAll (listOf arbitraryBS) $ \alt_elems -> forAll arbitraryBS $ \bs ->
          test_script_with
              ctx
              ( stack_equal (Seq.fromList $ elems ++ [bs])
              . alt_stack_equal (Seq.fromList alt_elems)
              )
              [OP_TOALTSTACK]
            $ success_with_alt_elem_check (`shouldBe` bs : alt_elems)
    it "performs OP_FROMALTSTACK on arbitrary data"
      $ forAll arbitrary
      $ \ctx -> forAll (listOf arbitraryBS) $ \elems ->
          forAll (listOf arbitraryBS) $ \alt_elems ->
            test_script_with
                ctx
                ( stack_equal (Seq.fromList elems)
                . alt_stack_equal (Seq.fromList alt_elems)
                )
                [OP_FROMALTSTACK]
              $ case alt_elems of
                  x : _ -> success_with_elem_check (`shouldBe` elems ++ [x])
                  _     -> const (`shouldBe` Just InvalidAltstackOperation)
    arrange_test OP_2DROP 2 (\[a, b] -> [])
    arrange_test OP_2DUP  2 (\[a, b] -> [a, b, a, b])
    arrange_test OP_3DUP  3 (\[a, b, c] -> [a, b, c, a, b, c])
    arrange_test OP_2OVER 4 (\[a, b, c, d] -> [a, b, c, d, a, b])
    arrange_test OP_2ROT  6 (\[a, b, c, d, e, f] -> [c, d, e, f, a, b])
    arrange_test OP_2SWAP 4 (\[a, b, c, d] -> [c, d, a, b])
    arrange_test OP_IFDUP 1 (\[a] -> if num a /= 0 then [a, a] else [a])
    it "performs OP_DEPTH on arbitrary data" $ forAll arbitrary $ \ctx ->
      forAll (listOf arbitraryBS) $ \elems ->
        test_script_with ctx (stack_equal $ Seq.fromList elems) [OP_DEPTH]
          $ success_with_elem_check
              (`shouldBe` elems ++ [int2BS $ length elems])
    arrange_test OP_DROP 1 (\[a] -> [])
    arrange_test OP_DUP  1 (\[a] -> [a, a])
    arrange_test OP_NIP  2 (\[a, b] -> [b])
    arrange_test OP_OVER 2 (\[a, b] -> [a, b, a])
    it "performs OP_PICK on arbitrary data"
      $ forAll (listOf arbitraryBS)
      $ \elems -> forAll (arbitrary :: Gen Word32) $ \i -> do
          let n = fromIntegral i
          test_script_with default_ctx
                           (stack_equal $ Seq.fromList elems)
                           [opPushData (int2BS n), OP_PICK]
            $ if n < length elems
                then success_with_elem_check
                  (`shouldBe` elems ++ [elems !! (length elems - 1 - n)])
                else const (`shouldBe` Just StackUnderflow)
    it "performs OP_ROLL on arbitrary data"
      $ forAll (listOf arbitraryBS)
      $ \xs -> forAll (arbitrary :: Gen Word32) $ \i -> do
          let n = fromIntegral i
          test_script_with default_ctx
                           (stack_equal $ Seq.fromList xs)
                           [opPushData (int2BS n), OP_ROLL]
            $ if n < length xs
                then do
                  let (before, x : after) = splitAt (length xs - 1 - n) xs
                  success_with_elem_check (`shouldBe` before ++ after ++ [x])
                else const (`shouldBe` Just StackUnderflow)
    arrange_test OP_ROT  3 (\[a, b, c] -> [b, c, a])
    arrange_test OP_SWAP 2 (\[a, b] -> [b, a])
    arrange_test OP_TUCK 2 (\[a, b] -> [b, a, b])
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
    it "handles double else branch" $ forAll arbitrary $ \ctx ->
      test_script_with ctx id [OP_0, OP_IF, OP_ELSE, OP_ELSE, OP_ENDIF] $ const
        (`shouldBe` if get UTXO_AFTER_GENESIS (script_flags ctx)
          then Just UnbalancedConditional
          else Nothing
        )
    terminatesWith UnbalancedConditional [OP_IF]
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
    bn_conversion 0      []
    bn_conversion 1      [1]
    bn_conversion (-1)   [129]
    bn_conversion 255    [255, 0]
    bn_conversion (-255) [255, 128]
    bn_conversion 256    [0, 1]
    bn_conversion (-256) [0, 129]
  describe "Data manipulation" $ do
    n_bs_test OP_CAT 2 $ \elems [bs1, bs2] ->
      success_with_elem_check (`shouldBe` elems ++ [BS.append bs1 bs2])
    it "performs OP_SPLIT on arbitrary data" $ forAll arbitraryBS $ \bs ->
      forAll arbitraryIntScriptOp $ \n_op -> case scriptOpToInt n_op of
        Right n ->
          test_script [opPushData bs, n_op, OP_SPLIT]
            $ if n < 0 || n > BS.length bs
                then const (`shouldBe` Just InvalidSplitRange)
                else success_with_elem_check (`shouldBe` [bs1, bs2])
          where (bs1, bs2) = BS.splitAt n bs
        _ -> pure ()
    it "performs OP_NUM2BIN on arbitrary data" $ forAll arbitrary $ \ctx ->
      forAll arbitrary $ \bn -> forAll arbitrary $ \size ->
        test_script_with ctx
                         (stack_equal $ Seq.fromList $ bin <$> [bn, size])
                         [OP_NUM2BIN]
          $ \env error -> do
              let
                genesis = get UTXO_AFTER_GENESIS $ script_flags ctx
                tooBig =
                  not genesis
                    && size
                    >  fromIntegral maxScriptElementSizeBeforeGenesis
              if size < 0 || size > fromIntegral (maxBound :: Int) || tooBig
                then error `shouldBe` Just PushSize
                else case num2binpad bn (fromIntegral size) of
                  Just x -> pure () -- elems env `shouldBe` [x]
                  _      -> error `shouldBe` Just ImpossibleEncoding
    arrange_test OP_BIN2NUM 1 (\[a] -> [bin $ num $ a])
    arrange_test OP_SIZE    1 (\[a] -> [a, int2BS $ BS.length a])
  describe "Bitwise logic" $ do
    arrange_test OP_INVERT 1 (\[a] -> [B.complement a])
    binary_bitwise_test OP_AND (B..&.)
    binary_bitwise_test OP_OR  (B..|.)
    binary_bitwise_test OP_XOR (B.xor)
    n_bs_test
      OP_EQUAL
      2
      (\elems [a, b] ->
        success_with_elem_check (`shouldBe` elems ++ [bin $ truth $ a == b])
      )
    n_bs_test
      OP_EQUALVERIFY
      2
      (\elems [a, b] -> if a == b
        then success_with_elem_check (`shouldBe` elems)
        else const (`shouldBe` Just EqualVerify)
      )
  describe "Arithmetic" $ do
    it "performs OP_1ADD on arbitrary data" $ unary_arith_success OP_1ADD succ
    it "performs OP_1SUB on arbitrary data" $ unary_arith_success OP_1SUB pred
    it "handles disabled OP_2MUL on arbitrary data" $ testDisabledOp OP_2MUL
    it "handles disabled OP_2DIV on arbitrary data" $ testDisabledOp OP_2DIV
    it "performs OP_NEGATE on arbitrary data"
      $ unary_arith_success OP_NEGATE negate
    it "performs OP_ABS on arbitrary data" $ unary_arith_success OP_ABS abs
    it "performs OP_NOT on arbitrary data"
      $ unary_arith_success OP_NOT (truth . (== 0))
    it "performs OP_0NOTEQUAL on arbitrary data"
      $ unary_arith_success OP_0NOTEQUAL (truth . (/= 0))
    it "performs OP_ADD on arbitrary data" $ binary_arith_success OP_ADD (+)
    it "performs OP_SUB on arbitrary data" $ binary_arith_success OP_SUB (-)
    it "performs OP_MUL on arbitrary data" $ binary_arith_success OP_MUL (*)
    it "performs OP_DIV on arbitrary data" $ binary_arith_test OP_DIV $ \a b ->
      if b == 0
        then const (`shouldBe` Just DivByZero)
        else success_with_elem_check $ num_check (`shouldBe` [a `div` b])
    it "performs OP_MOD on arbitrary data" $ binary_arith_test OP_MOD $ \a b ->
      if b == 0
        then const (`shouldBe` Just ModByZero)
        else success_with_elem_check $ num_check (`shouldBe` [a `mod` b])
    it "performs OP_LSHIFT on arbitrary data" $ test_shift OP_LSHIFT B.shiftL
    it "performs OP_RSHIFT on arbitrary data"
      $ test_shift OP_RSHIFT fixed_shiftR
    it "performs OP_BOOLAND on arbitrary data"
      $ binary_arith_success OP_BOOLAND (\a b -> truth (a /= 0 && b /= 0))
    it "performs OP_BOOLOR on arbitrary data"
      $ binary_arith_success OP_BOOLOR (\a b -> truth (a /= 0 || b /= 0))
    it "performs OP_NUMEQUAL on arbitrary data"
      $ binary_arith_success OP_NUMEQUAL (btruth (==))
    it "performs OP_NUMEQUALVERIFY on arbitrary data"
      $ binary_arith_test OP_NUMEQUALVERIFY
      $ \a b -> if a == b
          then success_with_elem_check (`shouldBe` [])
          else const (`shouldBe` Just NumEqualVerify)
    it "performs OP_NUMNOTEQUAL on arbitrary data"
      $ binary_arith_success OP_NUMNOTEQUAL (btruth (/=))
    it "performs OP_LESSTHAN on arbitrary data"
      $ binary_arith_success OP_LESSTHAN (btruth (<))
    it "performs OP_GREATERTHAN on arbitrary data"
      $ binary_arith_success OP_GREATERTHAN (btruth (>))
    it "performs OP_LESSTHANOREQUAL on arbitrary data"
      $ binary_arith_success OP_LESSTHANOREQUAL (btruth (<=))
    it "performs OP_GREATERTHANOREQUAL on arbitrary data"
      $ binary_arith_success OP_GREATERTHANOREQUAL (btruth (>=))
    it "performs OP_MIN on arbitrary data" $ binary_arith_success OP_MIN min
    it "performs OP_MAX on arbitrary data" $ binary_arith_success OP_MAX max
    it "performs OP_WITHIN on arbitrary data"
      $ forAll arbitraryPushOp
      $ \x_op -> forAll arbitraryPushOp $ \min_op ->
          forAll arbitraryPushOp $ \max_op ->
            case (pushOpToBN x_op, pushOpToBN min_op, pushOpToBN max_op) of
              (Right x, Right min, Right max) ->
                test_script [x_op, min_op, max_op, OP_WITHIN]
                  $ success_with_elem_check
                  $ num_check (`shouldBe` [truth (min <= x && x < max)])
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

num :: Elem -> BN
num = bin2num

test :: [ScriptOp] -> [BN] -> SpecWith (Arg Expectation)
test ops expected_nums =
  it ("returns " ++ show expected_nums ++ " given " ++ show ops)
    $ test_script ops
    $ success_with_elem_check
    $ num_check (`shouldBe` expected_nums)

arrange_test op n f = n_bs_test op n
  $ \elems bss -> success_with_elem_check (`shouldBe` elems ++ f bss)

n_bs_test op n f =
  it ("performs " ++ show op ++ " on arbitrary data")
    $ forAll (listOf arbitraryBS)
    $ \elems -> test_script_with
        default_ctx
        (stack_equal $ Seq.fromList elems)
        [op]
        (if length elems < n
          then const (`shouldBe` Just StackUnderflow)
          else uncurry f (splitAt (length elems - n) elems)
        )

test_script_with ctx change_env ops f = uncurry
  f
  (interpretWith ctx $ change_env $ script_equal (Script ops) empty_env)

bn_conversion :: BN -> [Word8] -> SpecWith (Arg Expectation)
bn_conversion n xs = do
  it ("converts BN " ++ show n) $ do
    bin n `shouldBe` BS.pack xs
    num (BS.pack xs) `shouldBe` n
  it ("converts limited BN" ++ show n) $ forAll arbitrary $ \max_size ->
    forAll arbitrary $ \require_minimal ->
      bin2num' require_minimal max_size (BS.pack xs)
        `shouldBe` if max_size < length xs then Nothing else Just n

test_script = test_script_with default_ctx id
success_with_elem_check f = success_with_env_check (f . elems)
success_with_alt_elem_check f = success_with_env_check (f . alt_elems)
success_with_env_check f env error = (error `shouldBe` Nothing) >> f env
elems = toList . stack
alt_elems = toList . alt_stack
num_check f elems = f (num <$> elems)

testDisabledOp op =
  forAll (listOf arbitraryBS) $ \elems -> forAll arbitraryBS $ \bs ->
    forAll arbitrary $ \failed -> do
      test_script_with
          default_ctx
          (stack_equal (Seq.fromList elems))
          [ if failed then OP_1 else OP_0
          , OP_IF
          , OP_ELSE
          , opPushData bs
          , op
          , OP_ENDIF
          ]
        $ if get UTXO_AFTER_GENESIS (script_flags default_ctx) && failed
            then success_with_elem_check (`shouldBe` elems)
            else const (`shouldBe` Just DisabledOpcode)

arbitraryPushOp = oneof [opPushData . bin <$> arbitrary, arbitraryIntScriptOp]

pushOpToBN (OP_PUSHDATA bs _) = Right $ num bs
pushOpToBN op                 = fromIntegral <$> scriptOpToInt op

unary_arith_success op f = forAll arbitraryPushOp $ \x_op ->
  case pushOpToBN x_op of
    Right x -> test_script [x_op, op] $ success_with_elem_check $ num_check
      (`shouldBe` [f x])
    _ -> pure ()

binary_bitwise_test op f = n_bs_test op 2 $ \elems [a, b] ->
  if BS.length a == BS.length b
    then success_with_elem_check (`shouldBe` elems ++ [f a b])
    else const (`shouldBe` Just InvalidOperandSize)

binary_arith_success op f = binary_arith_test op
  $ \a b -> success_with_elem_check $ num_check (`shouldBe` [f a b])

binary_arith_test op f = forAll arbitraryPushOp $ \a_op ->
  forAll arbitraryPushOp $ \b_op -> case (pushOpToBN a_op, pushOpToBN b_op) of
    (Right a, Right b) -> test_script [a_op, b_op, op] $ f a b
    _                  -> pure ()

test_shift op f = forAll arbitraryBS $ \bs ->
  forAll arbitraryPushOp $ \n_op -> case pushOpToBN n_op of
    Right n -> test_script [opPushData bs, n_op, op] $ if n < 0
      then const (`shouldBe` Just InvalidNumberRange)
      else success_with_elem_check
        (`shouldBe` [ if fromIntegral n >= size * 8
                        then BS.pack (replicate size 0)
                        else f bs (fromIntegral n)
                    ]
        )
      where size = BS.length bs
    _ -> pure ()

ftestBS
  :: (a -> BS.ByteString)
  -> [a]
  -> [ScriptOp]
  -> [a]
  -> SpecWith (Arg Expectation)
ftestBS f push_elems ops expected_elems =
  it ("returns " ++ show (hex <$> expected) ++ " given " ++ show_ops)
    $ test_script_with default_ctx (stack_equal $ Seq.fromList packed) ops
    $ success_with_elem_check
    $ \elems -> hex <$> elems `shouldBe` hex <$> expected
 where
  packed   = f <$> push_elems
  expected = f <$> expected_elems
  showpush xs = "OP_PUSHDATA " ++ show (hex xs)
  show_ops =
    "[" ++ intercalate "," (map showpush packed ++ map show ops) ++ "]"

testBS :: [Integer] -> [ScriptOp] -> [Integer] -> SpecWith (Arg Expectation)
testBS = ftestBS rawNumToBS

testPack :: [[Word8]] -> [ScriptOp] -> [[Word8]] -> SpecWith (Arg Expectation)
testPack = ftestBS BS.pack

terminatesWith :: InterpreterError -> [ScriptOp] -> SpecWith (Arg Expectation)
terminatesWith error ops =
  it ("returns " ++ show error ++ " given " ++ show ops)
    $ test_script ops
    $ const (`shouldBe` Just error)

testNoFlags
  :: Maybe InterpreterError -> [ScriptOp] -> SpecWith (Arg Expectation)
testNoFlags r ops =
  it ("returns [] given " ++ show ops ++ " and no flags")
    $ forAll arbitrary
    $ \ctx ->
        snd
            (interpretWith (ctx { script_flags = empty })
                           (script_equal (Script ops) empty_env)
            )
          `shouldBe` r

rawNumToBS = BS.reverse . BS.pack . unroll
hex = toLazyByteString . byteStringHex

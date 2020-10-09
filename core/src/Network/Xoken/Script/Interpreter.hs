{-# LANGUAGE LambdaCase #-}
module Network.Xoken.Script.Interpreter where

import           Data.Word                      ( Word8
                                                , Word32
                                                , Word64
                                                )
import           Data.Maybe                     ( maybe )
import           Data.Bits                      ( complement
                                                , (.&.)
                                                , (.|.)
                                                , xor
                                                , shiftL
                                                , shiftR
                                                )
import           Data.Bits.ByteString
import           Data.EnumBitSet                ( get
                                                , set
                                                , empty
                                                , fromEnums
                                                )
import           Control.Monad                  ( sequence_
                                                , when
                                                )
import           Crypto.Hash                    ( hashWith
                                                , RIPEMD160(..)
                                                , SHA1(..)
                                                , SHA256(..)
                                                )
import           Crypto.Secp256k1               ( importSig
                                                , importPubKey
                                                )
import qualified Data.ByteString               as BS
import qualified Data.ByteArray                as BA
import qualified Data.Sequence                 as Seq
import           Network.Xoken.Script.Common
import           Network.Xoken.Script.Interpreter.Commands
import           Network.Xoken.Script.Interpreter.Util

maxScriptElementSizeBeforeGenesis = 520
sequenceLocktimeDisableFlag = 2 ^ 31

maxScriptNumLength :: Integral a => Bool -> Bool -> a
maxScriptNumLength genesis consensus | not genesis = 4
                                     | consensus   = 750 * 1000
                                     | otherwise   = 250 * 1000

maxPubKeysPerMultiSig :: Integral a => Bool -> Bool -> a
maxPubKeysPerMultiSig genesis consensus
  | not genesis = 20
  | otherwise   = fromIntegral (maxBound :: Word32)

mandatoryScriptFlags :: ScriptFlags
mandatoryScriptFlags = fromEnums
  [ VERIFY_P2SH
  , VERIFY_STRICTENC
  , ENABLE_SIGHASH_FORKID
  , VERIFY_LOW_S
  , VERIFY_NULLFAIL
  ]

standardScriptFlags :: ScriptFlags
standardScriptFlags = fromEnums
  [ VERIFY_DERSIG
  , VERIFY_MINIMALDATA
  , VERIFY_NULLDUMMY
  , VERIFY_DISCOURAGE_UPGRADABLE_NOPS
  , VERIFY_CLEANSTACK
  , VERIFY_CHECKLOCKTIMEVERIFY
  , VERIFY_CHECKSEQUENCEVERIFY
  ]

interpretWith :: Env -> (Env, Maybe InterpreterError)
interpretWith env = go (script_end_to_hash env) env where
  go (op : rest) e
    | failed_branches e == 0 && not (non_top_level_return e) = next
      (opcode op)
      (if op == OP_CODESEPARATOR then e { script_end_to_hash = rest } else e)
    | otherwise = case op of
      OP_IF       -> next (pushbranch failed_branch) e
      OP_NOTIF    -> next (pushbranch failed_branch) e
      OP_VERIF    -> next (opcode op) e
      OP_VERNOTIF -> next (opcode op) e
      OP_ELSE     -> next (opcode op) e
      OP_ENDIF    -> next (opcode op) e
      _           -> go rest e
   where
    next cmd e = case interpretCmd cmd e of
      (e', OK         ) -> go rest e'
      (e', Error error) -> (e', Just error)
      (e', Return     ) -> (e', Nothing)
    failed_branch = Branch { satisfied = False, is_else_branch = False }
  go [] e = (e, Nothing)

empty_env :: Script -> BaseSignatureChecker -> Env
empty_env script checker = Env { stack                  = Seq.empty
                               , alt_stack              = Seq.empty
                               , branch_stack           = Seq.empty
                               , failed_branches        = 0
                               , non_top_level_return   = False
                               , script_flags           = empty
                               , base_signature_checker = checker
                               , script_end_to_hash     = scriptOps script
                               , is_consensus           = True
                               , op_count               = 0
                               }

opcode :: ScriptOp -> Cmd ()
-- Pushing Data
opcode OP_0                  = push $ BS.empty
opcode (OP_PUSHDATA bs size) = pushdata bs size
opcode OP_1NEGATE            = pushint (-1)
opcode OP_1                  = pushint 1
opcode OP_2                  = pushint 2
opcode OP_3                  = pushint 3
opcode OP_4                  = pushint 4
opcode OP_5                  = pushint 5
opcode OP_6                  = pushint 6
opcode OP_7                  = pushint 7
opcode OP_8                  = pushint 8
opcode OP_9                  = pushint 9
opcode OP_10                 = pushint 10
opcode OP_11                 = pushint 11
opcode OP_12                 = pushint 12
opcode OP_13                 = pushint 13
opcode OP_14                 = pushint 14
opcode OP_15                 = pushint 15
opcode OP_16                 = pushint 16
-- Flow control
opcode OP_NOP                = pure ()
opcode OP_VER                = terminate (Unimplemented OP_VER)
opcode OP_IF                 = ifcmd ((/= 0) . num)
opcode OP_NOTIF              = ifcmd ((== 0) . num)
opcode OP_VERIF              = terminate (Unimplemented OP_VERIF)
opcode OP_VERNOTIF           = terminate (Unimplemented OP_VERNOTIF)
opcode OP_ELSE               = popbranch >>= \b -> if is_else_branch b
  then terminate UnbalancedConditional
  else pushbranch
    (Branch { satisfied = not $ satisfied b, is_else_branch = True })
opcode OP_ENDIF  = popbranch >> pure ()
opcode OP_VERIFY = pop >>= \x -> when (num x == 0) (terminate Verify)
opcode OP_RETURN = flag UTXO_AFTER_GENESIS >>= \case
  True -> stacksize >>= \s -> when (s == 0) success >> nontoplevelreturn
  _    -> terminate OpReturn
-- Stack operations
opcode OP_TOALTSTACK   = pop >>= pushalt
opcode OP_FROMALTSTACK = popalt >>= push
opcode OP_2DROP        = pop >> pop >> pure ()
opcode OP_2DUP         = arrangepeek 2 (\[x1, x2] -> [x1, x2])
opcode OP_3DUP         = arrangepeek 3 (\[x1, x2, x3] -> [x1, x2, x3])
opcode OP_2OVER        = arrangepeek 4 (\[x1, x2, x3, x4] -> [x1, x2])
opcode OP_2ROT =
  arrange 6 (\[x1, x2, x3, x4, x5, x6] -> [x3, x4, x5, x6, x1, x2])
opcode OP_2SWAP = arrange 4 (\[x1, x2, x3, x4] -> [x3, x4, x1, x2])
opcode OP_IFDUP = peek >>= \x1 -> when (num x1 /= 0) (push x1)
opcode OP_DEPTH = stacksize >>= push . bin . BN . fromIntegral
opcode OP_DROP  = pop >> pure ()
opcode OP_DUP   = peek >>= push
opcode OP_NIP   = arrange 2 (\[x1, x2] -> [x2])
opcode OP_OVER  = arrangepeek 2 (\[x1, x2] -> [x1])
opcode OP_PICK  = pop >>= bn2u32 . num >>= peeknth >>= push
opcode OP_ROLL  = pop >>= bn2u32 . num >>= popnth >>= push
opcode OP_ROT   = arrange 3 (\[x1, x2, x3] -> [x2, x3, x1])
opcode OP_SWAP  = arrange 2 (\[x1, x2] -> [x2, x1])
opcode OP_TUCK  = arrange 2 (\[x1, x2] -> [x2, x1, x2])
-- Data manipulation
opcode OP_CAT   = popn 2 >>= \[x1, x2] ->
  isOverMaxElemSize (BS.length x1 + BS.length x2) >>= \case
    True -> terminate PushSize
    _    -> push (BS.append x1 x2)
opcode OP_SPLIT = popn 2 >>= \[x1, x2] -> do
  let n = num x2
  if n < 0 || n > fromIntegral (BS.length x1)
    then terminate InvalidSplitRange
    else let (y1, y2) = BS.splitAt (fromIntegral n) x1 in pushn [y1, y2]
opcode OP_NUM2BIN = popn 2 >>= arith >>= \[x1, x2] ->
  isOverMaxElemSize x2 >>= \tooBig ->
    if x2 < 0 || x2 > fromIntegral (maxBound :: Int) || tooBig
      then terminate PushSize
      else maybe (terminate ImpossibleEncoding)
                 push
                 (num2binpad x1 (fromIntegral x2))
opcode OP_BIN2NUM = pop >>= push . bin . num
opcode OP_SIZE    = peek >>= pushint . BS.length
-- Bitwise logic
opcode OP_INVERT  = unary (BS.map complement)
opcode OP_AND     = binarybitwise (.&.)
opcode OP_OR      = binarybitwise (.|.)
opcode OP_XOR     = binarybitwise xor
opcode OP_EQUAL   = popn 2 >>= push . bin . \[x1, x2] -> truth $ x1 == x2
opcode OP_EQUALVERIFY =
  popn 2 >>= \[x1, x2] -> when (x1 /= x2) (terminate EqualVerify)
-- Arithmetic
opcode OP_1ADD      = unaryarith succ
opcode OP_1SUB      = unaryarith pred
opcode OP_2MUL      = unaryarith (flip shiftL 1)
opcode OP_2DIV      = unaryarith (flip shiftR 1)
opcode OP_NEGATE    = unaryarith negate
opcode OP_ABS       = unaryarith abs
opcode OP_NOT       = unaryarith (truth . (== 0))
opcode OP_0NOTEQUAL = unaryarith (truth . (/= 0))
opcode OP_ADD       = binaryarith (+)
opcode OP_SUB       = binaryarith (-)
opcode OP_MUL       = binaryarith (*)
opcode OP_DIV       = popn 2 >>= arith >>= \[x1, x2] ->
  if x2 == 0 then terminate DivByZero else push $ bin (x1 `div` x2)
opcode OP_MOD = popn 2 >>= arith >>= \[x1, x2] ->
  if x2 == 0 then terminate ModByZero else push $ bin (x1 `mod` x2)
opcode OP_LSHIFT   = shift shiftL
opcode OP_RSHIFT   = shift shiftR
opcode OP_BOOLAND  = binaryarith (\a b -> truth (a /= 0 && b /= 0))
opcode OP_BOOLOR   = binaryarith (\a b -> truth (a /= 0 || b /= 0))
opcode OP_NUMEQUAL = binaryarith (btruth (==))
opcode OP_NUMEQUALVERIFY =
  popn 2 >>= arith >>= \[x1, x2] -> when (x1 /= x2) (terminate NumEqualVerify)
opcode OP_NUMNOTEQUAL        = binaryarith (btruth (/=))
opcode OP_LESSTHAN           = binaryarith (btruth (<))
opcode OP_GREATERTHAN        = binaryarith (btruth (>))
opcode OP_LESSTHANOREQUAL    = binaryarith (btruth (<=))
opcode OP_GREATERTHANOREQUAL = binaryarith (btruth (>=))
opcode OP_MIN                = binaryarith min
opcode OP_MAX                = binaryarith max
opcode OP_WITHIN = popn 3 >>= arith >>= push . bin . \[x, min, max] ->
  truth $ min <= x && x < max
-- Crypto
opcode OP_RIPEMD160 = unary (BA.convert . hashWith RIPEMD160)
opcode OP_SHA1      = unary (BA.convert . hashWith SHA1)
opcode OP_SHA256    = unary (BA.convert . hashWith SHA256)
opcode OP_HASH160   = unary (BA.convert . hashWith RIPEMD160 . hashWith SHA256)
opcode OP_HASH256   = unary (BA.convert . hashWith SHA256 . hashWith SHA256)
opcode OP_CODESEPARATOR =
  terminate (HigherLevelImplementation OP_CODESEPARATOR)
opcode OP_CHECKSIG            = checksig pushbool
opcode OP_CHECKSIGVERIFY      = checksig (verify CheckSigVerify)
opcode OP_CHECKMULTISIG       = checkmultisig pushbool
opcode OP_CHECKMULTISIGVERIFY = checkmultisig (verify CheckMultiSigVerify)
-- Pseudo-words
opcode OP_PUBKEYHASH          = terminate (Unimplemented OP_PUBKEYHASH)
opcode OP_PUBKEY              = terminate (Unimplemented OP_PUBKEY)
opcode (OP_INVALIDOPCODE n)   = terminate (BadOpcode (OP_INVALIDOPCODE n))
-- Reserved words
opcode OP_RESERVED            = terminate (BadOpcode OP_RESERVED)
opcode OP_RESERVED1           = terminate (BadOpcode OP_RESERVED1)
opcode OP_RESERVED2           = terminate (BadOpcode OP_RESERVED2)
opcode OP_NOP1                = nop
opcode OP_NOP2                = maybenop
  VERIFY_CHECKLOCKTIMEVERIFY
  (pop >>= limitednum 5 >>= \n -> if n < 0
    then terminate NegativeLocktime
    else checker
      >>= \c -> when (not $ checkLockTime c n) (terminate UnsatisfiedLockTime)
  )
opcode OP_NOP3 = maybenop
  VERIFY_CHECKSEQUENCEVERIFY
  (pop >>= limitednum 5 >>= \n -> if n < 0
    then terminate NegativeLocktime
    else checker >>= \c -> when
      (n .&. sequenceLocktimeDisableFlag == 0 && not (checkSequence c n))
      (terminate UnsatisfiedLockTime)
  )
opcode OP_NOP4  = nop
opcode OP_NOP5  = nop
opcode OP_NOP6  = nop
opcode OP_NOP7  = nop
opcode OP_NOP8  = nop
opcode OP_NOP9  = nop
opcode OP_NOP10 = nop

pushint :: Int -> Cmd ()
pushint = push . bin . BN . fromIntegral

pushn :: [Elem] -> Cmd ()
pushn = sequence_ . map push

pushdata :: BS.ByteString -> PushDataType -> Cmd ()
pushdata bs size = flag VERIFY_MINIMALDATA >>= \case
  True -> maybe
    (terminate MinimalData)
    (\optimal -> if size == optimal then go else terminate MinimalData)
    (pushDataType (BS.length bs))
  _ -> go
 where
  go = isOverMaxElemSize (BS.length bs) >>= \case
    True -> terminate PushSize
    _    -> push bs

isOverMaxElemSize :: Integral a => a -> Cmd Bool
isOverMaxElemSize n = flag UTXO_AFTER_GENESIS >>= pure . isOver
  where isOver = (&& n > fromIntegral maxScriptElementSizeBeforeGenesis)

arrange :: Int -> ([Elem] -> [Elem]) -> Cmd ()
arrange n f = popn n >>= pushn . f

arrangepeek :: Int -> ([Elem] -> [Elem]) -> Cmd ()
arrangepeek n f = peekn n >>= pushn . f

unary :: (Elem -> Elem) -> Cmd ()
unary f = pop >>= push . f

arith :: [Elem] -> Cmd [BN]
arith = pure . map num

unaryarith :: (BN -> BN) -> Cmd ()
unaryarith f = pop >>= push . bin . f . num

binaryarith :: (BN -> BN -> BN) -> Cmd ()
binaryarith f = popn 2 >>= arith >>= \[x1, x2] -> push $ bin $ f x1 x2

binarybitwise :: (Word8 -> Word8 -> Word8) -> Cmd ()
binarybitwise f = popn 2 >>= \[x1, x2] -> if BS.length x1 == BS.length x2
  then push (BS.pack $ BS.zipWith f x1 x2)
  else terminate InvalidOperandSize

btruth :: (a1 -> a2 -> Bool) -> a1 -> a2 -> BN
btruth = ((truth .) .)

num :: Elem -> BN
num = bin2num

bin :: BN -> Elem
bin = num2bin

shift :: (Elem -> Int -> Elem) -> Cmd ()
shift f = popn 2 >>= \[x1, x2] -> do
  let n = num x2
  if n < 0
    then terminate InvalidNumberRange
    else if n >= fromIntegral (BS.length x1) * 8
      then push (BS.replicate (BS.length x1) 0)
      else go x1 n
 where
  go x n | n <= max  = bn2u32 n >>= push . f x . fromIntegral
         | otherwise = go (f x maxInt) (n - max)
  maxInt = maxBound :: Int
  max    = fromIntegral maxInt

ifcmd :: (Elem -> Bool) -> Cmd ()
ifcmd is_satisfied = stacksize >>= \case
  0 -> terminate UnbalancedConditional
  _ -> do
    x  <- pop
    fs <- flags
    let n = BS.length x
    when
      (get VERIFY_MINIMALIF fs && (n > 1 || (n == 1 && x /= BS.singleton 1)))
      (terminate MinimalIf)
    pushbranch (Branch { satisfied = is_satisfied x, is_else_branch = False })

nop :: Cmd ()
nop = flags >>= \fs -> when (get VERIFY_DISCOURAGE_UPGRADABLE_NOPS fs)
                            (terminate DiscourageUpgradableNOPs)

maybenop :: ScriptFlag -> Cmd () -> Cmd ()
maybenop flag cmd = flags >>= \fs ->
  if (  (not (get flag fs) || get UTXO_AFTER_GENESIS fs)
     && get VERIFY_DISCOURAGE_UPGRADABLE_NOPS fs
     )
    then terminate DiscourageUpgradableNOPs
    else cmd

checksig :: (Bool -> Cmd ()) -> Cmd ()
checksig finalize = popn 2 >>= \[sigBS, pubKeyBS] -> do
  fs     <- flags
  script <- scriptendtohash
  c      <- checker
  let forkid  = get ENABLE_SIGHASH_FORKID fs
      sighash = sigHash sigBS
  case (importSig (BS.init sigBS), importPubKey pubKeyBS) of
    (Just sig, Just pubKey) -> do
      let clean   = cleanupScriptCode script sigBS sighash forkid
          success = checkSig c sig sighash pubKey (Script clean) forkid
      when (not success && get VERIFY_NULLFAIL fs && BS.length sigBS > 0)
           (terminate SigNullfail)
      finalize success
    _ -> terminate InvalidSigOrPubKey

checkmultisig :: (Bool -> Cmd ()) -> Cmd ()
checkmultisig finalize = do
  c       <- consensus
  fs      <- flags
  script  <- scriptendtohash
  opCount <- opcount
  nKeysBN <- pop
  let nKeysCountSigned = fromIntegral (num nKeysBN) :: Int
      nKeysCount       = fromIntegral nKeysCountSigned :: Word64
      genesis          = get UTXO_AFTER_GENESIS fs
      opCount'         = opCount + nKeysCount
  when (nKeysCountSigned < 0 || nKeysCount > maxPubKeysPerMultiSig genesis c)
       (terminate PubKeyCount)
  when (opCount' > 0) (terminate InvalidOpCount)
  keys    <- popn nKeysCountSigned
  nSigsBN <- pop
  let nSigsCountSigned = fromIntegral (num nSigsBN) :: Int
      nSigsCount       = fromIntegral nSigsCountSigned :: Word64
  when (nSigsCountSigned < 0 || nSigsCount > nKeysCount) (terminate SigCount)
  sigs <- popn nSigsCountSigned
  x    <- pop -- bug extra value
  let forkid = get ENABLE_SIGHASH_FORKID fs
      clean sigBS ops = cleanupScriptCode ops sigBS (sigHash sigBS) forkid
      script' = foldr clean script sigs
  pure ()

verify :: InterpreterError -> Bool -> Cmd ()
verify error x = when (not x) (terminate error)

pushbool :: Bool -> Cmd ()
pushbool = push . bin . truth

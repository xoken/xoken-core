module Network.Xoken.Script.Interpreter where

import           Data.Maybe                     ( maybe )
import           Data.Bits                      ( complement
                                                , (.&.)
                                                , (.|.)
                                                , xor
                                                , shiftL
                                                , shiftR
                                                )
import           Control.Monad                  ( sequence_
                                                , when
                                                )
import qualified Data.ByteString               as BS
import qualified Data.Serialize                as S
import qualified Data.Sequence                 as Seq
import           Network.Xoken.Script.Common
import           Network.Xoken.Script.Interpreter.Commands
import           Network.Xoken.Script.Interpreter.OpenSSL_BN

interpret :: Script -> (Env, Maybe InterpreterError)
interpret script = go (scriptOps script) empty_env where
  go (op : rest) e = case Seq.viewl (branch_stack e) of
    Branch { satisfied = False } Seq.:< _ -> case op of
      OP_ELSE  -> next
      OP_ENDIF -> next
      _        -> go rest e
    _ -> next
   where
    next = case interpretCmd (opcode op) e of
      (e', Nothing) -> go rest e'
      r             -> r
  go [] e = (e, Nothing)

empty_env = Env { stack          = Seq.empty
                , alt_stack      = Seq.empty
                , branch_stack   = Seq.empty
                , marked_invalid = False
                }

opcode :: ScriptOp -> Cmd ()
-- Pushing Data
opcode (OP_PUSHDATA bs OPCODE ) = push bs
opcode (OP_PUSHDATA bs OPDATA1) = pushdata 1 bs
opcode (OP_PUSHDATA bs OPDATA2) = pushdata 2 bs
opcode (OP_PUSHDATA bs OPDATA4) = pushdata 4 bs
opcode OP_0                     = pushint 0
opcode OP_1NEGATE               = pushint (-1)
opcode OP_1                     = pushint 1
opcode OP_2                     = pushint 2
opcode OP_3                     = pushint 3
opcode OP_4                     = pushint 4
opcode OP_5                     = pushint 5
opcode OP_6                     = pushint 6
opcode OP_7                     = pushint 7
opcode OP_8                     = pushint 8
opcode OP_9                     = pushint 9
opcode OP_10                    = pushint 10
opcode OP_11                    = pushint 11
opcode OP_12                    = pushint 12
opcode OP_13                    = pushint 13
opcode OP_14                    = pushint 14
opcode OP_15                    = pushint 15
opcode OP_16                    = pushint 16
-- Flow control
opcode OP_NOP                   = pure ()
opcode OP_VER                   = terminate (Unimplemented OP_VER)
opcode OP_IF                    = stacksize >>= num >>= \s -> if s == 0
  then terminate UnbalancedConditional
  else pop >>= num >>= \x ->
    pushbranch (Branch { satisfied = x /= 0, is_else_branch = False })
opcode OP_NOTIF = stacksize >>= num >>= \s -> if s == 0
  then terminate UnbalancedConditional
  else pop >>= num >>= \x ->
    pushbranch (Branch { satisfied = x == 0, is_else_branch = False })
opcode OP_VERIF    = terminate (Unimplemented OP_VERIF)
opcode OP_VERNOTIF = terminate (Unimplemented OP_VERNOTIF)
opcode OP_ELSE     = popbranch >>= \b -> if is_else_branch b
  then terminate UnbalancedConditional
  else pushbranch
    (Branch { satisfied = not $ satisfied b, is_else_branch = True })
opcode OP_ENDIF        = popbranch >> pure ()
opcode OP_VERIFY       = pop >>= num >>= \x -> when (x == 0) markinvalid
opcode OP_RETURN       = terminate (Unimplemented OP_RETURN)
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
opcode OP_IFDUP = peek >>= \x1 -> num x1 >>= \n -> when (n /= BN 0) (push x1)
opcode OP_DEPTH = stacksize >>= push
opcode OP_DROP  = pop >> pure ()
opcode OP_DUP   = peek >>= push
opcode OP_NIP   = arrange 2 (\[x1, x2] -> [x2])
opcode OP_OVER  = arrangepeek 2 (\[x1, x2] -> [x1])
opcode OP_PICK  = pop >>= num >>= bn2u32 >>= peeknth >>= push
opcode OP_ROLL  = pop >>= num >>= bn2u32 >>= popnth >>= push
opcode OP_ROT   = arrange 3 (\[x1, x2, x3] -> [x2, x3, x1])
opcode OP_SWAP  = arrange 2 (\[x1, x2] -> [x2, x1])
opcode OP_TUCK  = arrange 2 (\[x1, x2] -> [x2, x1, x2])
-- Data manipulation
opcode OP_CAT   = binary BS.append
opcode OP_SPLIT = terminate (Unimplemented OP_SPLIT)
opcode OP_NUM2BIN =
  popn 2
    >>= arith
    >>= (\[x1, x2] -> maybe (terminate ConversionError) push (num2bin x1 x2))
opcode OP_BIN2NUM =
  pop >>= maybe (terminate ConversionError) ((>>= push) . bin) . bin2num
opcode OP_SIZE      = peek >>= pushint . BS.length
-- Bitwise logic
opcode OP_INVERT    = unary (BS.map complement)
opcode OP_AND       = binary ((BS.pack .) . BS.zipWith (.&.))
opcode OP_OR        = binary ((BS.pack .) . BS.zipWith (.|.))
opcode OP_XOR       = binary ((BS.pack .) . BS.zipWith xor)
opcode OP_EQUAL     = popn 2 >>= (\[x1, x2] -> bin $ truth $ x1 == x2) >>= push
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
opcode OP_DIV       = binaryarith div
opcode OP_MOD       = binaryarith mod
opcode OP_LSHIFT    = popn 2 >>= arith >>= \[x1, x2] ->
  if x2 <= BN (toInteger (maxBound :: Int))
    then push =<< bin (shiftL x1 (fromIntegral x2))
    else terminate $ TooMuchToLShift x2
opcode OP_RSHIFT =
  popn 2
    >>= arith
    >>= (\[x1, x2] -> bin $ if x2 <= BN (toInteger (maxBound :: Int))
          then shiftR x1 (fromIntegral x2)
          else 0
        )
    >>= push
opcode OP_BOOLAND            = binaryarith (\a b -> truth (a /= 0 && b /= 0))
opcode OP_BOOLOR             = binaryarith (\a b -> truth (a /= 0 || b /= 0))
opcode OP_NUMEQUAL           = binaryarith (btruth (==))
opcode OP_NUMNOTEQUAL        = binaryarith (btruth (/=))
opcode OP_LESSTHAN           = binaryarith (btruth (<))
opcode OP_GREATERTHAN        = binaryarith (btruth (>))
opcode OP_LESSTHANOREQUAL    = binaryarith (btruth (<=))
opcode OP_GREATERTHANOREQUAL = binaryarith (btruth (>=))
opcode OP_MIN                = binaryarith min
opcode OP_MAX                = binaryarith max
opcode OP_WITHIN =
  popn 3
    >>= arith
    >>= (\[x, min, max] -> bin $ truth $ min <= x && x < max)
    >>= push
opcode scriptOp = terminate (Unimplemented scriptOp)

pushint :: Int -> Cmd ()
pushint = push . int2bin

pushn :: [Elem] -> Cmd ()
pushn = sequence_ . map push

pushdata :: Int -> BS.ByteString -> Cmd ()
pushdata n bs = case S.decode bs1 of
  Right bytes -> if fromIntegral bytes <= BS.length bs2
    then push bs2
    else terminate $ NotEnoughBytes { expected = bytes, actual = BS.length bs2 }
  _ -> terminate $ NoDecoding { length_bytes = n, bytestring = bs1 }
  where (bs1, bs2) = BS.splitAt n bs

arrange :: Int -> ([Elem] -> [Elem]) -> Cmd ()
arrange n f = popn n >>= pushn . f

arrangepeek :: Int -> ([Elem] -> [Elem]) -> Cmd ()
arrangepeek n f = peekn n >>= pushn . f

unary :: (Elem -> Elem) -> Cmd ()
unary f = pop >>= push . f

binary :: (Elem -> Elem -> Elem) -> Cmd ()
binary f = popn 2 >>= \[x1, x2] -> push $ f x1 x2

arith :: [Elem] -> Cmd [BN]
arith = mapM num

unaryarith :: (BN -> BN) -> Cmd ()
unaryarith f = pop >>= num >>= bin . f >>= push

binaryarith :: (BN -> BN -> BN) -> Cmd ()
binaryarith f = popn 2 >>= arith >>= \[x1, x2] -> bin (f x1 x2) >>= push

truth :: Bool -> BN
truth x = BN $ if x then 1 else 0

btruth :: (a1 -> a2 -> Bool) -> a1 -> a2 -> BN
btruth = ((truth .) .)

{-# LANGUAGE DeriveFunctor #-}

module Network.Xoken.Script.Interpreter where

import           Data.Word                      ( Word8 )
import           Data.Bits                      ( complement
                                                , (.&.)
                                                , (.|.)
                                                , xor
                                                , shiftL
                                                , shiftR
                                                )
import qualified Data.ByteString               as BS
import qualified Data.Serialize                as S
import qualified Data.Sequence                 as Seq
import           Control.Monad                  ( sequence_
                                                , replicateM
                                                , when
                                                )
import           Control.Monad.Free             ( Free(Pure, Free)
                                                , liftF
                                                )
import           Network.Xoken.Script.Common

type Elem = Integer
type Stack = Seq.Seq Elem
type InterpreterResult = (Stack, Maybe InterpreterError)

data InterpreterError
  = StackUnderflow
  | NoDecoding {length_bytes :: Int, bytestring :: BS.ByteString}
  | NotEnoughBytes {expected :: Word8, actual :: Int}
  | TooMuchToLShift Integer
  | Unimplemented ScriptOp
  | Message String
  deriving (Show, Eq)

data InterpreterCommands a
    = Terminate InterpreterError a
    | Push Elem a
    | Pop (Elem -> a)
    | Peek (Elem -> a)
    deriving (Functor)

type Cmd = Free InterpreterCommands

interpret :: Script -> InterpreterResult
interpret script = interpretCmd (mapM_ opcode $ scriptOps script) Seq.empty

interpretCmd :: Cmd () -> Stack -> InterpreterResult
interpretCmd (Pure ()        ) stack = (stack, Nothing)
interpretCmd (Free (Push x m)) stack = interpretCmd m (x Seq.<| stack)
interpretCmd (Free (Pop k   )) stack = case Seq.viewl stack of
  x Seq.:< rest -> interpretCmd (k x) rest
  _             -> (stack, Just StackUnderflow)
interpretCmd (Free (Peek k)) stack = case Seq.viewl stack of
  x Seq.:< _ -> interpretCmd (k x) stack
  _          -> (stack, Just StackUnderflow)
interpretCmd (Free (Terminate e _)) stack = (stack, Just e)

terminate :: InterpreterError -> Cmd ()
terminate e = liftF (Terminate e ())

push :: Elem -> Cmd ()
push x = liftF (Push x ())

pop :: Cmd Elem
pop = liftF (Pop id)

peek :: Cmd Elem
peek = liftF (Peek id)

pushn :: [Elem] -> Cmd ()
pushn = sequence_ . map push

popn :: Int -> Cmd [Elem]
popn n = replicateM n pop >>= pure . reverse

peekn :: Int -> Cmd [Elem]
peekn n = do
  xs <- popn $ n - 1
  x  <- peek
  pure $ x : xs

popnpush :: Cmd [Elem] -> ([Elem] -> [Elem]) -> Cmd ()
popnpush popn f = popn >>= pushn . f

arrange :: Int -> ([Elem] -> [Elem]) -> Cmd ()
arrange = popnpush . popn

arrangepeek :: Int -> ([Elem] -> [Elem]) -> Cmd ()
arrangepeek = popnpush . peekn

unary :: (Elem -> Elem) -> Cmd ()
unary f = pop >>= push . f

binary :: (Elem -> Elem -> Elem) -> Cmd ()
binary f = do
  x2 <- pop
  x1 <- pop
  push $ f x1 x2

truth :: Bool -> Elem
truth True = 1
truth _    = 0

btruth :: (a1 -> a2 -> Bool) -> a1 -> a2 -> Elem
btruth = ((truth .) .)

pushdata n bs = case (S.decode bs1, S.decode bs2) of
  (Right bytes, Right x) -> if fromIntegral bytes <= BS.length bs2
    then push x
    else terminate $ NotEnoughBytes { expected = bytes, actual = BS.length bs2 }
  (Right bytes, Left msg) ->
    terminate $ NotEnoughBytes { expected = bytes, actual = BS.length bs2 }
  _ -> terminate $ NoDecoding { length_bytes = n, bytestring = bs }
  where (bs1, bs2) = BS.splitAt n bs

opcode :: ScriptOp -> Cmd ()
-- Pushing Data
opcode (OP_PUSHDATA bs OPCODE) = case S.decode bs of
  Right x -> push x
  _       -> push 0
opcode (OP_PUSHDATA bs OPDATA1) = pushdata 1 bs
opcode (OP_PUSHDATA bs OPDATA2) = pushdata 2 bs
opcode (OP_PUSHDATA bs OPDATA4) = pushdata 4 bs
opcode OP_0                     = push 0
opcode OP_1NEGATE               = push (-1)
opcode OP_1                     = push 1
opcode OP_2                     = push 2
opcode OP_3                     = push 3
opcode OP_4                     = push 4
opcode OP_5                     = push 5
opcode OP_6                     = push 6
opcode OP_7                     = push 7
opcode OP_8                     = push 8
opcode OP_9                     = push 9
opcode OP_10                    = push 10
opcode OP_11                    = push 11
opcode OP_12                    = push 12
opcode OP_13                    = push 13
opcode OP_14                    = push 14
opcode OP_15                    = push 15
opcode OP_16                    = push 16
-- Stack operations
opcode OP_DROP                  = pop >> pure ()
opcode OP_DUP                   = peek >>= push
opcode OP_NIP                   = arrange 2 (\[x1, x2] -> [x2])
opcode OP_OVER                  = arrangepeek 2 (\[x1, x2] -> [x2, x1])
opcode OP_ROT                   = arrange 3 (\[x1, x2, x3] -> [x2, x3, x1])
opcode OP_SWAP                  = arrange 2 (\[x1, x2] -> [x2, x1])
opcode OP_TUCK                  = arrange 2 (\[x1, x2] -> [x2, x1, x2])
opcode OP_2DROP                 = pop >> pop >> pure ()
opcode OP_2DUP                  = arrangepeek 2 (\[x1, x2] -> [x2, x1, x2])
opcode OP_3DUP = arrangepeek 3 (\[x1, x2, x3] -> [x2, x3, x1, x2, x3])
opcode OP_2OVER = arrangepeek 4 (\[x1, x2, x3, x4] -> [x2, x3, x4, x1, x2])
opcode OP_2ROT =
  arrange 6 (\[x1, x2, x3, x4, x5, x6] -> [x3, x4, x5, x6, x1, x2])
opcode OP_2SWAP     = arrange 4 (\[x1, x2, x3, x4] -> [x3, x4, x1, x2])
-- Bitwise logic
opcode OP_INVERT    = unary complement
opcode OP_AND       = binary (.&.)
opcode OP_OR        = binary (.|.)
opcode OP_XOR       = binary xor
opcode OP_EQUAL     = binary undefined
-- Arithmetic
opcode OP_1ADD      = unary succ
opcode OP_1SUB      = unary pred
opcode OP_2MUL      = unary (flip shiftL 1)
opcode OP_2DIV      = unary (flip shiftR 1)
opcode OP_NEGATE    = unary negate
opcode OP_ABS       = unary abs
opcode OP_NOT       = unary (truth . (== 0))
opcode OP_0NOTEQUAL = unary (truth . (/= 0))
opcode OP_ADD       = binary (+)
opcode OP_SUB       = binary (-)
opcode OP_MUL       = binary (*)
opcode OP_DIV       = binary div
opcode OP_MOD       = binary mod
opcode OP_LSHIFT    = do
  b <- pop
  a <- pop
  if b <= toInteger (maxBound :: Int)
    then push $ shiftL a (fromIntegral b)
    else terminate $ TooMuchToLShift b
opcode OP_RSHIFT = binary
  (\a b ->
    if b <= toInteger (maxBound :: Int) then shiftR a (fromIntegral b) else 0
  )
opcode OP_BOOLAND            = binary (\a b -> truth (a /= 0 && b /= 0))
opcode OP_BOOLOR             = binary (\a b -> truth (a /= 0 || b /= 0))
opcode OP_NUMEQUAL           = binary (btruth (==))
opcode OP_NUMNOTEQUAL        = binary (btruth (/=))
opcode OP_LESSTHAN           = binary (btruth (<))
opcode OP_GREATERTHAN        = binary (btruth (>))
opcode OP_LESSTHANOREQUAL    = binary (btruth (<=))
opcode OP_GREATERTHANOREQUAL = binary (btruth (>=))
opcode OP_MIN                = binary min
opcode OP_MAX                = binary max
opcode OP_WITHIN = arrange 3 (\[x, min, max] -> [truth (min <= x && x < max)])

opcode scriptOp              = terminate (Unimplemented scriptOp)

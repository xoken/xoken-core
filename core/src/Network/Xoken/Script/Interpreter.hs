{-# LANGUAGE DeriveFunctor #-}

module Network.Xoken.Script.Interpreter where

import           Data.Bool                      ( bool )
import           Control.Monad                  ( sequence_
                                                , replicateM
                                                )
import           Control.Monad.Free
import           Network.Xoken.Script.Common

type Elem = Int

data StackCommands a
    = Push Elem a
    | Peek (Elem -> a)
    | Pop (Elem -> a)
    deriving (Functor)

type StackCmd = Free StackCommands

push :: Elem -> StackCmd ()
push x = liftF (Push x ())

peek = liftF (Peek id)
pop = liftF (Pop id)

unary f = pop >>= push . f

binary f = do
  x2 <- pop
  x1 <- pop
  push $ f x1 x2

popn = flip replicateM pop
pushn = sequence_ . map push
peekn n = do
  xs <- popn $ n - 1
  x  <- peek
  pure $ x : xs

popnpush popn f = popn >>= pushn . f
arrange = popnpush . popn
arrangepeek = popnpush . peekn

true True = 1
true _    = 0

btrue = ((true .) .)

opcode :: ScriptOp -> StackCmd ()

opcode OP_1NEGATE = push (-1)
opcode OP_1       = push 1
opcode OP_2       = push 2
opcode OP_3       = push 3
opcode OP_4       = push 4
opcode OP_5       = push 5
opcode OP_6       = push 6
opcode OP_7       = push 7
opcode OP_8       = push 8
opcode OP_9       = push 9
opcode OP_10      = push 10
opcode OP_11      = push 11
opcode OP_12      = push 12
opcode OP_13      = push 13
opcode OP_14      = push 14
opcode OP_15      = push 15
opcode OP_16      = push 16

opcode OP_DROP    = pop >> pure ()
opcode OP_DUP     = peek >>= push
opcode OP_NIP     = arrange 2 (\[x1, x2] -> [x2])
opcode OP_OVER    = arrangepeek 2 (\[x1, x2] -> [x2, x1])
opcode OP_ROT     = arrange 3 (\[x1, x2, x3] -> [x2, x3, x1])
opcode OP_SWAP    = arrange 2 (\[x1, x2] -> [x2, x1])
opcode OP_TUCK    = arrange 2 (\[x1, x2] -> [x2, x1, x2])
opcode OP_2DROP   = pop >> pop >> pure ()
opcode OP_2DUP    = arrangepeek 2 (\[x1, x2] -> [x2, x1, x2])
opcode OP_3DUP    = arrangepeek 3 (\[x1, x2, x3] -> [x2, x3, x1, x2, x3])
opcode OP_2OVER   = arrangepeek 4 (\[x1, x2, x3, x4] -> [x2, x3, x4, x1, x2])
opcode OP_2ROT =
  arrange 6 (\[x1, x2, x3, x4, x5, x6] -> [x3, x4, x5, x6, x1, x2])
opcode OP_2SWAP              = arrange 4 (\[x1, x2, x3, x4] -> [x3, x4, x1, x2])

opcode OP_1ADD               = unary succ
opcode OP_1SUB               = unary pred
opcode OP_2MUL               = unary (* 2)
opcode OP_2DIV               = unary (`div` 2)
opcode OP_NEGATE             = unary negate
opcode OP_ABS                = unary abs
opcode OP_NOT                = unary (\x -> bool 1 0 (x == 0))
opcode OP_0NOTEQUAL          = unary (\x -> bool 0 1 (x == 0))
opcode OP_ADD                = binary (+)
opcode OP_SUB                = binary (-)
opcode OP_MUL                = binary (*)
opcode OP_DIV                = binary div
opcode OP_MOD                = binary mod
opcode OP_LSHIFT             = binary undefined
opcode OP_RSHIFT             = binary undefined
opcode OP_BOOLAND            = binary (\a b -> true (a == 0 && b == 0))
opcode OP_BOOLOR             = binary (\a b -> true (a == 0 || b == 0))
opcode OP_NUMEQUAL           = binary (btrue (==))
opcode OP_NUMNOTEQUAL        = binary (btrue (/=))
opcode OP_LESSTHAN           = binary (btrue (<))
opcode OP_GREATERTHAN        = binary (btrue (>))
opcode OP_LESSTHANOREQUAL    = binary (btrue (<=))
opcode OP_GREATERTHANOREQUAL = binary (btrue (>=))
opcode OP_MIN                = binary min
opcode OP_MAX                = binary max
opcode OP_WITHIN = arrange 3 (\[x, min, max] -> [true (min <= x && x < max)])

opcode _                     = pure ()

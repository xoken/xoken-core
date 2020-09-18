{-# LANGUAGE DeriveFunctor #-}
module Network.Xoken.Script.Interpreter.Commands where

import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Data.Foldable                  ( toList )
import           Control.Monad.Free             ( Free(Pure, Free)
                                                , liftF
                                                )
import qualified Data.ByteString               as BS
import qualified Data.Sequence                 as Seq
import           Network.Xoken.Script.Common
import           Network.Xoken.Script.Interpreter.OpenSSL_BN

type Elem = BS.ByteString
type Stack a = Seq.Seq a

data InterpreterCommands a
    -- signal
    = Terminate InterpreterError
    | MarkInvalid
    -- stack
    | Push Elem a
    | Pop (Elem -> a)
    | PopN Int ([Elem] -> a)
    | PopNth Word32 (Elem -> a)
    | Peek (Elem -> a)
    | PeekN Int ([Elem] -> a)
    | PeekNth Word32 (Elem -> a)
    | StackSize (BN -> a)
    -- alt stack
    | PushAlt Elem a
    | PopAlt (Elem -> a)
    -- branch stack
    | PushBranch Branch a
    | PopBranch (Branch -> a)
    -- num
    | Num2u32 BN (Word32 -> a)
    deriving (Functor)

type Cmd = Free InterpreterCommands

data InterpreterError
  = StackUnderflow
  | NoDecoding {length_bytes :: Int, bytestring :: BS.ByteString}
  | NotEnoughBytes {expected :: Word8, actual :: Int}
  | ConversionError
  | Unimplemented ScriptOp
  | Message String
  | UnbalancedConditional
  | InvalidAltstackOperation
  | InvalidNumberRange
  | InvalidOperandSize
  deriving (Show, Eq)

data Env = Env
  { stack :: Stack Elem
  , alt_stack :: Stack Elem
  , branch_stack :: Stack Branch
  , marked_invalid :: Bool
  , failed_branches :: Word32
  } deriving (Show, Eq)

data Branch = Branch
  { satisfied :: Bool
  , is_else_branch :: Bool
  } deriving (Show, Eq)

rindex :: Int -> Env -> Int
rindex i e = length (stack e) - 1 - i

truth :: Integral a => Bool -> a
truth x = if x then 1 else 0

interpretCmd :: Cmd () -> Env -> (Env, Maybe InterpreterError)
interpretCmd = go where
  go (Pure ()) e = (e, Nothing)
  go (Free x ) e = case x of
    -- signal
    Terminate error -> (e, Just error)
    MarkInvalid     -> (e { marked_invalid = True }, Nothing)
    -- stack
    Push x m        -> go m (e { stack = stack e Seq.|> x })
    Pop k           -> case Seq.viewr (stack e) of
      rest Seq.:> x -> go (k x) (e { stack = rest })
      _             -> (e, Just StackUnderflow)
    PopN n k | length topn == n -> go (k $ toList topn) (e { stack = rest })
             | otherwise        -> (e, Just StackUnderflow)
      where (rest, topn) = Seq.splitAt (rindex n e) (stack e)
    PopNth n k -> case stack e Seq.!? i of
      Just x -> go (k x) (e { stack = Seq.deleteAt i (stack e) })
      _      -> (e, Just StackUnderflow)
      where i = rindex (fromIntegral n) e
    Peek k -> case Seq.viewr (stack e) of
      _ Seq.:> x -> go (k x) e
      _          -> (e, Just StackUnderflow)
    PeekN n k | length topn == n -> go (k $ toList topn) e
              | otherwise        -> (e, Just StackUnderflow)
      where topn = Seq.drop (rindex n e) (stack e)
    PeekNth n k -> case stack e Seq.!? rindex (fromIntegral n) e of
      Just x -> go (k x) e
      _      -> (e, Just StackUnderflow)
    StackSize k -> go (k $ BN $ fromIntegral $ length $ stack e) e
    -- alt stack
    PushAlt x m -> go m (e { alt_stack = x Seq.<| alt_stack e })
    PopAlt k    -> case Seq.viewl (alt_stack e) of
      x Seq.:< rest -> go (k x) (e { alt_stack = rest })
      _             -> (e, Just InvalidAltstackOperation)
    -- branch stack
    PushBranch b m ->
      ( e { branch_stack    = b Seq.<| branch_stack e
          , failed_branches = failed_branches e + truth (not $ satisfied b)
          }
      , Nothing
      )
    PopBranch k -> case Seq.viewl (branch_stack e) of
      b Seq.:< rest -> go
        (k b)
        (e { branch_stack    = rest
           , failed_branches = failed_branches e - truth (not $ satisfied b)
           }
        )
      _ -> (e, Just UnbalancedConditional)
    -- num
    Num2u32 n k -> case num2u32 n of
      Just u -> go (k u) e
      _      -> (e, Just InvalidNumberRange)

-- signal
terminate :: InterpreterError -> Cmd ()
terminate e = liftF (Terminate e)

markinvalid :: Cmd ()
markinvalid = liftF MarkInvalid

-- stack
push :: Elem -> Cmd ()
push x = liftF (Push x ())

pop :: Cmd Elem
pop = liftF (Pop id)

popn :: Int -> Cmd [Elem]
popn n = liftF (PopN n id)

popnth :: Word32 -> Cmd Elem
popnth n = liftF (PopNth n id)

peek :: Cmd Elem
peek = liftF (Peek id)

peekn :: Int -> Cmd [Elem]
peekn n = liftF (PeekN n id)

peeknth :: Word32 -> Cmd Elem
peeknth n = liftF (PeekNth n id)

stacksize :: Cmd BN
stacksize = liftF (StackSize id)

-- alt stack
pushalt :: Elem -> Cmd ()
pushalt x = liftF (PushAlt x ())

popalt :: Cmd Elem
popalt = liftF (PopAlt id)

-- branch stack
pushbranch :: Branch -> Cmd ()
pushbranch b = liftF (PushBranch b ())

popbranch :: Cmd Branch
popbranch = liftF (PopBranch id)

-- num
bn2u32 :: BN -> Cmd Word32
bn2u32 n = liftF (Num2u32 n id)

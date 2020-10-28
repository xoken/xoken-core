{-# LANGUAGE DeriveFunctor #-}
module Network.Xoken.Script.Interpreter.Commands where

import           Data.Maybe                     ( mapMaybe )
import           Data.Word                      ( Word8
                                                , Word32
                                                , Word64
                                                )
import           Data.EnumBitSet                ( T
                                                , toEnums
                                                , get
                                                )
import           Data.Foldable                  ( toList )
import           Control.Monad.Free             ( Free(Pure, Free)
                                                , liftF
                                                )
import qualified Data.ByteString               as BS
import qualified Data.Sequence                 as Seq
import           Network.Xoken.Script.Common
import           Network.Xoken.Script.Interpreter.Util

maxOpsPerScript :: Integral a => Bool -> Bool -> a
maxOpsPerScript genesis consensus
  | not genesis = 500
  | otherwise   = fromIntegral (maxBound :: Word32)

maxScriptNumLength :: Integral a => Bool -> Bool -> a
maxScriptNumLength genesis consensus | not genesis = 4
                                     | consensus   = 750 * 1000
                                     | otherwise   = 250 * 1000

type Elem = BS.ByteString
type Stack a = Seq.Seq a

data InterpreterCommand a
    -- signal
    = Terminate InterpreterError
    | Success
    | NonTopLevelReturn a
    -- stack
    | Push Elem a
    | Pop (Elem -> a)
    | PopN Int ([Elem] -> a)
    | PopNth Word32 (Elem -> a)
    | Peek (Elem -> a)
    | PeekN Int ([Elem] -> a)
    | PeekNth Word32 (Elem -> a)
    | StackSize (Int -> a)
    -- alt stack
    | PushAlt Elem a
    | PopAlt (Elem -> a)
    -- branch stack
    | PushBranch Branch a
    | PopBranch (Branch -> a)
    -- num
    | Num2u32 BN (Word32 -> a)
    | LimitedNum Int Elem (BN -> a)
    | Num Elem (BN -> a)
    | Arith [Elem] ([BN] -> a)
    -- field access
    | Flag ScriptFlag (Bool -> a)
    | Flags (ScriptFlags -> a)
    | Checker (BaseSignatureChecker -> a)
    | ScriptEndToHash ([ScriptOp] -> a)
    | Consensus (Bool -> a)
    | OpCount (Word64 -> a)
    | AddToOpCount Word64 a
    | MaxNumLength (Int -> a)
    deriving (Functor)

type Cmd = Free InterpreterCommand

data InterpreterError
  = StackUnderflow
  | ImpossibleEncoding
  | PushSize
  | Unimplemented ScriptOp
  | HigherLevelImplementation ScriptOp
  | UnbalancedConditional
  | InvalidAltstackOperation
  | InvalidNumberRange
  | InvalidSplitRange
  | InvalidOperandSize
  | Verify
  | EqualVerify
  | NumEqualVerify
  | CheckSigVerify
  | CheckMultiSigVerify
  | MinimalIf
  | MinimalData
  | DiscourageUpgradableNOPs
  | OpReturn
  | DivByZero
  | ModByZero
  | BadOpcode ScriptOp
  | NumOverflow
  | NegativeLocktime
  | UnsatisfiedLockTime
  | SigNullFail
  | SigNullDummy
  | InvalidSigOrPubKey
  | PubKeyCount
  | PubKeyType
  | NonCompressedPubKey
  | SigCount
  | InvalidOpCount
  | NonMinimalNum
  deriving (Show, Eq)

data Env = Env
  { stack :: Stack Elem
  , alt_stack :: Stack Elem
  , branch_stack :: Stack Branch
  , failed_branches :: Word32
  , non_top_level_return :: Bool
  , script_flags :: ScriptFlags
  , base_signature_checker :: BaseSignatureChecker
  , script_end_to_hash :: [ScriptOp]
  , is_consensus :: Bool
  , op_count :: Word64
  }

stack_equal x e = e { stack = x }
alt_stack_equal x e = e { alt_stack = x }

data Branch = Branch
  { satisfied :: Bool
  , is_else_branch :: Bool
  } deriving (Show, Eq)

rindex :: Int -> Env -> Int
rindex i e = length (stack e) - i

truth :: Integral a => Bool -> a
truth x = if x then 1 else 0

data CmdResult = OK | Error InterpreterError | Return

interpretCmd :: Cmd () -> Env -> (Env, CmdResult)
interpretCmd = go where
  go (Pure ()) e = (e, OK)
  go (Free x ) e = case x of
    -- signal
    Terminate error     -> (e, Error error)
    Success             -> (e, Return)
    NonTopLevelReturn m -> go m (e { non_top_level_return = True })
    -- stack
    Push x m            -> go m (e { stack = stack e Seq.|> x })
    Pop k               -> case Seq.viewr (stack e) of
      rest Seq.:> x -> go (k x) (e { stack = rest })
      _             -> (e, Error StackUnderflow)
    PopN n k | length topn == n -> go (k $ toList topn) (e { stack = rest })
             | otherwise        -> (e, Error StackUnderflow)
      where (rest, topn) = Seq.splitAt (rindex n e) (stack e)
    PopNth n k -> case stack e Seq.!? i of
      Just x -> go (k x) (e { stack = Seq.deleteAt i (stack e) })
      _      -> (e, Error StackUnderflow)
      where i = rindex (fromIntegral n) e - 1
    Peek k -> case Seq.viewr (stack e) of
      _ Seq.:> x -> go (k x) e
      _          -> (e, Error StackUnderflow)
    PeekN n k | length topn == n -> go (k $ toList topn) e
              | otherwise        -> (e, Error StackUnderflow)
      where topn = Seq.drop (rindex n e) (stack e)
    PeekNth n k -> case stack e Seq.!? i of
      Just x -> go (k x) e
      _      -> (e, Error StackUnderflow)
      where i = rindex (fromIntegral n) e - 1
    StackSize k -> go (k $ length $ stack e) e
    -- alt stack
    PushAlt x m -> go m (e { alt_stack = x Seq.<| alt_stack e })
    PopAlt k    -> case Seq.viewl (alt_stack e) of
      x Seq.:< rest -> go (k x) (e { alt_stack = rest })
      _             -> (e, Error InvalidAltstackOperation)
    -- branch stack
    PushBranch b m -> go
      m
      (e { branch_stack    = b Seq.<| branch_stack e
         , failed_branches = failed_branches e + truth (not $ satisfied b)
         }
      )
    PopBranch k -> case Seq.viewl (branch_stack e) of
      b Seq.:< rest -> go
        (k b)
        (e { branch_stack    = rest
           , failed_branches = failed_branches e - truth (not $ satisfied b)
           }
        )
      _ -> (e, Error UnbalancedConditional)
    -- num
    Num2u32 n k -> if n >= 0 && n <= fromIntegral (maxBound :: Word32)
      then go (k $ fromIntegral n) e
      else (e, Error InvalidNumberRange)
    LimitedNum n x k -> if BS.length x <= n
      then go (k $ (bin2num x :: BN)) e
      else (e, Error NumOverflow)
    Num bs k -> case num bs of
      Just n -> go (k n) e
      _      -> (e, Error NonMinimalNum)
    Arith xs k -> if length xs /= length ys
      then (e, Error NonMinimalNum)
      else go (k ys) e
      where ys = mapMaybe num xs
    -- field access
    Flag f k          -> go (k $ flag f) e
    Flags           k -> go (k $ script_flags e) e
    Checker         k -> go (k $ base_signature_checker e) e
    ScriptEndToHash k -> go (k $ script_end_to_hash e) e
    Consensus       k -> go (k c) e
    OpCount         k -> go (k $ op_count e) e
    AddToOpCount x m  -> if opcount' > maxOpsPerScript genesis c
      then (e { op_count = opcount' }, Error InvalidOpCount)
      else go m (e { op_count = opcount' })
      where opcount' = op_count e + x
    MaxNumLength k -> go (k maxNumLength) e
   where
    c = is_consensus e
    flag x = get x (script_flags e)
    genesis      = flag UTXO_AFTER_GENESIS
    maxNumLength = maxScriptNumLength genesis c
    num          = bin2num' (flag VERIFY_MINIMALDATA) maxNumLength

-- signal
terminate :: InterpreterError -> Cmd ()
terminate e = liftF (Terminate e)

success :: Cmd ()
success = liftF Success

nontoplevelreturn :: Cmd ()
nontoplevelreturn = liftF (NonTopLevelReturn ())

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

stacksize :: Cmd Int
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

limitednum :: Int -> Elem -> Cmd BN
limitednum n x = liftF (LimitedNum n x id)

num' :: Elem -> Cmd BN
num' x = liftF (Num x id)

arith :: [Elem] -> Cmd [BN]
arith xs = liftF (Arith xs id)

-- field access
flag :: ScriptFlag -> Cmd Bool
flag f = liftF (Flag f id)

flags :: Cmd ScriptFlags
flags = liftF (Flags id)

checker :: Cmd BaseSignatureChecker
checker = liftF (Checker id)

scriptendtohash :: Cmd [ScriptOp]
scriptendtohash = liftF (ScriptEndToHash id)

consensus :: Cmd Bool
consensus = liftF (Consensus id)

opcount :: Cmd Word64
opcount = liftF (OpCount id)

addtoopcount :: Word64 -> Cmd ()
addtoopcount x = liftF (AddToOpCount x ())

maxnumlength :: Cmd Int
maxnumlength = liftF (MaxNumLength id)

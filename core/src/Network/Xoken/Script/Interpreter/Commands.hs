{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.Xoken.Script.Interpreter.Commands where

import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Data.Int                       ( Int64 )
import           Data.EnumBitSet                ( T
                                                , toEnums
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
type ScriptFlags = T Word ScriptFlag

data InterpreterCommands a
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
    | StackSize (BN -> a)
    -- alt stack
    | PushAlt Elem a
    | PopAlt (Elem -> a)
    -- branch stack
    | PushBranch Branch a
    | PopBranch (Branch -> a)
    -- num
    | Num2u32 BN (Word32 -> a)
    | LimitedNum Int Elem (Int64 -> a)
    -- field access
    | Flags (ScriptFlags -> a)
    | Checker (BaseSignatureChecker -> a)
    | ScriptEndToHash ([ScriptOp] -> a)
    deriving (Functor)

type Cmd = Free InterpreterCommands

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
  | SigNullfail
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
  }

type Signature = Elem
type PublicKey = Elem
type EnabledSighashForkid = Bool

data BaseSignatureChecker = BaseSignatureChecker
  { checkSig :: Signature -> PublicKey -> Script -> EnabledSighashForkid -> Bool
  , checkLockTime :: Int64 -> Bool
  , checkSequence :: Int64 -> Bool
  }

instance Show ScriptFlags where
  show = show . toEnums

data ScriptFlag
  = VERIFY_NONE
  | VERIFY_P2SH
  | VERIFY_STRICTENC
  | VERIFY_DERSIG
  | VERIFY_LOW_S
  | VERIFY_NULLDUMMY
  | VERIFY_SIGPUSHONLY
  | VERIFY_MINIMALDATA
  | VERIFY_DISCOURAGE_UPGRADABLE_NOPS
  | VERIFY_CLEANSTACK
  | VERIFY_CHECKLOCKTIMEVERIFY
  | VERIFY_CHECKSEQUENCEVERIFY
  | VERIFY_MINIMALIF
  | VERIFY_NULLFAIL
  | VERIFY_COMPRESSED_PUBKEYTYPE
  | ENABLE_SIGHASH_FORKID
  | GENESIS
  | UTXO_AFTER_GENESIS
  deriving (Show, Enum)

data Branch = Branch
  { satisfied :: Bool
  , is_else_branch :: Bool
  } deriving (Show, Eq)

rindex :: Int -> Env -> Int
rindex i e = length (stack e) - 1 - i

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
      where i = rindex (fromIntegral n) e
    Peek k -> case Seq.viewr (stack e) of
      _ Seq.:> x -> go (k x) e
      _          -> (e, Error StackUnderflow)
    PeekN n k | length topn == n -> go (k $ toList topn) e
              | otherwise        -> (e, Error StackUnderflow)
      where topn = Seq.drop (rindex n e) (stack e)
    PeekNth n k -> case stack e Seq.!? rindex (fromIntegral n) e of
      Just x -> go (k x) e
      _      -> (e, Error StackUnderflow)
    StackSize k -> go (k $ BN $ fromIntegral $ length $ stack e) e
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
    Num2u32 n k -> case num2u32 n of
      Just u -> go (k u) e
      _      -> (e, Error InvalidNumberRange)
    LimitedNum n x k -> if BS.length x <= n
      then go (k $ fromIntegral $ (bin2num x :: BN)) e
      else (e, Error NumOverflow)
    -- field access
    Flags           k -> go (k $ script_flags e) e
    Checker         k -> go (k $ base_signature_checker e) e
    ScriptEndToHash k -> go (k $ script_end_to_hash e) e

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

limitednum :: Int -> Elem -> Cmd Int64
limitednum n x = liftF (LimitedNum n x id)

-- field access
flags :: Cmd ScriptFlags
flags = liftF (Flags id)

checker :: Cmd BaseSignatureChecker
checker = liftF (Checker id)

scriptendtohash :: Cmd [ScriptOp]
scriptendtohash = liftF (ScriptEndToHash id)

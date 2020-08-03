module Network.Xoken.Script.Interpreter.Internal where

import Control.Monad.State
import qualified Data.ByteString as BS

type Stack = [StackElement]

type Interpreter = State Stack (Maybe StackElement)

data StackElement =
      D BS.ByteString
    | I Int
    | Bottom
    deriving (Show, Ord, Eq, Read)

peek :: State Stack (Maybe StackElement)
peek = state $ \s -> case s of
    [] -> (Just $ Bottom, [])
    s' -> (Just $ head s', s')

pop :: State Stack (Maybe StackElement)
pop = state $ \s -> case s of
    [] -> (Just $ Bottom, [])
    s' -> (Just $ head s', tail s')

push :: StackElement -> State Stack (Maybe StackElement)
push e = state $ \s -> (Nothing, e:s)

runInterpreter :: Interpreter -> Stack
runInterpreter i = snd $ runState i []

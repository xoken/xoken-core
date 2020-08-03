{-# LANGUAGE OverloadedStrings #-}

module Network.Xoken.Script.Interpreter.Operations where

import qualified Data.ByteString as BS
import Network.Xoken.Script.Common
import Network.Xoken.Script.Interpreter.Internal

pushData :: ScriptOp -> Maybe StackElement
pushData (OP_PUSHDATA d _) = Just $ D d
pushData (OP_1NEGATE) = Just $ I (-1)
pushData so =
    case scriptOpToInt so of
        Left err -> Nothing
        Right i -> Just $ I i

equal :: StackElement -> StackElement -> StackElement
equal e1 e2 = if e1 == e2
    then I 1
    else I 0

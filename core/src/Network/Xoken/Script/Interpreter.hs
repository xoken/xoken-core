module Network.Xoken.Script.Interpreter where

import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Network.Xoken.Script.Common
import Network.Xoken.Script.Interpreter.Internal
import Network.Xoken.Script.Interpreter.Operations
import Network.Xoken.Crypto.Hash

import qualified Network.Xoken.Script.Interpreter.Operations as OP

toy :: Script -> Interpreter
toy script =
    case scriptOps script of
        so:s -> if isPushOp so
                    then do
                        let res = OP.pushData so
                        case res of
                            Nothing -> toy $ script { scriptOps = s}
                            (Just d) -> do
                                push d
                                toy $ script { scriptOps = s }
                    else case so of
                        OP_DUP -> do
                            t <- pop
                            if t == Nothing
                                then toy $ script { scriptOps = s}
                                else do
                                    push $ fromJust t
                                    push $ fromJust t
                        OP_EQUAL -> do
                            o1 <- pop
                            o2 <- pop
                            if Nothing `elem` [o1, o2]
                                then toy $ script { scriptOps = s }
                                else do
                                    push $ OP.equal (fromJust o1) (fromJust o2)
                                    toy $ script { scriptOps = s }
                        _ -> toy $ script { scriptOps = s }
        [] -> peek

sampleScript = Script [OP_1, OP_2, OP_1NEGATE, OP_1, OP_1, OP_EQUAL]

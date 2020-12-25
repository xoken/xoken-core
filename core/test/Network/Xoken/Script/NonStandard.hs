module Network.Xoken.Script.NonStandard where

import           Network.Xoken.Script.Interpreter.Util
import           Network.Xoken.Script.Common

int = opPushData . int2BS

non_standard_0 = Script
    [ int 1
    , int 64
    , int $ -125
    , int $ -126
    , int $ -127
    , int 3
    , int 20
    , OP_NOP
    , opPush
        0x39396136363338302d343037322d313165622d626135662d62316335336264353966373740627574746f6e6f666d6f6e65792e636f6d
    , opPush
        0x025f44adfa89cc33c42def4165be539a523c3d3fd7e537bc1e9bd44b32fa341cfd
    , int 0
    , int 0
    , int 3
    , OP_PICK
    , int 2
    , OP_ROLL
    , OP_DROP
    , int 1
    , OP_ROLL
    , int 2
    , OP_PICK
    , int 1
    , OP_ROLL
    , OP_DROP
    , OP_NOP
    , OP_NOP
    , int 14
    , OP_PICK
    , OP_NOP
    , int 0
    , OP_PICK
    , opPush 0x97dfd76851bf465e8f715593b217714858bbe9570ff3bd5e33840a34e20ff026
    , opPush
        0x02ba79df5f8ae7604a9830f03c7933028186aede0675a16f025dc4f8be8eec0382
    , opPush
        0x0ac407f0e4bd44bfc207355a778b046225a7068fc59ee7eda43ad905aadbffc800
    , opPush 0x6c266b30e6a1319c66dc401e5bd6b432ba49688eecd118297041da8074ce0810
    , opPush 0x1008ce7480da41702918d1ec8e6849ba32b4d65b1e40dc669c31a1e6306b266c
    , int 17
    , OP_PICK
    , int 17
    , OP_PICK
    , OP_OR
    , int 6
    , OP_PICK
    , OP_HASH256
    , OP_NOP
    , OP_NOP
    , int 0
    , OP_PICK
    , int 0
    , OP_PICK
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , int 0
    , OP_CAT
    , OP_BIN2NUM
    , OP_NIP
    , OP_NOP
    , int 7
    , OP_PICK
    , int 6
    , OP_PICK
    , int 6
    , OP_PICK
    , int 6
    , OP_PICK
    , int 6
    , OP_PICK
    , int 3
    , OP_PICK
    , int 6
    , OP_PICK
    , int 4
    , OP_PICK
    , int 7
    , OP_PICK
    , OP_MUL
    , OP_ADD
    , OP_MUL
    , opPush
        0x414136d08c5ed2bf3ba048afe6dcaebafeffffffffffffffffffffffffffffff00
    , OP_NOP
    , int 1
    , OP_PICK
    , int 1
    , OP_PICK
    , int 1
    , OP_PICK
    , int 1
    , OP_PICK
    , OP_MOD
    , int 2
    , OP_ROLL
    , OP_DROP
    , int 1
    , OP_ROLL
    , int 1
    , OP_PICK
    , int 0
    , OP_LESSTHAN
    , OP_IF
    , int 1
    , OP_PICK
    , int 1
    , OP_PICK
    , OP_ADD
    , int 2
    , OP_ROLL
    , OP_DROP
    , int 1
    , OP_ROLL
    , OP_ENDIF
    , int 1
    , OP_PICK
    , OP_NIP
    , OP_NIP
    , OP_NOP
    , int 2
    , OP_ROLL
    , OP_DROP
    , int 1
    , OP_ROLL
    , int 1
    , OP_PICK
    , int 1
    , OP_PICK
    , int 2
    , OP_DIV
    , OP_GREATERTHAN
    , OP_IF
    , int 0
    , OP_PICK
    , int 2
    , OP_PICK
    , OP_SUB
    , int 2
    , OP_ROLL
    , OP_DROP
    , int 1
    , OP_ROLL
    , OP_ENDIF
    , int 3
    , OP_PICK
    , OP_SIZE
    , OP_NIP
    , int 2
    , OP_PICK
    , OP_SIZE
    , OP_NIP
    , int 4
    , int 2
    , OP_PICK
    , OP_ADD
    , int 1
    , OP_PICK
    , OP_ADD
    , int 48
    , int 1
    , OP_PICK
    , OP_CAT
    , int 2
    , OP_CAT
    , int 3
    , OP_PICK
    , OP_CAT
    , int 7
    , OP_PICK
    , OP_CAT
    , int 2
    , OP_CAT
    , int 2
    , OP_PICK
    , OP_CAT
    , int 5
    , OP_PICK
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , int 1
    , OP_SPLIT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_SWAP
    , OP_CAT
    , OP_CAT
    , int 6
    , OP_PICK
    , OP_CAT
    , int 0
    , OP_PICK
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NOP
    , int 0
    , OP_PICK
    , int 7
    , OP_PICK
    , OP_CHECKSIG
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NOP
    , OP_NIP
    , OP_NOP
    , OP_VERIFY
    , int 12
    , OP_PICK
    , int 0
    , OP_EQUAL
    , OP_NOT
    , OP_IF
    , OP_NOP
    , int 14
    , OP_PICK
    , OP_NOP
    , int 0
    , OP_PICK
    , int 104
    , OP_SPLIT
    , OP_NIP
    , int 0
    , int 0
    , int 2
    , OP_PICK
    , int 1
    , OP_SPLIT
    , OP_DROP
    , int 0
    , OP_SPLIT
    , OP_NIP
    , int 0
    , OP_PICK
    , int 14
    , OP_PICK
    , OP_EQUAL
    , OP_IF
    , OP_NOP
    , int 3
    , OP_PICK
    , int 3
    , OP_SPLIT
    , OP_DROP
    , int 1
    , OP_SPLIT
    , OP_NIP
    , int 0
    , OP_PICK
    , int 0
    , OP_CAT
    , OP_BIN2NUM
    , OP_NIP
    , OP_NOP
    , int 3
    , OP_ROLL
    , OP_DROP
    , int 2
    , OP_ROLL
    , int 2
    , OP_ROLL
    , int 3
    , OP_PICK
    , int 3
    , int 4
    , OP_PICK
    , OP_ADD
    , OP_SPLIT
    , OP_DROP
    , int 3
    , OP_SPLIT
    , OP_NIP
    , int 2
    , OP_ROLL
    , OP_DROP
    , int 1
    , OP_ROLL
    , OP_ELSE
    , int 0
    , OP_PICK
    , int 13
    , OP_PICK
    , OP_EQUAL
    , OP_IF
    , OP_NOP
    , int 3
    , OP_PICK
    , int 5
    , OP_SPLIT
    , OP_DROP
    , int 1
    , OP_SPLIT
    , OP_NIP
    , int 0
    , OP_PICK
    , int 0
    , OP_CAT
    , OP_BIN2NUM
    , OP_NIP
    , OP_NOP
    , int 3
    , OP_ROLL
    , OP_DROP
    , int 2
    , OP_ROLL
    , int 2
    , OP_ROLL
    , int 3
    , OP_PICK
    , int 5
    , int 4
    , OP_PICK
    , OP_ADD
    , OP_SPLIT
    , OP_DROP
    , int 5
    , OP_SPLIT
    , OP_NIP
    , int 2
    , OP_ROLL
    , OP_DROP
    , int 1
    , OP_ROLL
    , OP_ELSE
    , int 0
    , OP_PICK
    , int 12
    , OP_PICK
    , OP_EQUAL
    , OP_IF
    , OP_NOP
    , int 3
    , OP_PICK
    , int 9
    , OP_SPLIT
    , OP_DROP
    , int 1
    , OP_SPLIT
    , OP_NIP
    , int 0
    , OP_PICK
    , int 0
    , OP_CAT
    , OP_BIN2NUM
    , OP_NIP
    , OP_NOP
    , int 3
    , OP_ROLL
    , OP_DROP
    , int 2
    , OP_ROLL
    , int 2
    , OP_ROLL
    , int 3
    , OP_PICK
    , int 9
    , int 4
    , OP_PICK
    , OP_ADD
    , OP_SPLIT
    , OP_DROP
    , int 9
    , OP_SPLIT
    , OP_NIP
    , int 2
    , OP_ROLL
    , OP_DROP
    , int 1
    , OP_ROLL
    , OP_ELSE
    , OP_NOP
    , int 3
    , OP_PICK
    , int 1
    , OP_SPLIT
    , OP_DROP
    , int 0
    , OP_SPLIT
    , OP_NIP
    , int 0
    , OP_PICK
    , int 0
    , OP_CAT
    , OP_BIN2NUM
    , OP_NIP
    , OP_NOP
    , int 3
    , OP_ROLL
    , OP_DROP
    , int 2
    , OP_ROLL
    , int 2
    , OP_ROLL
    , int 3
    , OP_PICK
    , int 1
    , int 4
    , OP_PICK
    , OP_ADD
    , OP_SPLIT
    , OP_DROP
    , int 1
    , OP_SPLIT
    , OP_NIP
    , int 2
    , OP_ROLL
    , OP_DROP
    , int 1
    , OP_ROLL
    , OP_ENDIF
    , OP_ENDIF
    , OP_ENDIF
    , int 1
    , OP_PICK
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NOP
    , OP_NIP
    , OP_NOP
    , OP_NOP
    , int 0
    , int 1
    , OP_PICK
    , int 0
    , OP_PICK
    , OP_SIZE
    , OP_NIP
    , int 1
    , OP_PICK
    , int 1
    , OP_PICK
    , int 11
    , OP_PICK
    , OP_SUB
    , OP_SPLIT
    , OP_NIP
    , OP_BIN2NUM
    , int 2
    , OP_PICK
    , int 2
    , OP_PICK
    , int 2
    , OP_PICK
    , OP_SUB
    , int 12
    , OP_PICK
    , OP_SUB
    , OP_SPLIT
    , OP_NIP
    , int 4
    , OP_ROLL
    , OP_DROP
    , int 3
    , OP_ROLL
    , int 3
    , OP_ROLL
    , int 3
    , OP_ROLL
    , OP_DROP
    , OP_DROP
    , OP_DROP
    , OP_NOP
    , int 13
    , OP_PICK
    , OP_HASH160
    , OP_NOP
    , int 1
    , OP_PICK
    , int 0
    , OP_PICK
    , int 9
    , OP_PICK
    , OP_SPLIT
    , OP_DROP
    , int 0
    , OP_SPLIT
    , OP_NIP
    , OP_NIP
    , OP_NOP
    , OP_EQUAL
    , OP_VERIFY
    , int 14
    , OP_PICK
    , int 14
    , OP_PICK
    , OP_CHECKSIG
    , OP_VERIFY
    , OP_DROP
    , OP_DROP
    , OP_ENDIF
    , int 13
    , OP_PICK
    , int 1
    , OP_PICK
    , OP_CHECKSIG
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_NIP
    , OP_RETURN
    , opPush 0x161242cc207451e587b0b3a6f016163933d228fe40420f00000000001c0000
    ]

module TFTP where

import Data.Word

-- TODO consider using Word16 instead of Integer?
data Packet =
    RRQ String String -- opcode 1 + filename + 0 + mode + 0
    | WRQ String String -- opcode 2 + filename + 0 + mode + 0
    | DATA Integer [Word8] -- opcode 3 + block number + data TODO also consider strict/lazy bytestrings
    | ACK Integer -- opcode 4 + block number
    | ERROR Integer String deriving Show -- opcode 5 + error code + error message + 0


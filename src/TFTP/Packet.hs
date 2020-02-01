module TFTP.Packet where

import Data.Word
import Data.ByteString

data Packet =
    RRQ String String -- opcode 1 + filename + 0 + mode + 0
    | WRQ String String -- opcode 2 + filename + 0 + mode + 0
    | DATA Word16 ByteString -- opcode 3 + block number + data TODO also consider lazy bytestrings
    | ACK Word16 -- opcode 4 + block number
    | ERROR Word16 String deriving Show -- opcode 5 + error code + error message + 0
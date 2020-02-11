module TFTP.Constants where

dataSize = 512 :: Int
packetSize = dataSize + 2 + 2 + 8
--                      ^ block number
--                          ^ opcode
--                              ^ UDP header size

defaultPort = 69

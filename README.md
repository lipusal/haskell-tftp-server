# haskell-tftp-server - Caesar mode server
In this branch, the server supports `caesar` mode: Accepts only netascii files and performs the [Caesar cipher](https://en.wikipedia.org/wiki/Caesar_cipher) on them, only in RRQ.

## Installation

1. [Install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade)
1. `stack setup` installs dependencies
1. `stack build` compiles the program

## Usage

1. `stack run [portNum=69]`
2. Check out `caesar-client` and read the [Client's instructions](https://github.com/lipusal/haskell-tftp-server/blob/caesar-client/README.md)

Note that all other functions still work normally, this branch only adds support for Caesar mode.
# haskell-tftp-server - Caesar mode client
In this branch, the main executable is replaced with a client (not a server) that performs an RRQ request in `caesar` mode.

## Installation

1. [Install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade)
1. `stack setup` installs dependencies
1. `stack build` compiles the program

## Usage

0. Check out `caesar-server` and read the [Servers's instructions](https://github.com/lipusal/haskell-tftp-server/blob/caesar-server/README.md)
1. Checkout this branch
1. `stack run <ip> <port> <filename>`, where `filename` is relative to where the server is running

This will perform an RRQ request in `caesar` mode, writing to a file with the same name as the remote file.
The client exits after error or after a successful transfer.

**NOTE:** The client does not decipher the file. For that, use the decoder Ruby script with `ruby decoder.rb <filename>`, which will read the cipher key from the file and print the deciphered file to STDOUT. Compare that with the original file to make sure everything is in place.

# haskell-tftp-server
This is an RFC 1350-compliant TFPT server, implemented as a final for a Functional Programming class.

## Installation

1. [Install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade)
1. `stack setup` installs dependencies
1. `stack build` compiles the program

## Usage

1. `stack run [portNum=69]`
2. Interrupt to stop (ie. ctrl/cmd + c). Note that this doesn't seem to terminate the program correctly on Windows (:disappointed:) so you might need to forcibly terminate it through Task Manager.

This automatically compiles the program before running, so you don't need to run `stack build` again if you made any changes.

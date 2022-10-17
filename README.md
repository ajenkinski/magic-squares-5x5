# Generate all 5x5 magic squares

This project implements the algorithm described at 
https://www.researchgate.net/publication/294288450_Generation_of_all_magic_squares_of_order_5_and_interesting_patterns_finding
I have also checked in a copy of the paper as a PDF [here](./Generation_of_all_magic_squares_of_order_5_and_int.pdf).

## Setup

This project is written in Haskell, and uses cabal to build. If you don't already have the haskell toolchain installed, the easiest way to install it on Linux or Mac is to use [GHCUp](https://www.haskell.org/ghcup/).  Just follow the directions on that page to install the haskell toolchain into ~/.ghcup.   You can accept the defaults from the ghcup install script.

After running the install, run `source ~/.ghcup/env` to add the haskell tools to your path. You may also want to add this to your ~/.bashrc.

## Running

Once ghcup is installed, you can run the magic square generator as follows.

To run single-threaded, run 

```
cabal run magic-squares-5x5
```

To run in multi-threaded mode, run 

```
cabal run magic-squares-5x5 -- +RTS -N16
```

where the number after `-N` specifies how many threads to start up.  Currently the program won't take advantage more than 13 threads.
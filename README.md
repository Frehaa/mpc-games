# Installation

Follow the instructions for MP-SPDZ, we are able to get everything working with
the following commands, but your mileage may vary.

```
cd MP-SPDZ
git submodule update --init --recursive
apt-get install automake build-essential git libboost-dev libboost-thread-dev libntl-dev libsodium-dev libssl-dev libtool m4 python3 texinfo yasm
make tldr
make mascot
```

With MP-SPDZ correctly set up, you need to make sure ``generateMPC.hs`` and
``generateRandomTree.hs`` are compiled. We used the Glasgow Haskell Compiller 
which can be installed and used as follows.

```
apt-get install haskell-platform
ghc generateMPC.hs
ghc generateRandomTree.hs
```

# Test

If everything was installed correctly then you should be able to compile and run
as follows:

```
./generateMPC "[3, 5]" < exampleTree > exampleTree.mpc
cd ../MP-SPDZ
./compile.py -M ../src/exampleTree.mpc
./Scripts/mascot.sh -IF ../src/ExampleTree-Input exampleTree
```

If everything works then it should output the following, along with various
information about the execution. 

```
The selected outcome (TUMO) is 2
0 should pay 469 at TUMO
1 should be paid 469 at TUMO
0's fine for deviating anywhere is 469
1's fine for deviating anywhere is 381
```
# Run benchmarks 

At this point everything should be ready, so you should be able to run the benchmarking script. The output will be
located in the ``outputs`` folder. 

```
chmod +x run_benchmarks.sh
./run_benchmarks.sh
```

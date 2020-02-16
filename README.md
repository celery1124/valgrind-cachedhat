# Dynamic heap analysis tool for DRAM request using valgrind

## Description

This project aims at profiling heap memory access after cache. It combines valgrind existing tools cachegrind and dhat. The new tool added is cachedhat.

## Build

In project root directory

``` bash
mkdir build
./configure --prefix=`pwd`/build
make -j4
make install
```

## Run

After build, binary is installed in **build** directory. Run valgrind with the new binary compiled from source.

The tool parameters are currently same as cachegrind.

Heap profile is saved in heapProfile.out.pid in the run directory. You can analyze this result by the html included in dhat tool.

``` bash
# DIR is project root directory
$DIR/build/bin/valgrind --tool=cachedhat --I1=49152,3,64 --D1=32768,2,64 --LL=1048576,16,64 program
```

## TODO

Add access pattern analysis.  For example, for each malloc region, clssify access pattern as sequential,random,stride, etc.

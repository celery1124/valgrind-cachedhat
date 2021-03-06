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

Heap profile is saved in heapProfile.out.pid in the run directory. You can analyze this result by the html included in dhat tool. Heap profile heatmap (time vs address) is saved in heapProfileHM(R/W).out.pid in the run directory.

Heap profile heatmap has several parameters to control.  

```
--ts-read-res controls the time resolution (here time is actual count of memory reference rather than real processor cycles) for read requests heatmap. 
--ts-write-res controls the time resolution (here time is actual count of memory reference rather than real processor cycles) for write requests heatmap.  
--mem-read-res controls the memory resolution for read reqeusts profile, finer granularity requires more space to store the heatmap. 
--mem-write-res controls the memory resolution for write reqeusts profile, finer granularity requires more space to store the heatmap. 
--hm-read=(**yes**/no) controls whether record the read requests heatmaps.
--hm-write=(yes/**no**) controls whether record the write requests heatmaps.
--hm-size-limit controls the heap region size limit to record for heatmap. (Better set this as same as the LLC size). 
--hm-writes-limit controls the lower limit of the total writes bytes to record heatmap.
--hm-reads-limit controls the lower limit of the total reads bytes to record heatmap.
```

Here is an example of the running script.

``` bash
# DIR is project root directory
$DIR/build/bin/valgrind --tool=cachedhat --I1=49152,3,64 --D1=32768,2,64 --LL=1048576,16,64 --ts-res=16384 --mem-res=4096 --hm_size_limit 1048576 --hm-read=no --hm-write=yes program
```

## Heap profile heatmap (DRAM request) visualize 

Heap profile heatmap is used to identify memory access pattern on the off-chip memory.  A python script is written to plot the heatmap for each heap region that is profiled.  Noted the python need to run with version >=3.4


``` bash
# DIR is project root directory
$DIR/cachedhat/plotHM.py heapProfileHM(R/W).out.pid outDir
```

## TODO

Add automatical access pattern analysis.  For example, for each malloc region, classify access pattern as sequential,random,stride, etc.

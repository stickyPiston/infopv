# BOOST – BOunded fOrward Symbolic execuTion verification

Based on the [GCL Parser used in the course _Program Verification_](https://github.com/wooshrow/gclparser) by Stefan Koppier and Wishnu Prasetya.

## Compilation
To compile the library, run the command `> cabal build` from your console.

## Running
Run using `> cabal run infopv -- [-n|--n INT] [--ph None|LengthBased|Full] [-p|--print-tree] [--no-simplify] FILE`

The .gcl to verify is mandatory.

The optional command line options are:
- n, setting the maximum path length
- ph, setting the prune heuristic between None, LengthBased, and Full
- p or print-tree, to print the computation tree to the console, changing the show instance in `verifier/Tree.hs` lets you choose between a simple ASCII representation, or a valid Graphviz format
- no-simplify, to turn off the simplifier that is by default enabled

For example `> cabal run infopv -- -n 50 --ph Full -p examples/E.gcl`

## Benchmarking
To enable the benchmarks, defined in `bench/Benchmark.hs`, run `> cabal configure --enable-benchmarks` and rebuild the project.

To run the benchmarks, run `> cabal bench`
To export results of the benchmarks to csv and to generate a nice .html report, run `cabal bench verifier-bench --benchmark-options="--output=results.html --csv=results.csv"`
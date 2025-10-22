# GCL Parser

Based on the [GCL Parser used in the course _Program Verification_](https://github.com/wooshrow/gclparser) by Stefan Koppier and Wishnu Prasetya.

#### Compilation
To compile the library, run the command `> cabal build` from your console.

#### Running
Run using `> cabal run infopv [--k INT] [--n INT] [--ph HEURISTIC] FILE`

#### Benchmarking
To enable the benchmarks, defined in `/benchmark/Benchmark.hs`, run `> cabal configure --enable-benchmarks` and rebuild the project.

To run the benchmarks, run `> cabal bench`

#### Supported GCL syntax

See in `/docs`.

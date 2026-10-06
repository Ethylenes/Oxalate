# Oxalate (OCaml Micro XLA)

``` bash
opam install dune llvm ctypes ctypes-foreign
dune build
dune exec ./_build/default/bin/main.exe # -- dump
```

# Pipeline

- `hlo.ml` defines the intermediate representation (specifically mimicking XLA's HLO), a builder for constructing it, text printer, and slow interpreter (for ref)
- `opt.ml` is responsible for constant folding, algebraic simplification, common subexpression elimination, and dead code elimination
- `jit.ml` converts the optimized graph into one fused loop, optimizes with LLVM, JIT-compiles it, then finally wraps it as an OCaml function
- `main.ml` just contains a few test cases

# Output
``` bash
== HLO (as written) ==
HloModule main
ENTRY main{
  v0 = f64[1000000] parameter(0)
  v1 = f64[1000000] parameter(1)
  v2 = f64[1000000] multiply(v0, v1)
  v3 = f64[1000000] constant(3)
  v4 = f64[1000000] constant(2)
  v5 = f64[1000000] multiply(v4, v3)
  v6 = f64[1000000] constant(1)
  v7 = f64[1000000] add(v2, v5)
  v8 = f64[1000000] multiply(v7, v6)
  v9 = f64[1000000] constant(0)
  v10 = f64[1000000] maximum(v8, v9)
  v11 = f64[1000000] multiply(v1, v0)
  v12 = f64[1000000] negate(v0)
  ROOT v13 = f64[1000000] add(v10, v11)
}

== HLO (optimized) ==
HloModule main
ENTRY main{
  v0 = f64[1000000] parameter(0)
  v1 = f64[1000000] parameter(1)
  v2 = f64[1000000] multiply(v0, v1)
  v3 = f64[1000000] constant(6)
  v4 = f64[1000000] add(v2, v3)
  v5 = f64[1000000] constant(0)
  v6 = f64[1000000] maximum(v4, v5)
  ROOT v7 = f64[1000000] add(v2, v6)
}

interpreter (ref)      1395.2 ms
JIT kernel             7.5 ms
results match: true  (out[0..2] = 6 6.9093 5.2432)
```

> Test cases are very sparse and the JIT-ed time excludes LLVM compile time and the Bigarray copy-in, so unfortunately no 180x speedup

Outputted text using `-- dump` is stored in `dumped.txt`

# Todo

- Add more `op`'s, subexpressions, etc.
- Add activation functions
- Re-add autograd
- Better testing, average tests over multiple trials, more testcases

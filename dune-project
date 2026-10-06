(lang dune 3.24)

(name oxalate)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Ethylenes")

(maintainers "Ethylenes")

(license MIT)

(package
 (name oxalate)
 (allow_empty)
 (synopsis "OCaml Micro XLA")
 (description "JIT compiler using OCaml + LLVM for XLA-like representations")
 (depends ocaml)
)

(lang dune 3.6)

(name seq)

(generate_opam_files true)

(source
 (github aytao/ParallelSequences))

(authors "Andrew Tao")

(maintainers "Andrew Tao")

(license "ISC")

(package
 (name seq)
 (synopsis
  "A shared-memory parallelism implementation of the parallel sequences abstraction.")
 (depends
  (ocaml
   (>= "5.0"))))

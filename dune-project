(lang dune 3.6)

(name parallel_seq)

(generate_opam_files true)

(source
 (github aytao/parallel_seq))

(authors "Andrew Tao")

(maintainers "Andrew Tao")

(license "LGPL-2.1-or-later")

(package
 (name parallel_seq)
 (synopsis
  "A shared-memory parallelism implementation of the parallel sequences abstraction.")
 (depends
  (ocaml
   (>= "5.0"))
  (domainslib
   (>= "0.5.0"))))

open Domainslib
open Parallelseq
open Sequence

let sequential_m = Sequence.get_module Sequential
let pool = Task.setup_pool ~num_domains:Defaults.num_domains ()

let parallel_m =
  Sequence.get_module (Parallel (pool, Defaults.sequential_cutoff))

module SequentialS = (val sequential_m : Sequence.S)
module ParallelS = (val parallel_m : Sequence.S)
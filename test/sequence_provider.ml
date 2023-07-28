open Domainslib
open Parallelseq
open Sequence

let sequential_m = Sequence.get_module Sequential
let pool = Task.setup_pool ~num_domains:(Defaults.num_domains_total - 1) ()

let parallel_m =
  Sequence.get_module
    (Parallel { pool; sequential_cutoff = Defaults.sequential_cutoff })

module Sequential = (val sequential_m : Sequence.S)
module Parallel = (val parallel_m : Sequence.S)

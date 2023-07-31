open Domainslib
open Parallel_seq

let sequential_m = get_module Sequential
let pool = Task.setup_pool ~num_domains:(Defaults.num_domains_total - 1) ()

let parallel_m =
  get_module (Parallel { pool; sequential_cutoff = Defaults.sequential_cutoff })

module Sequential = (val sequential_m : S)
module Parallel = (val parallel_m : S)

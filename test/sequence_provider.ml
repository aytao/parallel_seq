open Domainslib
open Seq
open Sequence

let sequential_m = Sequence.get_module Sequential
let pool = Task.setup_pool ~num_domains:Defaults.num_domains ()
let parallel_m = Sequence.get_module (Parallel pool)

module Sequential = (val sequential_m : Sequence.S)
module Parallel = (val parallel_m : Sequence.S)
module S = Parallel
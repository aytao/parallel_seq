open Seq
open Sequence

let sequential_m = Sequence.get_module Sequential
let parallel_m = Sequence.get_module (Parallel Defaults.num_domains)

module Sequential = (val sequential_m : Sequence.S)
module Parallel = (val parallel_m : Sequence.S)
module S = Parallel
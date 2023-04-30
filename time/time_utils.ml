open Domainslib
open Seq
open Sequence

let seq_type =
  if Defaults.force_sequential then Sequential
  else
    let pool = Task.setup_pool ~num_domains:Defaults.num_domains () in
    Parallel pool

let m = Sequence.get_module seq_type

module S = (val m : Sequence.S)

let time (f : 'a -> 'b) (x : 'a) : 'b * float =
  let t = Unix.gettimeofday () in
  let fx = f x in
  let t' = Unix.gettimeofday () in
  (fx, t' -. t)

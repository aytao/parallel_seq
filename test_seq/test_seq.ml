open Seq
open Sequence

let seq_type =
  if Defaults.force_sequential then Sequential
  else Parallel Defaults.num_domains

let m = Sequence.get_module seq_type

open (val m : Sequence.S)

let harmonic_sum (n : int) : float =
  let denominators = tabulate (fun x -> x + 1) n in
  let terms = map (fun x -> 1. /. float_of_int x) denominators in
  reduce ( +. ) 0. terms

let _ = Printf.printf "%f\n" (harmonic_sum 3)

let get_array (n : int) : int array =
  let singletons = tabulate (fun x -> [| x |]) n in
  reduce Array.append [||] singletons

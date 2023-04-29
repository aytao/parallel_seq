open Seq
open Sequence
open S

let harmonic_sum (n : int) : float =
  let denominators = tabulate (fun x -> x + 1) n in
  let terms = map (fun x -> 1. /. float_of_int x) denominators in
  reduce ( +. ) 0. terms

let _ = Printf.printf "%f\n" (harmonic_sum 3)

let get_array (n : int) : int array =
  let singletons = tabulate (fun x -> [| x |]) n in
  reduce Array.append [||] singletons

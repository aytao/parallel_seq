open Seq
open Sequence

let n = 100000

let slow_divide n _ =
  let i = Random.float 1. in
  let rec aux n acc num = if acc > n then num else aux n (acc +. i) (num + 1) in
  aux n 1. 0

(* Ensure that module evaluation isn't counted in time *)
let _ = S.empty ()
let s, elapsed_time = Time_utils.time (S.tabulate (slow_divide 1000.)) n;;

Printf.printf "%f\n" elapsed_time

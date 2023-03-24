open Seq
open Sequence

let n = 10000000;;

let s = S.tabulate (fun _ -> Random.float 1.) n

let slow_log n i =
  let rec aux n acc num =
    if acc > n then num else aux n (acc *. (i +. 1.)) (num + 1)
  in
  aux n 1. 0

let s, elapsed_time = Time_utils.time (S.map (slow_log (float_of_int n))) s;;

Printf.printf "%f\n" elapsed_time

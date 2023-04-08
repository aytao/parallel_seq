open Seq
open Sequence

let n = 100000
let s = S.tabulate (fun _ -> Random.float 1.) n

let slow_divide n i =
  let rec aux n acc num = if acc > n then num else aux n (acc +. i) (num + 1) in
  aux n 1. 0

let s, elapsed_time = Time_utils.time (S.map (slow_divide 1000.)) s;;

Printf.printf "%f\n" elapsed_time

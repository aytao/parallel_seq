open Seq
open Sequence
open Quicksort

let n = Defines.(num_domains * sequential_cutoff * 10);;

let s = S.tabulate (fun _ -> Random.float Float.max_float) n

let s, elapsed_time = Time_utils.time (quicksort Float.compare) s;;

Printf.printf "%f" elapsed_time

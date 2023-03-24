open Seq
open Sequence

let n = Defines.(num_domains * sequential_cutoff * 10);;

let s = S.tabulate (fun _ -> Random.float 1.) n

let f, elapsed_time = Time_utils.time (S.reduce (+.) 0.) s;;

Printf.printf "%f\n" elapsed_time

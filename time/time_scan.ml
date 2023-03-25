open Seq
open Sequence

let n = Defines.(10000000);;

let s = S.tabulate (fun _ -> Random.float 1.) n

let f, elapsed_time = Time_utils.time (S.scan (+.) 0.) s;;

Printf.printf "%f\n" elapsed_time

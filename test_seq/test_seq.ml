open Seq
open Sequence

let a = [|2; 1; 4; -2; 3; 4; -1; 2|]

let s = ParallelSeq.tabulate (fun i -> a.(i)) (Array.length a)

let prefix_sums = ParallelSeq.scan (+) 0 s

let _ = ParallelSeq.iter (fun i -> print_string ((string_of_int i)^",")) prefix_sums

let _ = print_newline
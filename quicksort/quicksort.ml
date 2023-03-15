open Seq
open Sequence
open Sort

let n = 10;;
let rand_ints = S.tabulate (fun i -> Random.full_int (n * 10)) n;;

let sorted = quicksort_ints rand_ints;;

let _ = Utils.print_sequence string_of_int sorted;;
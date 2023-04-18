open Seq
open Sequence
open Array_handler

let n = Sys.max_array_length
let memset_init n : int array = get_uninitialized n
let _, elapsed_time = Time_utils.time memset_init n;;

Printf.printf "%f\n" elapsed_time
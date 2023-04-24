open Seq
open Sequence
open Array_handler

let n = 1000000
let memset_init n : int array = get_uninitialized n
let total_time = ref 0.0;;

for i = 1 to 100 do
  let _, elapsed_time = Time_utils.time memset_init n in
  total_time := !total_time +. elapsed_time
done

let _ = Printf.printf "%f\n" !total_time

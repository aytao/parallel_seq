open Seq
open Array_handler

let n = 10_000_000
let a : Obj.t array = get_uninitialized n
let _ = print_endline "Running array handler test";;

for i = 0 to n - 1 do
  assert (not (Obj.is_block a.(i)))
done

open Parallelseq
open Array_handler

let n = 10_000_000
let a : Obj.t array = get_uninitialized n;;

for i = 0 to n - 1 do
  assert (not (Obj.is_block a.(i)))
done

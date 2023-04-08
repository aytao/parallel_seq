open Seq
open Sequence

type thing = Foo | Bar of int

let n : int = (Defines.sequential_cutoff * Defines.num_domains) + 1
let weak_arr = Weak.create n

let f i =
  let v = Bar i in
  Weak.set weak_arr i (Some v);
  v

let is_all_full wa =
  S.tabulate (fun i -> i) (Weak.length wa)
  |> S.map_reduce (Weak.check wa) ( && ) true

let _ = print_endline "Running GC harmony test"
let s = S.tabulate f n;;

assert (is_all_full weak_arr);;

Gc.full_major ();
assert (is_all_full weak_arr)

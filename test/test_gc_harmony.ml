open Seq
open Sequence

type thing = Foo | Bar of int
let n: int = (Defines.sequential_cutoff * Defines.num_domains) + 1;;

let weak_arr = Weak.create n;;

let f i =
  let v = Bar i in
  Weak.set weak_arr i (Some v);
  Gc.full_major ();
  v

let assert_all_full wa =
  for i = 0 to (Weak.length wa - 1) do
    assert (Weak.check wa i);
  done;
  ()

let s = S.tabulate f n;;
assert_all_full weak_arr;;
Gc.full_major ();
assert_all_full weak_arr;;

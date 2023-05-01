type thing = Foo | Bar of int

let f wa int_array i =
  let w = Bar i in
  Weak.set wa 0 (Some w);
  let na_repr = Obj.repr int_array in
  let bar_repr = Obj.repr w in
  Obj.set_field na_repr 0 bar_repr

let weak_arr : thing Weak.t = Weak.create 1
let int_arr : int array = [| 0 |]
let _ = f weak_arr int_arr 10;;

assert (Weak.check weak_arr 0);;
Gc.full_major ();;
assert (Weak.check weak_arr 0);;
int_arr.(0) <- 0;;
Gc.full_major ();;
assert (not (Weak.check weak_arr 0))

(* Make a boxed int option and place it in the weak array and int array *)
let f weak_a int_array i =
  let int_opt_array : int option array = Obj.magic int_array in
  let boxed_val = Some i in
  Weak.set weak_a 0 (Some boxed_val);
  int_opt_array.(0) <- boxed_val

let weak_arr : int option Weak.t = Weak.create 1
let int_arr : int array = Array.make 1 (-1)
let _ = f weak_arr int_arr 326;;

(* The boxed int optional will not be garbage collected, since the int
 * array contains a reference to it. *)
Gc.full_major ();;
assert (Weak.check weak_arr 0);;

(* Overwrite the int array's reference to the int optional. Now, no references
 * to it exist, so it should be garbage collected. *)
int_arr.(0) <- 0;;
Gc.full_major ();;
assert (not (Weak.check weak_arr 0))

open Seq
open Matrix

let n = 2000

module Float_Elt = struct
  type t = float

  let b = 0.
  let add = Float.add
  let mul = Float.mul
end

module FloatMatrix = CRSMatrix (Float_Elt)

let get_random_dok n =
  let rec aux map i =
    if i = 0 then map
    else
      let row, col = (Random.full_int n, Random.full_int n) in
      let map' = DictOfKeys.add (row, col) (Random.float Float.max_float) map in
      aux map' (i - 1)
  in
  let num =
    int_of_float
      (let n = float_of_int n in
       n *. sqrt n)
  in
  aux DictOfKeys.empty num

let mat = FloatMatrix.of_dok n n (get_random_dok n)
let vect = FloatMatrix.vect_of_array @@ Array.make n (1. /. float_of_int n)

let multiply mat vect n =
  let rec aux n vect =
    let _ = print_endline "Hi" in
    if n = 0 then vect else aux (n - 1) (FloatMatrix.vect_mul mat vect)
  in
  aux 10 vect

let v, elapsed_time = Time_utils.time (multiply mat vect) n;;

Printf.printf "%f\n" elapsed_time

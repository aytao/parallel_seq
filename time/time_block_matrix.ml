open Seq
open Matrix
open Time_utils

let m = 2000
let n = 2000
let p = 2000

module Float_Elt = struct
  type t = float

  let b = 0.
  let add = Float.add
  let mul = Float.mul
end

(* Accommodate sequential override *)
let fm = (module BlockMatrix (Float_Elt) (S) : MATRIX with type elt = float)

module FloatMatrix = (val fm : MATRIX with type elt = float)

let get_random_float_arr_arr m n =
  Array.init m (fun i -> Array.init n (fun _ -> Random.float 1.))

let mat1 = FloatMatrix.of_2d_arr (get_random_float_arr_arr m p)
let mat2 = FloatMatrix.of_2d_arr (get_random_float_arr_arr p n)
let multiply mat1 mat2 = FloatMatrix.matrix_mul mat1 mat2
let v, elapsed_time = time (multiply mat1) mat2;;

Printf.printf "%f\n" elapsed_time

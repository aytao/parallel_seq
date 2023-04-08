open Seq
open Matrix

let m = 1000
let n = 1000
let p = 1000

module Float_Elt = struct
  type t = float

  let b = 0.
  let add = Float.add
  let mul = Float.mul
end

module FloatMatrix = ArrayMatrix (Float_Elt)

let get_random_float_arr_arr m n =
  Array.init m (fun i -> Array.init n (fun _ -> Random.float 1.))

let mat1 = FloatMatrix.of_2d_arr (get_random_float_arr_arr m p)
let mat2 = FloatMatrix.of_2d_arr (get_random_float_arr_arr p n)
let multiply mat1 mat2 = FloatMatrix.matrix_mul mat1 mat2
let v, elapsed_time = Time_utils.time (multiply mat1) mat2;;

Printf.printf "%f\n" elapsed_time

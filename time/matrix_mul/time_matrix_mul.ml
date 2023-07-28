open Parallelseq
open Matrix

module Float_elt = struct
  type t = float

  let b = 0.
  let add = Float.add
  let mul = Float.mul
end

let get_random_float_arr_arr n =
  Array.init n (fun i -> Array.init n (fun _ -> Random.float 1.))

let block seq_type n =
  let n = Option.value n ~default:2000 in
  let open
    Block_matrix (Float_elt) ((val Parallelseq.Sequence.get_module seq_type)) in
  let mat1 = of_2d_arr (get_random_float_arr_arr n) in
  let mat2 = of_2d_arr (get_random_float_arr_arr n) in
  Time_test.time (fun () -> matrix_mul mat1 mat2 |> ignore) ()

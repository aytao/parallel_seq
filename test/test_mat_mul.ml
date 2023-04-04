open Seq
open Matrix
open Printexc

let m = 100
let n = 150
let p = 200

module Int_Elt = struct
  type t = int
  let b = 0
  let add = Int.add
  let mul = Int.mul
end

module MatrixMul(M: MATRIX) = struct
  let mul elts1 elts2 =
    let mat1 = M.of_2d_arr elts1 in
    let mat2 = M.of_2d_arr elts2 in
    let result = M.matrix_mul mat1 mat2 in
    let m, n = M.dimensions result in
    Array.init m (fun i ->
      Array.init n (fun j -> M.get i j result))
end
module BlockMul = MatrixMul(BlockMatrix(Int_Elt))
module SeqMul = MatrixMul(SeqMatrix(Int_Elt))
module ArrayMul = MatrixMul(ArrayMatrix(Int_Elt))

let get_random_int_arr_arr m n =
  Array.init m (fun i ->
    Array.init n (fun _ -> Random.full_int 256))

let test_mul () =
  let equal m1 m2 =
    Array.for_all2 (fun r1 r2 ->
      Array.for_all2 (=) r1 r2) m1 m2
  in
  let elts1 = get_random_int_arr_arr m n in
  let elts2 = get_random_int_arr_arr n p in
  let block_result = BlockMul.mul elts1 elts2 in
  let seq_result = SeqMul.mul elts1 elts2 in
  let array_result = ArrayMul.mul elts1 elts2 in
  assert (equal array_result seq_result);
  assert (equal array_result block_result)

let _ = record_backtrace true;;
let _ = print_endline "Running matrix mul test";;
let _ = try (test_mul ()) with | e -> print_endline (Printexc.to_string e); print_backtrace stderr

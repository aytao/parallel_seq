open Seq
open Matrix
open Printexc
open Sequence_provider

let m = 1000
let n = 1500
let p = 200

let get_random_int_arr_arr m n =
  Array.init m (fun i -> Array.init n (fun _ -> Random.int 256))

module Int_Elt = struct
  type t = int

  let b = 0
  let add = ( + )
  let mul = ( * )
end

module MatrixMul (M : MATRIX) = struct
  let mul elts1 elts2 =
    let mat1 = M.of_2d_arr elts1 in
    let mat2 = M.of_2d_arr elts2 in
    let result = M.matrix_mul mat1 mat2 in
    let m, n = M.dimensions result in
    Array.init m (fun i -> Array.init n (fun j -> M.get i j result))
end

module Test (S : Sequence.S) = struct
  module BlockMul = MatrixMul (BlockMatrix (Int_Elt) (S))
  module ArrayMul = MatrixMul (ArrayMatrix (Int_Elt))

  let test_mul () =
    let equal m1 m2 =
      Array.for_all2 (fun r1 r2 -> Array.for_all2 ( = ) r1 r2) m1 m2
    in
    let elts1 = get_random_int_arr_arr m n in
    let elts2 = get_random_int_arr_arr n p in
    let block_result = BlockMul.mul elts1 elts2 in
    let array_result = ArrayMul.mul elts1 elts2 in
    let _ = assert (equal array_result block_result) in
    ()

  let run_tests () = test_mul ()
end

module ParallelTester = Test (ParallelS)
module SequentialTester = Test (SequentialS)

let _ = ParallelTester.run_tests ()
let _ = SequentialTester.run_tests ()

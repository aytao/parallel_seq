open Parallelseq

module type Matrix_elt = sig
  type t

  val b : t
  val add : t -> t -> t
  val mul : t -> t -> t
end

module type Matrix = sig
  type elt
  type vect
  type matrix

  val of_2d_arr : elt array array -> matrix
  val get : int -> int -> matrix -> elt
  val dimensions : matrix -> int * int
  val transpose : matrix -> matrix
  val vect_of_array : elt array -> vect
  val vect_mul : matrix -> vect -> vect
  val matrix_mul : matrix -> matrix -> matrix
end

module Array_matrix (E : Matrix_elt) : Matrix with type elt = E.t

module Block_matrix (E : Matrix_elt) (S : Sequence.S) :
  Matrix with type elt = E.t

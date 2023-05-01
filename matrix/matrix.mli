open Seq

module type MatrixElt = sig
  type t

  val b : t
  val add : t -> t -> t
  val mul : t -> t -> t
end

module type MATRIX = sig
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

module ArrayMatrix (E : MatrixElt) : MATRIX with type elt = E.t
module BlockMatrix (E : MatrixElt) (S : Sequence.S) : MATRIX with type elt = E.t

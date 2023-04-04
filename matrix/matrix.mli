open Seq
open Sequence

module DictOfKeys : Map.S with type key = (int * int)

module type MatrixElt =
sig
  type t
  val b : t
  val add : t -> t -> t
  val mul : t -> t -> t
end

module type MATRIX =
sig
  type elt
  type vect
  type matrix
  val of_dok : int -> int -> elt DictOfKeys.t -> matrix
  val of_elt_arr : ((int * int) * elt) array -> int -> int -> matrix
  val of_2d_arr : elt array array -> matrix
  val get : int -> int -> matrix -> elt
  val dimensions : matrix -> int * int
  val transpose: matrix -> matrix

  val vect_of_array : elt array -> vect
  val vect_mul : matrix -> vect -> vect
  val matrix_mul : matrix -> matrix -> matrix
end

module ArrayMatrix(E : MatrixElt) : MATRIX with type elt = E.t
module SeqMatrix(E : MatrixElt) : MATRIX with type elt = E.t
module CRSMatrix(E : MatrixElt) : MATRIX with type elt = E.t
module BlockMatrix(E : MatrixElt) : MATRIX with type elt = E.t

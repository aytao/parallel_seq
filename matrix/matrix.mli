module MatrixMap : Map.S with type key = (int * int)

module type MatrixElt =
sig
  type t
  val add : t -> t -> t
  val mult : t -> t -> t
end

module type MATRIX =
sig
  type elt
  type vect
  type matrix
  (* TODO: Correct of dict name *)
  val of_map : int -> int -> elt -> elt MatrixMap.t -> matrix
  val get : int -> int -> matrix -> elt
  val dimensions : matrix -> int * int
  val vect_mult : matrix -> vect -> vect
  val matrix_mult : matrix -> matrix -> matrix
end
open Seq

module IntTuple : Map.OrderedType with type t = (int * int) =
struct
  type t = (int * int)

  let compare (row1, col1 : t) (row2, col2 : t) = 
    let dx = row1 - row2 in
    if dx = 0 then col1 - col2 else dx
end

module type MatrixElt =
sig
  type t
  val b : t
  val add : t -> t -> t
  val mult : t -> t -> t
end

module DictOfKeys = Map.Make(IntTuple)

module type MATRIX =
sig
  type elt
  type vect
  type matrix
  val of_dok : int -> int -> elt -> elt DictOfKeys.t -> matrix
  val get : int -> int -> matrix -> elt
  val dimensions : matrix -> int * int
  val transpose: matrix -> matrix
  val vect_mult : matrix -> vect -> vect
  val matrix_mult : matrix -> matrix -> matrix
end

let check_index row col m n s =
  if row < 0 || row >= m || col < 0 || col >= n then
    raise (Invalid_argument s)

let check_size_nonzero m n s =
  if m <= 0 || n <= 0 then raise (Invalid_argument s)

module ArrayMatrix(E: MatrixElt) : (MATRIX with type elt = E.t) = struct
  type elt = E.t
  type vect = elt array
  type matrix = elt array array
  let b = E.b
  
  let of_dok m n b map =
    let _ = check_size_nonzero m n "ArrayMatrix.of_map" in
    let mat = Array.init m (fun i -> Array.make n b) in
    let update_matrix k v =
      let row, col = k in
      let _ = check_index row col m n "ArrayMatrix.of_map" in
      mat.(row).(col) <- v
    in
    DictOfKeys.iter update_matrix map;
    mat

  let get row col mat =
    let _ = check_index row col (Array.length mat) (Array.length mat.(0)) "ArrayMatrix.get" in
    mat.(row).(col)
  
  let dimensions mat = Array.length mat, Array.length mat.(0)

  let dot v1 v2 b =
    assert (Array.length v1 = Array.length v2);
    let acc = ref b in
    for i = 0 to (Array.length v1 - 1) do
      acc := E.add !acc (E.mult v1.(i) v2.(i))
    done;
    !acc

  let transpose mat =
    Array.init (Array.length mat.(0)) (fun i -> Array.init (Array.length mat) (fun j -> mat.(j).(i)))
  
  let vect_mult mat vect =
    Array.init (Array.length mat) (fun i -> dot mat.(i) vect b)
  
  let matrix_mult mat1 mat2 =
    let m1, n1 = dimensions mat1 in
    let m2, n2 = dimensions mat2 in
    if n1 != m2 then raise (Invalid_argument "ArrayMatrix.matrix_mult") else
    let mat2T = transpose mat2 in
    let resultT = Array.init n2 (fun i -> vect_mult mat1 mat2T.(i)) in
    transpose resultT
end
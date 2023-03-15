open Seq

module IntTuple : Map.OrderedType with type t = (int * int) =
struct
  type t = (int * int)

  let compare (row1, col1 : t) (row2, col2 : t) = 
    let dx = row1 - row2 in
    if dx = 0 then col1 - col2 else dx
  
  let get_coords (row, col : t) =
    row, col
end

module type MatrixElt =
sig
  type t
  val add : t -> t -> t
  val mult : t -> t -> t
end

module MatrixMap = Map.Make(IntTuple)

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

let check_index row col m n s =
  if row < 0 || row >= m || col < 0 || col >= n then
    raise (Invalid_argument s)

module ArrayMatrix(E: MatrixElt) : (MATRIX with type elt = E.t) = struct
  type elt = E.t
  type vect = elt array
  type matrix = elt * elt array array
  
  let of_map m n b map =
    if m <= 0 || n <= 0 then raise (Invalid_argument "ArrayMatrix.of_map") else
    let mat = Array.init m (fun i -> Array.make n b) in
    let update_matrix k v =
      let row, col = k in
      let _ = check_index row col m n "ArrayMatrix.of_map" in
      mat.(row).(col) <- v
    in
    MatrixMap.iter update_matrix map;
    b, mat

  let get row col (_, mat) =
    let _ = check_index row col (Array.length mat) (Array.length mat.(0)) "ArrayMatrix.get" in
    mat.(row).(col)
  
  let dimensions (_, mat) = Array.length mat, Array.length mat.(0)

  let dot v1 v2 b =
    assert (Array.length v1 = Array.length v2);
    let acc = ref b in
    for i = 0 to (Array.length v1 - 1) do
      acc := E.add !acc (E.mult v1.(i) v2.(i))
    done;
    !acc
  
  let vect_mult (b, mat) vect =
    Array.init (Array.length mat) (fun i -> dot mat.(i) vect b)
  
  let matrix_mult matrix1 matrix2 =
    let m1, n1 = dimensions matrix1 in
    let m2, n2 = dimensions matrix2 in
    if n1 != m2 then raise (Invalid_argument "ArrayMatrix.matrix_mult") else
    let result = Array.init m1 (fun i -> Array_handler.get_uninitialized n2) in
    failwith "Unimplemented"
end
open Seq
open Sequence
open Quicksort

module IntTuple : Map.OrderedType with type t = (int * int) =
struct
  type t = (int * int)

  let compare (row1, col1 : t) (row2, col2 : t) = 
    let cmp_r = compare row1 row2 in
    if cmp_r = 0 then compare col1 col2 else cmp_r
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
  val of_dok : int -> int -> elt DictOfKeys.t -> matrix
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
  
  let of_dok m n map =
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

  let dot v1 v2 =
    assert (Array.length v1 = Array.length v2);
    let acc = ref b in
    for i = 0 to (Array.length v1 - 1) do
      acc := E.add !acc (E.mult v1.(i) v2.(i))
    done;
    !acc

  let transpose mat =
    Array.init (Array.length mat.(0)) (fun i -> Array.init (Array.length mat) (fun j -> mat.(j).(i)))
  
  let vect_mult mat vect =
    let m, n = dimensions mat in
    let len = Array.length vect in
    if n != len then raise (Invalid_argument "ArrayMatrix.vect_mult") else
      (* TODO: Fix redundant length *)
    Array.init (Array.length mat) (fun i -> dot mat.(i) vect)
  
  let matrix_mult mat1 mat2 =
    let m1, n1 = dimensions mat1 in
    let m2, n2 = dimensions mat2 in
    if n1 != m2 then raise (Invalid_argument "ArrayMatrix.matrix_mult") else
    let mat2T = transpose mat2 in
    let resultT = Array.init n2 (fun i -> vect_mult mat1 mat2T.(i)) in
    transpose resultT
end

module SeqMatrix(E: MatrixElt)(S: S) : (MATRIX with type elt = E.t) = struct
  type elt = E.t
  type vect = elt S.t
  type matrix = elt S.t S.t
  let b = E.b
  
  let of_dok m n map =
    let _ = check_size_nonzero m n "SeqMatrix.of_map" in
    S.tabulate (fun i -> 
      S.tabulate (fun j ->
        if DictOfKeys.mem (i, j) map then DictOfKeys.find (i, j) map else b
      ) n  
    ) m

  let get row col mat =
    let _ = check_index row col (S.length mat) (S.length (S.nth mat 0)) "SeqMatrix.get" in
    S.nth (S.nth mat row) col
  
  let dimensions mat = 
    S.length mat, S.length (S.nth mat 0)

  let dot v1 v2 =
    assert (S.length v1 = S.length v2);
    S.tabulate (fun i -> E.mult (S.nth v1 i) (S.nth v2 i)) (S.length v1)
    |> S.reduce E.add b

  let transpose mat =
    S.tabulate (fun i ->
      S.tabulate (fun j ->
        S.nth (S.nth mat j) i
      ) (S.length mat)
    ) (S.length (S.nth mat 0)) 
  
  let vect_mult mat vect =
    (* TODO: Check dim *)
    S.tabulate (fun i -> dot (S.nth mat i) vect) (S.length mat)
  
  let matrix_mult mat1 mat2 =   
    let m1, n1 = dimensions mat1 in
    let m2, n2 = dimensions mat2 in
    if n1 != m2 then raise (Invalid_argument "SeqMatrix.matrix_mult") else
    let mat2T = transpose mat2 in
    let resultT = S.tabulate (fun i -> vect_mult mat1 (S.nth mat2T i)) n2 in
    transpose resultT
end

module CRSMatrix(E: MatrixElt)(S: S) : (MATRIX with type elt = E.t) = struct
  type elt = E.t
  type vect = elt S.t
  type matrix = {elts: elt S.t; cols: int S.t; row_ptrs: int S.t; m: int; n: int}
  let b = E.b
  
  module Sort = Quicksort(S)
  let sort = Sort.quicksort

  let bin_search s i lo hi =
    let rec helper lo hi =
      if hi <= lo then -1
      else
        let mid = lo + (hi - lo / 2) in
        let v = S.nth s mid in
        if v < i then helper (mid + 1) hi
        else if v > i then helper lo mid
        else mid
    in
    helper lo hi

  let of_elt_list (l: ((int * int) * elt) S.t) m n =
    let cmp ((row1, col1), _) ((row2, col2), _) =
      let cmp_r = Int.compare row1 row2 in
      if cmp_r = 0 then Int.compare col1 col2 else cmp_r
    in
    let l = sort cmp l in
    let elts = S.map (fun ((_, _), e) -> e) l in
    let cols = S.map (fun ((_, c), _) -> c) l in

    let inject ((r, _), _) = S.tabulate (fun i -> if i = r then 1 else 0) m in
    let combine counts1 counts2 =
      S.tabulate (fun i -> (S.nth counts1 i + S.nth counts2 i)) m
    in
    let zeros = S.tabulate (fun i -> 0) m in
    let row_ptrs = S.map_reduce inject combine zeros l in
    {elts; cols; row_ptrs; m; n}

  let of_dok m n map =
    let _ = check_size_nonzero m n "CRSMatrix.of_map" in
    let s = S.seq_of_array @@ Array.of_list @@ DictOfKeys.bindings map in
    of_elt_list s m n

  let get row col {elts; cols; row_ptrs; m; n} =
    let _ = check_index row col m n "CRSMatrix.get" in
    let row_start, row_end = S.nth row_ptrs row, S.nth row_ptrs (row + 1) in
    let row_idx = bin_search cols col row_start row_end in
    if row_idx < 0 then b else S.nth elts (row_start + row_idx)
  
  let dimensions mat = 
    mat.m, mat.n

  let transpose {elts; cols; row_ptrs; m; n} =
    let repeat_row row =
      let row_size = S.nth row_ptrs (row + 1) - S.nth row_ptrs row in
      S.tabulate (fun _ -> row) row_size
    in
    let rows = S.flatten (S.tabulate repeat_row m) in
    let zip i =
      (S.nth cols i, S.nth rows i), S.nth elts i
    in
    let list = S.tabulate zip (S.length elts) in
    of_elt_list list n m
    
  let vect_mult {elts; cols; row_ptrs; m; n} vect =
    let len = S.length vect in
    if n != len then raise (Invalid_argument "ArrayMatrix.vect_mult") else
    let row_body row =
      let row_start, row_end = S.nth row_ptrs row, S.nth row_ptrs (row + 1) in
      S.tabulate (fun i ->
        let idx = i + row_start in
        S.nth cols idx, S.nth elts idx
      ) (row_end - row_start)
    in
    let row_tuple_seqs = S.tabulate row_body m in
    let row_dot row_seq =
      S.map_reduce (fun (c, e) -> E.mult e (S.nth vect c)) E.add b row_seq
    in
    S.map row_dot row_tuple_seqs
  
  let matrix_mult mat1 mat2 = 
    failwith "Unimplemented"
end
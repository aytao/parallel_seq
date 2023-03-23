open Seq
open Sequence
open Quicksort

let compare_int_tuple (row1, col1 : int * int) (row2, col2 : int * int) = 
  let cmp_r = compare row1 row2 in
  if cmp_r = 0 then compare col1 col2 else cmp_r

module IntTuple : Map.OrderedType with type t = (int * int) =
struct
  type t = (int * int)

  let compare = compare_int_tuple
end

module type MatrixElt =
sig
  type t
  val b : t
  val add : t -> t -> t
  val mul : t -> t -> t
end

module DictOfKeys = Map.Make(IntTuple)

module type MATRIX =
sig
  type elt
  type vect
  type matrix
  val of_dok : int -> int -> elt DictOfKeys.t -> matrix
  val of_elt_arr : ((int * int) * elt) array -> int -> int -> matrix
  val get : int -> int -> matrix -> elt
  val dimensions : matrix -> int * int
  val transpose: matrix -> matrix

  val vect_of_array : elt array -> vect
  val vect_mul : matrix -> vect -> vect
  (* val matrix_mul : matrix -> matrix -> matrix *)
end

let check_index row col m n s =
  if row < 0 || row >= m || col < 0 || col >= n then
    raise (Invalid_argument s)

let check_size_nonzero m n s =
  if m <= 0 || n <= 0 then raise (Invalid_argument s)

let bin_search nth s compare i lo hi =
  let rec helper lo hi =
    if hi <= lo then -1
    else
      let mid = lo + (hi - lo / 2) in
      let v = nth s mid in
      let cmp = compare v i in
      if cmp < 0 then helper (mid + 1) hi
      else if cmp > 0 then helper lo mid
      else mid
  in
  helper lo hi

let sort_elt_seq sort elt_seq =
  let cmp (tup1, _) (tup2, _) =
    compare_int_tuple tup1 tup2
  in
  sort cmp elt_seq

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
  
  let of_elt_arr (elt_arr: ((int * int) * elt) array) m n =
    let _ = check_size_nonzero m n "ArrayMatrix.of_map" in
    let mat = Array.init m (fun i -> Array.make n b) in
    let update_matrix elt =
      let (row, col), v = elt in
      let _ = check_index row col m n "ArrayMatrix.of_map" in
      mat.(row).(col) <- v
    in
    Array.iter update_matrix elt_arr;
    mat

  let get row col mat =
    let _ = check_index row col (Array.length mat) (Array.length mat.(0)) "ArrayMatrix.get" in
    mat.(row).(col)
  
  let dimensions mat = Array.length mat, Array.length mat.(0)

  let dot v1 v2 =
    assert (Array.length v1 = Array.length v2);
    let acc = ref b in
    for i = 0 to (Array.length v1 - 1) do
      acc := E.add !acc (E.mul v1.(i) v2.(i))
    done;
    !acc

  let transpose mat =
    Array.init (Array.length mat.(0)) (fun i -> Array.init (Array.length mat) (fun j -> mat.(j).(i)))

  let vect_of_array arr =
    Array.copy arr
  
  let vect_mul mat vect =
    let m, n = dimensions mat in
    let len = Array.length vect in
    if n != len then raise (Invalid_argument "ArrayMatrix.vect_mul") else
      (* TODO: Fix redundant length *)
    Array.init (Array.length mat) (fun i -> dot mat.(i) vect)
  
  let matrix_mul mat1 mat2 =
    let m1, n1 = dimensions mat1 in
    let m2, n2 = dimensions mat2 in
    if n1 != m2 then raise (Invalid_argument "ArrayMatrix.matrix_mul") else
    let mat2T = transpose mat2 in
    let resultT = Array.init n2 (fun i -> vect_mul mat1 mat2T.(i)) in
    transpose resultT
end

module SeqMatrix(E: MatrixElt) : (MATRIX with type elt = E.t) = struct
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

  let of_elt_arr (elt_arr: ((int * int) * elt) array) m n =
    let elt_seq = sort_elt_seq quicksort (S.seq_of_array elt_arr) in
    let get_val row col =
      let cmp (tup1, _) (tup2, _) =
        compare_int_tuple tup1 tup2
      in
      let tup = ((row, col), b) in
      let idx = bin_search S.nth elt_seq cmp tup 0 (S.length elt_seq) in
      if idx < 0 then b
      else
        let (_, elt) = S.nth elt_seq idx in
        elt
    in
    S.tabulate (fun i -> 
      S.tabulate (fun j ->
        get_val i j
      ) n  
    ) m

  let get row col mat =
    let _ = check_index row col (S.length mat) (S.length (S.nth mat 0)) "SeqMatrix.get" in
    S.nth (S.nth mat row) col
  
  let dimensions mat = 
    S.length mat, S.length (S.nth mat 0)

  let dot v1 v2 =
    assert (S.length v1 = S.length v2);
    S.tabulate (fun i -> E.mul (S.nth v1 i) (S.nth v2 i)) (S.length v1)
    |> S.reduce E.add b

  let transpose mat =
    S.tabulate (fun i ->
      S.tabulate (fun j ->
        S.nth (S.nth mat j) i
      ) (S.length mat)
    ) (S.length (S.nth mat 0)) 
  
  let vect_of_array = S.seq_of_array
  
  let vect_mul mat vect =
    (* TODO: Check dim *)
    S.tabulate (fun i -> dot (S.nth mat i) vect) (S.length mat)
  
  let matrix_mul mat1 mat2 =   
    let m1, n1 = dimensions mat1 in
    let m2, n2 = dimensions mat2 in
    if n1 != m2 then raise (Invalid_argument "SeqMatrix.matrix_mul") else
    let mat2T = transpose mat2 in
    let resultT = S.tabulate (fun i -> vect_mul mat1 (S.nth mat2T i)) n2 in
    transpose resultT
end

module CRSMatrix(E: MatrixElt) : (MATRIX with type elt = E.t) = struct
  type elt = E.t
  type vect = elt S.t
  type matrix = {elts: elt S.t; cols: int S.t; row_ptrs: int S.t; m: int; n: int}
  let b = E.b

  let of_elt_seq (s: ((int * int) * elt) S.t) m n =
    let s = sort_elt_seq quicksort s in
    let elts = S.map (fun ((_, _), e) -> e) s in
    let cols = S.map (fun ((_, c), _) -> c) s in

    let inject ((r, _), _) = S.tabulate (fun i -> if i = r then 1 else 0) m in
    let combine counts1 counts2 =
      S.tabulate (fun i -> (S.nth counts1 i + S.nth counts2 i)) m
    in
    let zeros = S.tabulate (fun i -> 0) m in
    let row_counts = S.map_reduce inject combine zeros s in
    let row_ptrs = S.cons 0 (S.scan (+) 0 row_counts) in
    {elts; cols; row_ptrs; m; n}
  
  let of_elt_arr (elt_arr: ((int * int) * elt) array) m n =
    of_elt_seq (S.seq_of_array elt_arr) m n

  let of_dok m n map =
    let _ = check_size_nonzero m n "CRSMatrix.of_map" in
    let s = S.seq_of_array @@ Array.of_list @@ DictOfKeys.bindings map in
    of_elt_seq s m n

  let get row col {elts; cols; row_ptrs; m; n} =
    let _ = check_index row col m n "CRSMatrix.get" in
    let row_start, row_end = S.nth row_ptrs row, S.nth row_ptrs (row + 1) in
    let row_idx = bin_search S.nth cols Int.compare col row_start row_end in
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
    of_elt_seq list n m
    
  let vect_of_array = S.seq_of_array

  let vect_mul {elts; cols; row_ptrs; m; n} vect =
    let len = S.length vect in
    if n != len then raise (Invalid_argument "CRSMatrix.vect_mul") else
    let row_body row =
      let row_start, row_end = S.nth row_ptrs row, S.nth row_ptrs (row + 1) in
      S.tabulate (fun i ->
        let idx = i + row_start in
        S.nth cols idx, S.nth elts idx
      ) (row_end - row_start)
    in
    let row_tuple_seqs = S.tabulate row_body m in
    let row_dot row_seq =
      S.map_reduce (fun (c, e) -> E.mul e (S.nth vect c)) E.add b row_seq
    in
    S.map row_dot row_tuple_seqs
  
  let matrix_mul mat1 mat2 = 
    failwith "Unimplemented"
end
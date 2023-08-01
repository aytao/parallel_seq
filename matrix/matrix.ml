open Parallel_seq
include Matrix_intf

let check_index row col m n s =
  if row < 0 || row >= m || col < 0 || col >= n then raise (Invalid_argument s)

let check_size_legal m n s = if m <= 0 || n <= 0 then raise (Invalid_argument s)

module Array_matrix (E : Matrix_elt) : Matrix with type elt = E.t = struct
  type elt = E.t
  type vect = elt array
  type matrix = elt array array

  let b = E.b

  let of_2d_arr (eaa : elt array array) =
    (* Check legal lengths, ensure all rows same length *)
    let m = Array.length eaa in
    let exn = Invalid_argument "Array_matrix.of_2d_arr" in
    if m <= 0 then raise exn
    else
      let len = Array.length eaa.(0) in
      let _ =
        Array.iter (fun arr -> if Array.length arr <> len then raise exn) eaa
      in
      (* init *)
      Array.init (Array.length eaa) (fun i -> Array.copy eaa.(i))

  let get row col mat =
    let _ =
      check_index row col (Array.length mat)
        (Array.length mat.(0))
        "Array_matrix.get"
    in
    mat.(row).(col)

  let dimensions mat = (Array.length mat, Array.length mat.(0))

  let dot v1 v2 =
    assert (Array.length v1 = Array.length v2);
    let acc = ref b in
    for i = 0 to Array.length v1 - 1 do
      acc := E.add !acc (E.mul v1.(i) v2.(i))
    done;
    !acc

  let transpose mat =
    Array.init
      (Array.length mat.(0))
      (fun i -> Array.init (Array.length mat) (fun j -> mat.(j).(i)))

  let vect_of_array = Array.copy

  let vect_mul mat vect =
    let m, n = dimensions mat in
    let len = Array.length vect in
    if n <> len then raise (Invalid_argument "Array_matrix.vect_mul")
    else Array.init m (fun i -> dot mat.(i) vect)

  let matrix_mul mat1 mat2 =
    let m, p = dimensions mat1 in
    let p', n = dimensions mat2 in
    if p <> p' then raise (Invalid_argument "Array_matrix.matrix_mul")
    else
      let body i j =
        let m1_i = mat1.(i) in
        let acc = ref b in
        for k = 0 to p - 1 do
          acc := E.add !acc (E.mul m1_i.(k) mat2.(k).(j))
        done;
        !acc
      in
      Array.init m (fun i -> Array.init n (fun j -> body i j))
end

module Block_matrix (E : Matrix_elt) (S : S) :
  Matrix with type elt = E.t and type vect = E.t S.t = struct
  type elt = E.t
  type vect = elt S.t
  type matrix = elt S.t S.t

  type submatrix = {
    elts : matrix;
    row_start : int;
    col_start : int;
    m : int;
    n : int;
  }

  type submat_addend = Full of elt array array | Empty

  let b = E.b

  type row_len = Base | Same of int | Diff

  let combine_row_len al1 al2 =
    match (al1, al2) with
    | Diff, _ | _, Diff -> Diff
    | Base, _ -> al2
    | _, Base -> al1
    | Same i, Same j -> if i = j then Same i else Diff

  let of_2d_arr (eaa : elt array array) =
    (* Check legal lengths, ensure all rows same length *)
    let m = Array.length eaa in
    let n =
      S.tabulate (fun i -> Same (Array.length eaa.(i))) m
      |> S.reduce combine_row_len Base
      |> function
      | Base | Diff -> 0
      | Same i -> i
    in
    let _ = check_size_legal m n "Block_matrix.of_2d_arr" in
    (* init *)
    S.tabulate
      (fun i -> S.tabulate (fun j -> eaa.(i).(j)) (Array.length eaa.(i)))
      (Array.length eaa)

  let get row col mat =
    let _ =
      check_index row col (S.length mat)
        (S.length (S.nth mat 0))
        "Block_matrix.get"
    in
    S.nth (S.nth mat row) col

  let dimensions mat = (S.length mat, S.length (S.nth mat 0))

  let dot v1 v2 mul add b =
    assert (S.length v1 = S.length v2);
    S.tabulate (fun i -> mul (S.nth v1 i) (S.nth v2 i)) (S.length v1)
    |> S.reduce add b

  let transpose mat =
    S.tabulate
      (fun i -> S.tabulate (fun j -> S.nth (S.nth mat j) i) (S.length mat))
      (S.length (S.nth mat 0))

  let vect_of_array = S.seq_of_array

  let vect_mul mat vect =
    let _m, n = dimensions mat in
    let len = S.length vect in
    if n <> len then raise (Invalid_argument "Block_matrix.vect_mul")
    else
      S.tabulate (fun i -> dot (S.nth mat i) vect E.mul E.add b) (S.length mat)

  let submatrix_get row col { elts; row_start; col_start; m = _; n = _ } =
    (* let _ = check_index row col m n "Block_matrix.submatrix_get" in *)
    S.nth (S.nth elts (row_start + row)) (col_start + col)

  let submatrix_mul sm1 sm2 =
    let m, p, n = (sm1.m, sm1.n, sm2.n) in
    (* if p <> p' then raise (Invalid_argument "Block_matrix.submatrix_mul") else *)
    let body r c =
      let acc = ref b in
      for i = 0 to p - 1 do
        acc :=
          E.add !acc (E.mul (submatrix_get r i sm1) (submatrix_get i c sm2))
      done;
      !acc
    in
    let elts' = Array.init m (fun i -> Array.init n (body i)) in
    Full elts'

  let eaa_add eaa1 eaa2 =
    let dim eaa = (Array.length eaa, Array.length eaa.(0)) in
    (* assert (dim eaa1 = dim eaa2); *)
    let m, n = dim eaa1 in
    Array.init m (fun i ->
        Array.init n (fun j -> E.add eaa1.(i).(j) eaa2.(i).(j)))

  let submat_add sa1 sa2 =
    match (sa1, sa2) with
    | Empty, _ -> sa2
    | _, Empty -> sa1
    | Full elts1, Full elts2 -> Full (eaa_add elts1 elts2)

  let matrix_mul mat1 mat2 =
    let m, p = dimensions mat1 in
    let p', n = dimensions mat2 in
    if p <> p' then raise (Invalid_argument "Block_matrix.matrix_mul")
    else
      let ceil_div num den = (num + den - 1) / den in
      let submat_size = Defaults.sequential_cutoff in
      let num_secs i = ceil_div i submat_size in
      let m_secs = num_secs m in
      let p_secs = num_secs p in
      let n_secs = num_secs n in
      let blocks_of_mat mat num_rows num_cols =
        S.tabulate
          (fun i ->
            S.tabulate
              (fun j ->
                let m, n = dimensions mat in
                let m' = min (m - (i * submat_size)) submat_size in
                let n' = min (n - (j * submat_size)) submat_size in
                {
                  elts = mat;
                  row_start = i * submat_size;
                  col_start = j * submat_size;
                  m = m';
                  n = n';
                })
              num_cols)
          num_rows
      in
      let bm1 = blocks_of_mat mat1 m_secs p_secs in
      let bm2 = blocks_of_mat mat2 p_secs n_secs in
      let body i j =
        let bm1_i = S.nth bm1 i in
        S.tabulate
          (fun k -> submatrix_mul (S.nth bm1_i k) (S.nth (S.nth bm2 k) j))
          p_secs
        |> S.reduce submat_add Empty
      in
      let bres = S.tabulate (fun i -> S.tabulate (body i) n_secs) m_secs in
      S.tabulate
        (fun i ->
          S.tabulate
            (fun j ->
              let row_num, col_num = (i / submat_size, j / submat_size) in
              let row_off, col_off = (i mod submat_size, j mod submat_size) in
              let block = S.nth (S.nth bres row_num) col_num in
              match block with
              | Full block -> block.(row_off).(col_off)
              | Empty -> failwith "Impossible")
            n)
        m
end

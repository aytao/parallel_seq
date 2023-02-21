open Domainslib

module type S = sig
  type 'a t
  val tabulate : (int -> 'a) -> int -> 'a t
  val seq_of_array : 'a array -> 'a t
  val array_of_seq : 'a t -> 'a array
  val iter: ('a -> unit) -> 'a t -> unit
  val length : 'a t -> int
  val empty : unit  ->'a t
  val cons : 'a -> 'a t -> 'a t
  val singleton : 'a -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val nth : 'a t -> int -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map_reduce : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'b -> 'a t -> 'b
  val reduce : ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a
  val flatten : 'a t t -> 'a t
  val repeat : 'a -> int -> 'a t
  val zip : ('a t * 'b t) -> ('a * 'b) t
  val split : 'a t -> int -> 'a t * 'a t
  val scan: ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a t

  val build_fenwick_tree: ('a -> 'a -> 'a) -> 'a -> int -> 'a t -> ('a array * int)
  val reduce_alt : ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a
  val scan_alt: ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a t
end

module ParallelSeq : S = struct

  type 'a t = 'a Array.t
  
  (* TODO: Address temp solution for domain count and pooling *)
  let pool = Task.setup_pool ~num_domains:(Defines.num_domains - 1) ()
  
  (* TODO: Address nested run(?) *)
  let run (task: 'a Task.task): 'a =
    Task.run pool task
  
  let parallel_for (n: int) (body: (int -> unit)): unit =
    run (fun _ ->
      Task.parallel_for ~start:0 ~finish:(n - 1) ~body:body pool  
    )

  let empty (): 'a t =
    [||]

  let tabulate (f: int -> 'a) (n: int): 'a t =
    (* TODO: Address array initialization *)
    if n = 0 then
      empty ()
    else if n < 0 then
      raise (Invalid_argument "Cannot make sequence of negative length")
    else (
      let arr: 'a array = Array_handler.get_uninitialized n in
      parallel_for n (fun i -> arr.(i) <- f i);
      arr
    )

  let seq_of_array (arr: 'a array): 'a t =
    tabulate (fun i -> arr.(i)) (Array.length arr)

  let array_of_seq (s: 'a t): 'a array =
    tabulate (fun i -> s.(i)) (Array.length s)

  let clone (s: 'a t): 'a t =
    tabulate (fun i -> s.(i)) (Array.length s)

  let iter (f: 'a -> unit) (s: 'a t): unit =
    Array.iter f s

  let length (s: 'a t): int =
    Array.length s

  let cons (x: 'a) (s: 'a t): 'a t =
    let len = length s in
    tabulate (fun i -> if i = 0 then x else s.(i + 1)) (len + 1)

  let singleton (x: 'a): 'a t =
    Array.make 1 x

  let append (s1: 'a t) (s2: 'a t): 'a t =
    let len1, len2 = length s1, length s2 in
    let body (idx: int): 'a =
      if idx < len1 then s1.(idx) else s2.(idx - len1)
    in
    tabulate body (len1 + len2)

  let nth (s: 'a t) (n: int): 'a =
    s.(n)

  let repeat (x: 'a) (n: int): 'a t =
    tabulate (fun _ -> x) n

  let zip ((s1, s2): 'a t * 'b t): ('a * 'b) t =
    let len1, len2 = length s1, length s2 in
    if len1 != len2 then
      raise (Invalid_argument "Sequences are different lengths")
    else
      tabulate (fun i -> (s1.(i), s2.(i))) len1

  let split (s: 'a t) (i: int): 'a t * 'a t =
    let len = length s in
    if i < 0 || i > len then
      raise (Invalid_argument "i is outside of bounds")
    else
      let l = Array_handler.get_uninitialized i in
      let r = Array_handler.get_uninitialized (len - i) in
      let body idx =
        if idx < i then l.(idx) <- s.(idx)
        else r.(idx - i) <- s.(idx)
      in
      parallel_for len body;
      (l, r)
  
  let even_elts (s: 'a t): 'a t =
    let num_elts = (length s + 1) / 2 in
    tabulate (fun i -> s.(i * 2)) num_elts
  
  let odd_elts (s: 'a t): 'a t =
    let num_elts = length s / 2 in
    tabulate (fun i -> s.(i * 2 + 1)) num_elts

  (* Applies f evens.(i) odds.(i) for all valid indices in odd. If even is
     one item longer, the last element of the array returned in f evens.(i) b*)
  let combine (f: 'a -> 'a -> 'a) (b: 'a) (evens: 'a t) (odds: 'a t): 'a t =
    let len = length evens in
    let uneven = (len != length odds) in
    let body (idx: int): 'a =
      let e = nth evens idx in
      let o = (if uneven && idx = len - 1 then b else nth odds idx) in
      f e o
    in
    tabulate body len

  (* Returns a calculated k-way fenwick tree, as well as the size of the greatest subtree in the structure *)
  let build_fenwick_tree (f: 'a -> 'a -> 'a) (b: 'a) (k: int) (s: 'a t): ('a array * int) =
    if k < 2 then failwith "cannot have k < 2" else
    let len = length s in
    let tree = clone s in
    let group_subtrees (tree: 'a array) (subtree_size: int) (k: int): unit =
      let rec group_sequentially (subtree_num: int): unit =
        let offset = subtree_num * subtree_size * k in
        let acc = ref b in
        let last = min k ((len - offset) / subtree_size) in
        for i = 1 to last do
          let idx = offset + (i * subtree_size) in
          acc := f !acc tree.(idx - 1);
          tree.(idx - 1) <- !acc;
        done;
      in
      let ceil_div num den = (num + den - 1) / den in
      parallel_for (ceil_div len (subtree_size * k)) group_sequentially;
      ()
    in
    let rec loop (tree: 'a array) (prev_size: int): int =
      if prev_size > len then prev_size / k
      else begin
        group_subtrees tree prev_size k;
        loop tree (prev_size * k)
      end
    in
    let last_size = loop tree 1 in
    (tree, last_size)
  
  let get_from_fenwick_tree (f: 'a -> 'a -> 'a) (b: 'a) (k: int) (tree: 'a array) (last_size: int) (i: int): 'a =
    let i = i + 1 in
    let round_down idx d = d * (idx / d) in
    let rec get_sum (subtree_size: int) (last_idx: int) (acc: 'a) : 'a =
      if subtree_size > i then get_sum (subtree_size / k) last_idx acc else
      let layer_idx = round_down i subtree_size in
      if layer_idx = i then
        f acc tree.(layer_idx - 1)
      else if layer_idx = last_idx then
        get_sum (subtree_size / k) last_idx acc
      else
        get_sum (subtree_size / k) layer_idx (f acc tree.(layer_idx - 1))
    in
    get_sum last_size (-1) b
  
  
  let reduce_layer (f: 'a -> 'a -> 'a) (b: 'a) (s: 'a t): 'a t =
    (* TODO: Consider using both? *)
    combine f b (even_elts s) (odd_elts s)

  let reduce (g: 'a -> 'a -> 'a) (b: 'a) (s: 'a t): 'a =
    let rec helper (b: 'a) (s: 'a t): 'a =
      if length s = 1 then nth s 0
      else helper b (reduce_layer g b s)
    in
    if length s = 0 then b else helper b s

  let reduce_alt (g: 'a -> 'a -> 'a) (b: 'a) (s: 'a t): 'a =
    let tree, size = build_fenwick_tree g b Defines.sequential_cutoff s in
    get_from_fenwick_tree g b Defines.sequential_cutoff tree size (length s - 1)

  let map (f: 'a -> 'b) (s: 'a t): 'b t =
    let body idx =
      f s.(idx)
    in
    tabulate body (length s)
  
  let map_reduce (inject: 'a -> 'b) (combine: 'b -> 'b -> 'b) (b: 'b) (s: 'a t): 'b =
    map inject s
    |> reduce combine b
    
  let interleave (s1: 'a t) (s2: 'a t): 'a t =
    let len1, len2 = length s1, length s2 in
    if len1 != len2 then
      raise (Invalid_argument "Sequences are different lengths")
    else
      let body idx =
        if idx mod 2 = 0 then nth s1 (idx / 2) else nth s2 (idx / 2)
      in
      tabulate (body) (len1 + len2)

  (* Algorithm inspired by a power of 2-restrained implementation from NESL *)
  let scan (f: 'a -> 'a -> 'a) (b: 'a) (s: 'a t): 'a t =
    let rec helper f b s =
      if length s = 1 then
        singleton b
      else
        (* TODO: Consider using both? *)
        let even_elts = even_elts s in
        let odd_elts = odd_elts s in
        let s' = helper f b (combine f b even_elts odd_elts) in
        let half_sums = combine f b even_elts s' in
        let body idx =
          if idx mod 2 = 0 then nth s' (idx / 2) else nth half_sums (idx / 2)
        in
        tabulate (body) (length s)
    in
    if length s = 0 then empty () else helper f b s
 
  let flatten (ss: 'a t t): 'a t =
    let lens = map (fun s -> length s) ss in
    let total_len = reduce (+) 0 lens in
    let starts = scan (+) 0 lens in
    let arr: 'a array = Array_handler.get_uninitialized total_len in
    parallel_for (length ss) (fun i -> 
      let start = starts.(i) in
      let s = ss.(i) in
      parallel_for (length s) (fun j ->
        arr.(start + j) <- s.(j)
      )
    );
    arr

  let scan_alt (f: 'a -> 'a -> 'a) (b: 'a) (s: 'a t): 'a t =
    let tree, size = build_fenwick_tree f b Defines.sequential_cutoff s in
    tabulate (fun i -> get_from_fenwick_tree f b Defines.sequential_cutoff tree size (i)) (length s)
end
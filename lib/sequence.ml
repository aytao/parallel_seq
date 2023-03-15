open Domainslib

module type S = sig
  type 'a t
  val tabulate : (int -> 'a) -> int -> 'a t
  val seq_of_array : 'a array -> 'a t
  val array_of_seq : 'a t -> 'a array
  val iter: ('a -> unit) -> 'a t -> unit
  val iteri: (int -> 'a -> unit) -> 'a t -> unit
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
  val filter: ('a -> bool) -> 'a t -> 'a t
end




(* TODO: Address temp solution for domain count and pooling *)
let pool = Task.setup_pool ~num_domains:(Defines.num_domains - 1) ()
  
(* TODO: Address nested run(?) *)
let run (task: 'a Task.task): 'a =
  Task.run pool task

let parallel_for (n: int) (body: (int -> unit)): unit =
  run (fun _ ->
    Task.parallel_for ~start:0 ~finish:(n - 1) ~body:body pool  
  )

let both (f: 'a -> 'b) (x: 'a) (g: 'c -> 'd) (y: 'c): 'b * 'd =
  run (fun _ ->
    let xp = Task.async pool (fun _ -> f x) in
    let y = g y in
    (Task.await pool xp, y)
  )

module FlatArraySeq : S = struct

  type 'a t = 'a Array.t

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

  let iter =
    Array.iter

  let iteri =
    Array.iteri
  
  let print (to_string: 'a -> string) (s: 'a t): unit =
    iter (fun v -> Printf.printf "%s, " (to_string v)) s;
    print_newline ()

  let length (s: 'a t): int =
    Array.length s

  let cons (x: 'a) (s: 'a t): 'a t =
    let len = length s in
    tabulate (fun i -> if i = 0 then x else s.(i - 1)) (len + 1)

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

  (* Returns a calculated k-way fenwick tree, as well as the size of the greatest subtree in the structure *)
  let build_fenwick_tree (f: 'a -> 'a -> 'a) (b: 'a) (k: int) (s: 'a t): ('a array * int) =
    if k < 2 then failwith "cannot have k < 2" else
    let len = length s in
    let tree = clone s in
    (* TODO: remove redundant params *)
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
  
  let collapse_fenwick_tree (f: 'a -> 'a -> 'a) (b: 'a) (k: int) (tree: 'a array) (last_size: int): 'a t =
    let len = Array.length tree in
    let round_down idx d = d * (idx / d) in
    let collapse_layer (prev_size: int): unit =
      let subtree_size = prev_size / k in
      let rec collapse_sequentially (group_num: int): unit =
        (* First group is already correct *)
        if group_num = 0 then () else
        let offset = group_num * prev_size in
        let last = min (k - 1) ((len - offset) / subtree_size) in
        (* No need to do first *)
        for i = 1 to last do
          let idx = offset + (i * subtree_size) in
          let prev_group_idx = round_down idx prev_size in
          tree.(idx - 1) <- f tree.(prev_group_idx - 1) tree.(idx - 1);
        done;
      in
      let ceil_div num den = (num + den - 1) / den in
      parallel_for (ceil_div len (prev_size)) collapse_sequentially;
      ()
    in
    let rec loop (tree: 'a array) (prev_size: int): unit =
      if prev_size <= 1 then ()
      else begin
        collapse_layer prev_size;
        loop tree (prev_size / k)
      end
    in
    loop tree last_size;
    tree
  
  let even_elts (s: 'a t): 'a t =
    let num_elts = (length s + 1) / 2 in
    tabulate (fun i -> s.(i * 2)) num_elts
  
  let odd_elts (s: 'a t): 'a t =
    let num_elts = length s / 2 in
    tabulate (fun i -> s.(i * 2 + 1)) num_elts

  (* Applies f evens.(i) odds.(i) for all valid indices in odd. If even is
      one item longer, the last element of the array returned is f evens.(i) b *)
  let combine (f: 'a -> 'a -> 'a) (b: 'a) (evens: 'a t) (odds: 'a t): 'a t =
    let len = length evens in
    let uneven = (len != length odds) in
    let body (idx: int): 'a =
      let e = nth evens idx in
      let o = (if uneven && idx = len - 1 then b else nth odds idx) in
      f e o
    in
    tabulate body len
  
  let reduce_layer (f: 'a -> 'a -> 'a) (b: 'a) (s: 'a t): 'a t =
    (* TODO: Consider using both? *)
    combine f b (even_elts s) (odd_elts s)

  let tree_reduce (g: 'a -> 'a -> 'a) (b: 'a) (s: 'a t): 'a =
    let rec helper (b: 'a) (s: 'a t): 'a =
      if length s = 1 then nth s 0
      else helper b (reduce_layer g b s)
    in
    if length s = 0 then b else helper b s

  let fenwick_reduce (g: 'a -> 'a -> 'a) (b: 'a) (s: 'a t): 'a =
    let tree, size = build_fenwick_tree g b Defines.sequential_cutoff s in
    get_from_fenwick_tree g b Defines.sequential_cutoff tree size (length s - 1)
    
  let reduce = fenwick_reduce

  let map (f: 'a -> 'b) (s: 'a t): 'b t =
    let body idx =
      f s.(idx)
    in
    tabulate body (length s)
  
  let map_reduce (inject: 'a -> 'b) (combine: 'b -> 'b -> 'b) (b: 'b) (s: 'a t): 'b =
    map inject s
    |> reduce combine b

  (* Algorithm inspired by a power of 2-restrained implementation from NESL *)
  let nesl_exclusive_scan (f: 'a -> 'a -> 'a) (b: 'a) (s: 'a t): 'a t =
    let rec helper f b s =
      if length s = 1 then
        singleton b
      else
        (* TODO: Consider using both? *)
        let even_elts = even_elts s in
        let odd_elts = odd_elts s in
        let s' = helper f b (combine f b even_elts odd_elts) in
        (* Since elements of s' represents left prefix sum, order must be reversed so that communativity is not required *)
        let half_sums = combine f b s' even_elts in
        let body idx =
          if idx mod 2 = 0 then nth s' (idx / 2) else nth half_sums (idx / 2)
        in
        tabulate (body) (length s)
    in
    if length s = 0 then empty () else helper f b s
  
  (* The NESL algorithm doesn't include the value at the index as part of the prefix sum,
     so that prefix[i] = f (f ( f(... f (b s[0]) ...) s[i - 2]) s[i - 1]).
     However, the API specifies that prefix sum should be inclusive, so that
     prefix[i] = f (f ( f(... f (b s[0]) ...) s[i - 1]) s[i])
     As a workaround, combine each element to the exclusive prefix sum to
     match the inclusive prefix specified by the API
     *)
  let nesl_inclusive_scan (f: 'a -> 'a -> 'a) (b: 'a) (s: 'a t): 'a t =
    let exlcusive_scan = nesl_exclusive_scan f b s in
    tabulate (fun i -> f (nth exlcusive_scan i) (nth s i)) (length s)


  let fenwick_scan (f: 'a -> 'a -> 'a) (b: 'a) (s: 'a t): 'a t =
    let tree, size = build_fenwick_tree f b Defines.sequential_cutoff s in
    collapse_fenwick_tree f b Defines.sequential_cutoff tree size
  
  let scan = fenwick_scan
 
  let flatten (ss: 'a t t): 'a t =
    let lens = map (fun s -> length s) ss in
    let total_len = tree_reduce (+) 0 lens in
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

  let filter (pred: 'a -> bool) (s: 'a t): 'a t =
    let length = length s in
    if length = 0 then empty () else
    let bit_vec = map (fun v -> if pred v then 1 else 0) s in
    let prefixes = scan (+) 0 bit_vec in
    let filtered_length = nth prefixes (length - 1) in
    let filtered: 'a array = Array_handler.get_uninitialized filtered_length in
    let body idx =
      let l = if idx = 0 then 0 else nth prefixes (idx - 1) in
      if nth prefixes idx > l then filtered.(l) <- nth s idx else ()
    in
    parallel_for length body;
    filtered
end


module NestedArraySeq : S = struct

  type 'a t = {contents: 'a array array; grain: int; num_sections: int; length: int}

  let sequential_cutoff = Defines.sequential_cutoff
  let num_domains = Defines.num_domains

  let ceil_div num den = (num + den - 1) / den

  let empty (): 'a t =
    {contents = [||]; grain = 0; num_sections = 0; length = 0}

  (* TODO: Amount? Round up? Round down? *)
  let calculate_grain (length: int): int * int =
    let grain = max sequential_cutoff (ceil_div length num_domains) in
    let num_sections = ceil_div length grain in
    grain, num_sections

  let tabulate (f: int -> 'a) (n: int): 'a t =
    if n = 0 then
      empty ()
    else if n < 0 then
      raise (Invalid_argument "Cannot make sequence of negative length")
    else (
      let grain, num_sections = calculate_grain n in
      let outer_arr: 'a array array = Array_handler.get_uninitialized num_sections in
      
      let sequential_init (i: int): unit =
        (* TODO: In parallel? *)
        let section_length = min grain (n - i * grain) in
        let section = Array.init section_length (fun idx -> f (i * grain + idx)) in
        (* TODO: Unsafe set? *)
        outer_arr.(i) <- section
      in
      parallel_for num_sections sequential_init;
      {contents = outer_arr; grain = grain; num_sections = num_sections; length = n}
    )

  (* NOTE: For internal use only *)
  let set (contents: 'a array array) (grain: int) (idx: int) (v: 'a): unit =
    let section_num = idx / grain in
    let section_idx = idx mod grain in
    contents.(section_num).(section_idx) <- v

  let nth ({contents; grain; num_sections; length}: 'a t) (n: int): 'a =
    let section_num = n / grain in
    let section_idx = n mod grain in
    contents.(section_num).(section_idx)
    
  let length (s: 'a t): int =
    s.length

  let seq_of_array (arr: 'a array): 'a t =
    tabulate (fun i -> arr.(i)) (Array.length arr)

  let array_of_seq (s: 'a t): 'a array =
    Array.init (length s) (fun i -> nth s i)
  
  let clone (s: 'a t): 'a t =
    tabulate (fun i -> nth s i) (length s)

  let iter (f: 'a -> unit) (s: 'a t): unit =
    let inner_iter (section: 'a array): unit =
      Array.iter f section
    in
    Array.iter inner_iter s.contents
  
  let iteri (f: int -> 'a -> unit) (s: 'a t): unit =
    let modified_f section_num i v =
      f (s.grain * section_num + i) v
    in
    let inner_iteri (section_num: int) (section: 'a array): unit =
      Array.iteri (modified_f section_num) section
    in
    Array.iteri inner_iteri s.contents

  let cons (x: 'a) (s: 'a t): 'a t =
    let len = length s in
    tabulate (fun i -> if i = 0 then x else nth s (i - 1)) (len + 1)

  let singleton (x: 'a): 'a t =
    {contents = [|[|x|]|]; grain = 1; num_sections = 1; length = 1}

  let append (s1: 'a t) (s2: 'a t): 'a t =
    let len1, len2 = length s1, length s2 in
    let body (idx: int): 'a =
      if idx < len1 then nth s1 idx else nth s2 (idx - len1)
    in
    tabulate body (len1 + len2)

  let repeat (x: 'a) (n: int): 'a t =
    tabulate (fun _ -> x) n

  let zip ((s1, s2): 'a t * 'b t): ('a * 'b) t =
    let len1, len2 = length s1, length s2 in
    if len1 != len2 then
      raise (Invalid_argument "Sequences are different lengths")
    else
      tabulate (fun i -> (nth s1 i, nth s2 i)) len1

  let split (s: 'a t) (i: int): 'a t * 'a t =
    let len = length s in
    if i < 0 || i > len then
      raise (Invalid_argument "i is outside of bounds")
    (* TODO: Ensure reuse is safe *)
    else if i = 0 then
      (empty (), s)
    else if i = len then
      (s, empty ())
    else
      both (tabulate (fun idx -> nth s idx)) i
           (tabulate (fun idx -> nth s (i + idx))) (len - i)

  let reduce (g: 'a -> 'a -> 'a) (b: 'a) (s: 'a t): 'a =
    let sequential_reduce: ('a array -> 'a) =
      Array.fold_left g b
    in
    let section_sums = Array_handler.get_uninitialized s.num_sections in
    let reduce_section (idx: int): unit =
      section_sums.(idx) <- sequential_reduce s.contents.(idx)
    in
    parallel_for s.num_sections reduce_section;
    sequential_reduce section_sums

  let map (f: 'a -> 'b) (s: 'a t): 'b t =
    let body idx =
      f (nth s idx)
    in
    tabulate body (length s)
  
  let map_reduce (inject: 'a -> 'b) (combine: 'b -> 'b -> 'b) (b: 'b) (s: 'a t): 'b =
    map inject s
    |> reduce combine b

  let scan (f: 'a -> 'a -> 'a) (b: 'a) (s: 'a t): 'a t =
    let sequential_array_scan (arr: 'a array) (b': 'a): 'a =
      let acc = ref b' in
      for i = 0 to (Array.length arr - 1) do
        acc := f !acc arr.(i);
        arr.(i) <- !acc;
      done;
      !acc
    in
    let section_sums: 'a array = Array_handler.get_uninitialized s.num_sections in
    let new_contents: 'a array array = Array_handler.get_uninitialized s.num_sections in
    let scan_section (i: int): unit =
      let section_copy = Array.copy s.contents.(i) in
      let section_sum = sequential_array_scan section_copy b in
      section_sums.(i) <- section_sum;
      new_contents.(i) <- section_copy
    in
    parallel_for s.num_sections scan_section;
    sequential_array_scan section_sums b;
    (* TODO: This could probably be a bit cleaner *)
    let update_section (i: int): unit =
      if i = 0 then () else
      let section_prefix = section_sums.(i - 1) in
      let section = new_contents.(i) in
      for j = 0 to (Array.length section - 1) do
        section.(j) <- f section_prefix section.(j) 
      done;
    in
    parallel_for (s.num_sections) update_section;
    {contents = new_contents; grain = s.grain; num_sections = s.num_sections; length = s.length}

 
  let flatten (ss: 'a t t): 'a t =    
    let lengths = map length ss in
    (* TODO: Can save a bit of recomputation here with reduce and scan *)
    let total_len = reduce (+) 0 lengths in
    let starts = scan (+) 0 lengths in
    let grain, num_sections = calculate_grain total_len in
    let new_contents = Array_handler.get_uninitialized num_sections in
    let set_contents (i: int) (v: 'a): unit =    
      let section_num, section_idx = (i / grain, i mod grain) in
      new_contents.(section_num).(section_idx) <- v
    in
    parallel_for (length ss) (fun i -> 
      let start = nth starts i in
      let s = nth ss i in
      parallel_for (length s) (fun j ->
        set_contents (start + j) (nth s j)
      )
    );
    {contents = new_contents; grain = grain; num_sections = num_sections; length = total_len}

  let filter (pred: 'a -> bool) (s: 'a t): 'a t =
    failwith "Unimplemented"
end

module S = FlatArraySeq
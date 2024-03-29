(**************************************************************************)
(* Copyright (C) 2023  Andrew Tao                                         *)
(*                                                                        *)
(* This library is free software; you can redistribute it and/or          *)
(* modify it under the terms of the GNU Lesser General Public             *)
(* License as published by the Free Software Foundation; either           *)
(* version 2.1 of the License, or (at your option) any later version.     *)
(*                                                                        *)
(* This library is distributed in the hope that it will be useful,        *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      *)
(* Lesser General Public License for more details.                        *)
(*                                                                        *)
(* You should have received a copy of the GNU Lesser General Public       *)
(* License along with this library; if not, write to the Free Software    *)
(* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,             *)
(* MA  02110-1301  USA                                                    *)
(**************************************************************************)

open Domainslib
include Sequence_intf

module type Parallel_array_configs = sig
  val pool : Task.pool
  val sequential_cutoff : int
end

module ArraySeq : S = struct
  type 'a t = 'a array

  let tabulate (f : int -> 'a) (n : int) : 'a t = Array.init n f
  let seq_of_array (a : 'a array) : 'a t = Array.copy a
  let array_of_seq (s : 'a t) : 'a array = Array.copy s
  let iter = Array.iter
  let iteri = Array.iteri
  let length = Array.length
  let empty () : 'a t = [||]
  let cons (x : 'a) (s : 'a t) : 'a t = Array.append [| x |] s
  let singleton (x : 'a) : 'a t = [| x |]
  let append = Array.append
  let nth = Array.get
  let map = Array.map

  let map_reduce (f : 'a -> 'b) (g : 'b -> 'b -> 'b) (b : 'b) (s : 'a t) : 'b =
    let n = Array.length s in
    let rec iter acc i =
      if i = n then acc else iter (g acc (f s.(i))) (i + 1)
    in
    iter b 0

  let reduce = Array.fold_left

  let flatten (a : 'a array array) : 'a array =
    let n = Array.length a in
    let m = map_reduce Array.length ( + ) 0 a in
    if m = 0 then empty ()
    else
      let b = Array_handler.get_uninitialized m in
      let rec copy i k =
        if i = n then ()
        else
          let len = Array.length a.(i) in
          Array.blit a.(i) 0 b k len;
          copy (i + 1) (k + len)
      in
      copy 0 0;
      b

  let repeat (x : 'a) (n : int) : 'a t = Array.make n x

  let zip ((s1, s2) : 'a t * 'b t) : ('a * 'b) t =
    let len1, len2 = (length s1, length s2) in
    if len1 <> len2 then raise (Invalid_argument "Parallel_seq.zip")
    else tabulate (fun i -> (s1.(i), s2.(i))) len1

  let split s i = (Array.sub s 0 i, Array.sub s i (Array.length s - i))

  let scan (f : 'a -> 'a -> 'a) (b : 'a) (s : 'a array) =
    let u = Array.copy s in
    let n = Array.length s in
    let rec iter i x =
      if i = n then ()
      else
        let y = f x u.(i) in
        u.(i) <- y;
        iter (i + 1) y
    in
    iter 0 b;
    u

  let filter (pred : 'a -> bool) (s : 'a t) : 'a t =
    let matches = map pred s in
    let num_matches =
      map_reduce (fun b -> if b then 1 else 0) ( + ) 0 matches
    in
    let a = Array_handler.get_uninitialized num_matches in
    let rec iter i next =
      if next = num_matches then ()
      else if matches.(i) then (
        a.(next) <- s.(i);
        iter (i + 1) (next + 1))
      else iter (i + 1) next
    in
    iter 0 0;
    a
end

module ParallelArraySeq (P : Parallel_array_configs) : S = struct
  type 'a t = 'a Array.t

  let pool = P.pool
  let num_domains = Task.get_num_domains pool
  let run (task : 'a Task.task) : 'a = Task.run pool task

  let parallel_for (n : int) (body : int -> unit) : unit =
    run (fun _ -> Task.parallel_for ~start:0 ~finish:(n - 1) ~body pool)

  let ceil_div num den = (num + den - 1) / den
  let empty () : 'a t = [||]

  let tabulate (f : int -> 'a) (n : int) : 'a t =
    if n = 0 then empty ()
    else if n < 0 then raise (Invalid_argument "Parallel_seq.tabulate")
    else
      let arr : 'a array = Array_handler.get_uninitialized n in
      parallel_for n (fun i -> arr.(i) <- f i);
      arr

  let seq_of_array (arr : 'a array) : 'a t =
    tabulate (fun i -> arr.(i)) (Array.length arr)

  let array_of_seq (s : 'a t) : 'a array =
    tabulate (fun i -> s.(i)) (Array.length s)

  let clone (s : 'a t) : 'a t = tabulate (fun i -> s.(i)) (Array.length s)
  let iter = Array.iter
  let iteri = Array.iteri
  let length : 'a t -> int = Array.length

  let cons (x : 'a) (s : 'a t) : 'a t =
    let len = length s in
    tabulate (fun i -> if i = 0 then x else s.(i - 1)) (len + 1)

  let singleton (x : 'a) : 'a t = Array.make 1 x

  let append (s1 : 'a t) (s2 : 'a t) : 'a t =
    let len1, len2 = (length s1, length s2) in
    let body (idx : int) : 'a =
      if idx < len1 then s1.(idx) else s2.(idx - len1)
    in
    tabulate body (len1 + len2)

  let nth (s : 'a t) (n : int) : 'a = s.(n)
  let repeat (x : 'a) (n : int) : 'a t = tabulate (fun _ -> x) n

  let zip ((s1, s2) : 'a t * 'b t) : ('a * 'b) t =
    let len1, len2 = (length s1, length s2) in
    if len1 <> len2 then raise (Invalid_argument "Parallel_seq.zip")
    else tabulate (fun i -> (s1.(i), s2.(i))) len1

  let split (s : 'a t) (i : int) : 'a t * 'a t =
    let len = length s in
    if i < 0 || i > len then raise (Invalid_argument "Parallel_seq.split")
    else
      let l = Array_handler.get_uninitialized i in
      let r = Array_handler.get_uninitialized (len - i) in
      let body idx =
        if idx < i then l.(idx) <- s.(idx) else r.(idx - i) <- s.(idx)
      in
      parallel_for len body;
      (l, r)

  (* Returns a calculated k-way fenwick tree, as well as the size of the greatest subtree in the structure *)
  let build_fenwick_tree (f : 'a -> 'a -> 'a) (b : 'a) (k : int) (s : 'a t) :
      'a array * int =
    assert (k >= 2);
    (* cannot have k < 2 *)
    let len = length s in
    let tree = clone s in
    let group_subtrees (subtree_size : int) : unit =
      let group_sequentially (group_num : int) : unit =
        let offset = group_num * subtree_size * k in
        let acc = ref b in
        let last = min k ((len - offset) / subtree_size) in
        for i = 1 to last do
          let idx = offset + (i * subtree_size) in
          acc := f !acc tree.(idx - 1);
          tree.(idx - 1) <- !acc
        done
      in
      parallel_for (ceil_div len (subtree_size * k)) group_sequentially;
      ()
    in
    let rec loop (prev_size : int) : int =
      if prev_size > len then prev_size / k
      else (
        group_subtrees prev_size;
        loop (prev_size * k))
    in
    let last_size = loop 1 in
    (tree, last_size)

  let get_from_fenwick_tree (f : 'a -> 'a -> 'a) (b : 'a) (k : int)
      (tree : 'a array) (last_size : int) (i : int) : 'a =
    let i = i + 1 in
    let round_down idx d = d * (idx / d) in
    let rec get_sum (subtree_size : int) (last_idx : int) (acc : 'a) : 'a =
      if subtree_size > i then get_sum (subtree_size / k) last_idx acc
      else
        let layer_idx = round_down i subtree_size in
        if layer_idx = i then f acc tree.(layer_idx - 1)
        else if layer_idx = last_idx then
          get_sum (subtree_size / k) last_idx acc
        else get_sum (subtree_size / k) layer_idx (f acc tree.(layer_idx - 1))
    in
    get_sum last_size (-1) b

  let collapse_fenwick_tree (f : 'a -> 'a -> 'a) (k : int) (tree : 'a array)
      (last_size : int) : 'a t =
    let len = Array.length tree in
    let collapse_layer (prev_size : int) : unit =
      let subtree_size = prev_size / k in
      let collapse_sequentially (group_num : int) : unit =
        let offset = (group_num + 1) * prev_size in
        let last = min (k - 1) ((len - offset) / subtree_size) in
        (* No need to do first *)
        for i = 1 to last do
          let idx = offset + (i * subtree_size) in
          tree.(idx - 1) <- f tree.(offset - 1) tree.(idx - 1)
        done
      in
      (* First group is already correct *)
      parallel_for (ceil_div len prev_size - 1) collapse_sequentially;
      ()
    in
    let rec loop (tree : 'a array) (prev_size : int) : unit =
      if prev_size <= 1 then ()
      else (
        collapse_layer prev_size;
        loop tree (prev_size / k))
    in
    loop tree last_size;
    tree

  let get_fenwick_k (n : int) : int =
    min (max 2 (n / num_domains)) P.sequential_cutoff

  let reduce (g : 'a -> 'a -> 'a) (b : 'a) (s : 'a t) : 'a =
    match length s with
    | 0 -> b
    | len ->
        let k = get_fenwick_k len in
        let tree, size = build_fenwick_tree g b k s in
        get_from_fenwick_tree g b k tree size (length s - 1)

  let map (f : 'a -> 'b) (s : 'a t) : 'b t =
    let body idx = f s.(idx) in
    tabulate body (length s)

  let map_reduce (inject : 'a -> 'b) (combine : 'b -> 'b -> 'b) (b : 'b)
      (s : 'a t) : 'b =
    map inject s |> reduce combine b

  let scan (f : 'a -> 'a -> 'a) (b : 'a) (s : 'a t) : 'a t =
    match length s with
    | 0 -> empty ()
    | len ->
        let k = get_fenwick_k len in
        let tree, size = build_fenwick_tree f b k s in
        collapse_fenwick_tree f k tree size

  let flatten (ss : 'a t t) : 'a t =
    if length ss = 0 then empty ()
    else
      let lens = map (fun s -> length s) ss in
      let starts = scan ( + ) 0 lens in
      let total_len = nth starts (length starts - 1) in
      let arr : 'a array = Array_handler.get_uninitialized total_len in
      parallel_for (length ss) (fun i ->
          let start = if i = 0 then 0 else starts.(i - 1) in
          let s = ss.(i) in
          parallel_for (length s) (fun j -> arr.(start + j) <- s.(j)));
      arr

  let filter (pred : 'a -> bool) (s : 'a t) : 'a t =
    let len = length s in
    if len = 0 then empty ()
    else
      let bit_vec = map (fun v -> if pred v then 1 else 0) s in
      let prefixes = scan ( + ) 0 bit_vec in
      let filtered_length = nth prefixes (len - 1) in
      let filtered : 'a array =
        Array_handler.get_uninitialized filtered_length
      in
      let body idx =
        let l = if idx = 0 then 0 else nth prefixes (idx - 1) in
        if nth prefixes idx > l then filtered.(l) <- nth s idx else ()
      in
      parallel_for len body;
      filtered
end

let get_module (seq_type : seq_type) : (module S) =
  match seq_type with
  | Sequential -> (module ArraySeq : S)
  | Parallel { pool; sequential_cutoff } ->
      (module ParallelArraySeq (struct
        let pool = pool
        let sequential_cutoff = sequential_cutoff
      end) : S)

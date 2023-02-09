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
end

module ParallelSeq : S = struct

  type 'a t = 'a Array.t
  
  (* TODO: Address temp solution for domain count and pooling *)
  let pool = Task.setup_pool ~num_domains:(Defines.num_domains - 1) ()
  
  (* TODO: Address nested run(?) *)
  let run (task: 'a Task.task): 'a =
    Task.run pool task

  let tabulate (f: int -> 'a) (n: int): 'a t =
    (* TODO: Address array initialization *)
    let arr = Array.make n (f 0) in
    run (fun _ ->
      Task.parallel_for ~start:0 ~finish:(n - 1) ~body:(fun i -> arr.(i) <- f i) pool
      );
    arr

  let seq_of_array (arr: 'a array): 'a t =
    tabulate (fun i -> arr.(i)) (Array.length arr)

  let array_of_seq (s: 'a t): 'a array =
    tabulate (fun i -> s.(i)) (Array.length s)

  let iter (f: 'a -> unit) (s: 'a t): unit =
    Array.iter f s

  let length (s: 'a t): int =
    Array.length s

  let empty (): 'a t =
    failwith "Unimplemented"

  let cons (x: 'a) (s: 'a t): 'a t =
    failwith "Unimplemented"

  let singleton (x: 'a): 'a t =
    Array.make 1 x

  let append (s1: 'a t) (s2: 'a t): 'a t =
    failwith "Unimplemented"

  let nth (s: 'a t) (n: int): 'a =
    s.(n)

  let map (f: 'a -> 'b) (s: 'a t): 'b t =
    let body idx =
      f s.(idx)
    in
    tabulate body (length s)
  
  let map_reduce (inject: 'a -> 'b) (combine: 'b -> 'b -> 'b) (b: 'b) (s: 'a t): 'b =
    failwith "Unimplemented"

  let reduce (g: 'a -> 'a -> 'a) (base: 'a) (s: 'a t): 'a =
    failwith "Unimplemented"

  let flatten (ss: 'a t t): 'a t =
    failwith "Unimplemented"

  let repeat (x: 'a) (n: int): 'a t =
    failwith "Unimplemented"

  let zip ((s1, s2): 'a t * 'b t): ('a * 'b) t =
    failwith "Unimplemented"

  let split (s: 'a t) (i: int): 'a t * 'a t =
    failwith "Unimplemented"
  
  let interleave (s1: 'a t) (s2: 'a t): 'a t =
    let len1, len2 = length s1, length s2 in
    if len1 != len2 then
      raise (Invalid_argument "Sequences are different lengths")
    else
      let body idx =
        if idx mod 2 = 0 then nth s1 (idx / 2) else nth s2 (idx / 2)
      in
      tabulate (body) (len1 + len2)

  let map2 (f: 'a -> 'b -> 'c) (s1: 'a t) (s2: 'b t): 'c t =
    let len1, len2 = length s1, length s2 in
    if len1 != len2 then
      raise (Invalid_argument "Sequences are different lengths")
    else
      tabulate (fun idx -> f s1.(idx) s2.(idx)) len1

  let even_elts (s: 'a t): 'a t =
    let num_elts = (length s + 1) / 2 in
    tabulate (fun i -> s.(i * 2)) num_elts
  
  let odd_elts (s: 'a t): 'a t =
    let num_elts = length s / 2 in
    tabulate (fun i -> s.(i * 2 + 1)) num_elts

  (* Algorithm inspired by a power of 2-restrained implementation from NESL *)
  let scan (f: 'a -> 'a -> 'a) (b: 'a) (s: 'a t): 'a t =
    let combine (even_elts: 'a t) (odd_elts: 'a t): 'a t =
      let len = length even_elts in
      let uneven = (len != length odd_elts) in
      let body (idx: int): 'a =
        let e = nth even_elts idx in
        let o = (if uneven && idx = len - 1 then b else nth odd_elts idx) in
        f e o
      in
      tabulate body len
    in
    let rec helper f b s =
      if length s = 1 then
        singleton b
      else
        let even_elts = even_elts s in
        let odd_elts = odd_elts s in
        let s' = helper f b (combine even_elts odd_elts) in
        let half_sums = combine even_elts s' in
        let body idx =
          if idx mod 2 = 0 then nth s' (idx / 2) else nth half_sums (idx / 2)
        in
        tabulate (body) (length s)
    in
    if length s = 0 then empty () else helper f b s
end
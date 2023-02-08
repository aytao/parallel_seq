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
  let tabulate (f: int -> 'a) (b: int): 'a t =
    failwith "Unimplemented"

  let seq_of_array (arr: 'a array): 'a t =
    failwith "Unimplemented"

  let array_of_seq (s: 'a t): 'a array =
    failwith "Unimplemented"

  let iter (f: 'a -> unit) (s: 'a t): unit =
    failwith "Unimplemented"

  let length (s: 'a t): int =
    failwith "Unimplemented"

  let empty (): 'a t =
    failwith "Unimplemented"

  let cons (x: 'a) (s: 'a t): 'a t =
    failwith "Unimplemented"

  let singleton (x: 'a): 'a t =
    failwith "Unimplemented"

  let append (s1: 'a t) (s2: 'a t): 'a t =
    failwith "Unimplemented"

  let nth (s: 'a t) (n: int): 'a =
    failwith "Unimplemented"

  let map (f: 'a -> 'b) (s: 'a t): 'b t =
    failwith "Unimplemented"
  
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
  
  let scan (f: 'a -> 'a -> 'a) (b: 'a) (s: 'a t): 'a t =
    failwith "Unimplemented"
end
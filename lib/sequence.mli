open Domainslib

module type S = sig
  type 'a t

  val iter : ('a -> unit) -> 'a t -> unit
  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  val length : 'a t -> int
  val empty : unit -> 'a t
  val singleton : 'a -> 'a t
  val nth : 'a t -> int -> 'a
  val cons : 'a -> 'a t -> 'a t
  val tabulate : (int -> 'a) -> int -> 'a t
  val repeat : 'a -> int -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val seq_of_array : 'a array -> 'a t
  val array_of_seq : 'a t -> 'a array
  val zip : 'a t * 'b t -> ('a * 'b) t
  val split : 'a t -> int -> 'a t * 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val reduce : ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a
  val map_reduce : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'b -> 'a t -> 'b
  val scan : ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a t
  val flatten : 'a t t -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t
end

type seq_type = Sequential | Parallel of int

val get_module : seq_type -> (module S)

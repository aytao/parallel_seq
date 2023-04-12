open Sequence

module type Utils = sig
  type 'a s

  val print_sequence : ('a -> string) -> 'a s -> unit
end

module Make (S : S) : Utils with type 'a s = 'a S.t

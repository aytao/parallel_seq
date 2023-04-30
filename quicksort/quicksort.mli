open Seq

type 'a cmp = 'a -> 'a -> int

module Quicksort (S : Sequence.S) : sig
  val sort : 'a cmp -> 'a S.t -> 'a S.t
end

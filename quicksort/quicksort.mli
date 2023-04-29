open Seq

type 'a cmp = 'a -> 'a -> int

module Quicksort (S : Sequence.S) : sig
  val quicksort : 'a cmp -> 'a S.t -> 'a S.t
end

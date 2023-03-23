open Seq
open Sequence

type 'a cmp = 'a -> 'a -> int
val quicksort : 'a cmp -> 'a S.t -> 'a S.t
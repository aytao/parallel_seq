type t = (module Parallelseq.Sequence.S) -> ?n:int -> unit -> float

val time : ('a -> unit) -> 'a -> float

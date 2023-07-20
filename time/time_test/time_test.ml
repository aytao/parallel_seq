type t = (module Parallelseq.Sequence.S) -> ?n:int -> unit -> float

let time (f : 'a -> 'b) (x : 'a) : float =
  let t = Unix.gettimeofday () in
  let _fx = f x in
  let t' = Unix.gettimeofday () in
  t' -. t

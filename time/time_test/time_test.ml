type config = Parallelseq.Sequence.seq_type * int option
type t = config -> float

let time_test_pool_name = "time_test_pool"

let time (f : 'a -> 'b) (x : 'a) : float =
  let t = Unix.gettimeofday () in
  let _fx = f x in
  let t' = Unix.gettimeofday () in
  t' -. t

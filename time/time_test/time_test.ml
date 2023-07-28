type with_length = Parallelseq.Sequence.seq_type -> int option -> float
type with_file = Parallelseq.Sequence.seq_type -> string -> float
type t = With_length of with_length | With_file of with_file

let time_test_pool_name = "time_test_pool"

let time (f : 'a -> 'b) (x : 'a) : float =
  let t = Unix.gettimeofday () in
  let _fx = f x in
  let t' = Unix.gettimeofday () in
  t' -. t

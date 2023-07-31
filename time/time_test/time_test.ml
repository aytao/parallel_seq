type with_length = Parallel_seq.seq_type -> int option -> float
type with_file = Parallel_seq.seq_type -> string -> float
type t = With_length of with_length | With_file of with_file

let time_test_pool_name = "time_test_pool"

let time (f : 'a -> 'b) (x : 'a) : float =
  let start_time = Unix.gettimeofday () in
  let _ = f x in
  let end_time = Unix.gettimeofday () in
  end_time -. start_time

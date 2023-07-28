type with_length = Parallelseq.Sequence.seq_type -> int option -> float
type with_file = Parallelseq.Sequence.seq_type -> string -> float
type t = With_length of with_length | With_file of with_file

val time_test_pool_name : string
val time : ('a -> unit) -> 'a -> float

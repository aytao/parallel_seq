type config = Parallelseq.Sequence.seq_type * int option
type t = config -> float

val time_test_pool_name : string
val time : ('a -> unit) -> 'a -> float

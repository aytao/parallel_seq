type config = Parallelseq.Sequence.seq_type * int option
type t = config -> float

val time : ('a -> unit) -> 'a -> float

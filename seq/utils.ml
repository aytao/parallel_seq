open Sequence

let print_sequence (to_string: 'a -> string) (s: 'a ParallelSeq.t): unit =
  ParallelSeq.iter (fun i -> print_string ((to_string i)^",")) s
open Sequence

let print_sequence (to_string: 'a -> string) (s: 'a FlatArraySeq.t): unit =
  FlatArraySeq.iter (fun i -> print_string ((to_string i)^",")) s
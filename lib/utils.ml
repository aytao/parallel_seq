open Sequence

let print_flat_arr_sequence (to_string: 'a -> string) (s: 'a FlatArraySeq.t): unit =
  FlatArraySeq.iter (fun i -> print_string ((to_string i)^",")) s

let print_nested_arr_sequence (to_string: 'a -> string) (s: 'a NestedArraySeq.t): unit =
  NestedArraySeq.iter (fun i -> print_string ((to_string i)^",")) s

let print_sequence (to_string: 'a -> string) (s: 'a S.t): unit =
  S.iter (fun i -> print_string ((to_string i)^",")) s
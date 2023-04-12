open Sequence

let print_sequence (to_string : 'a -> string) (s : 'a S.t) : unit =
  S.iter (fun i -> print_string (to_string i ^ ",")) s

open Sequence

module type Utils = sig
  type 'a s

  val print_sequence : ('a -> string) -> 'a s -> unit
end

module Make (S : S) : Utils with type 'a s = 'a S.t = struct
  type 'a s = 'a S.t

  let print_sequence (to_string : 'a -> string) (s : 'a s) : unit =
    S.iter (fun i -> print_string (to_string i ^ ",")) s
end

open Sequence

val print_flat_arr_sequence : ('a -> string) -> 'a FlatArraySeq.t -> unit
val print_nested_arr_sequence : ('a -> string) -> 'a NestedArraySeq.t -> unit
val print_sequence : ('a -> string) -> 'a S.t -> unit

module DocID : sig
  type t = int

  val compare : 'a -> 'a -> int
end

type doc_id = int

module DMap : Map.S with type key = string

type location = doc_id * doc_id * doc_id
type doc_loc_index (* = location array DMap.t *)

val make_index : string -> doc_loc_index

type doc_id = int

module DMap : Map.S with type key = string

type location = doc_id * doc_id * doc_id

module LocSet : Set.S with type elt = location

type doc_loc_index = LocSet.t DMap.t

val make_index : string -> doc_loc_index

type index_intermediate

val parse : string -> index_intermediate
val combine : index_intermediate -> doc_loc_index
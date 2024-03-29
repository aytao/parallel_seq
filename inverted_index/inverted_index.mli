open Parallel_seq

type doc_id = int

module DMap : Map.S with type key = string

type location = doc_id * doc_id * doc_id

module LocSet : Set.S with type elt = location

type doc_loc_index = LocSet.t DMap.t

module Indexer (_ : S) : sig
  val make_index : ?num_chunks:int -> string -> doc_loc_index
end

open Seq
open Sequence
open Index
open Time_utils

let _ = S.empty ()

module Indexer = Inverted_index.Indexer (S)

let index, elapsed_time =
  Time_utils.time Indexer.make_index "index_data/test_index_1000000.txt"

let _ = Printf.printf "%f\n" elapsed_time

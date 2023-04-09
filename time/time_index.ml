open Seq
open Sequence
open Index

let _ = S.empty ()

let index, elapsed_time =
  Time_utils.time Inverted_index.make_index "index_data/test_index_100000.txt"

let _ = Printf.printf "%f\n" elapsed_time

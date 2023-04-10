open Seq
open Sequence
open Index

let _ = S.empty ()

let ii, parse_elapsed_time =
  Time_utils.time Inverted_index.parse "index_data/test_index_2000000.txt"

let index, combine_elapsed_time = Time_utils.time Inverted_index.combine ii

let _ =
  Printf.printf "Parse: %f, Combine: %f\n" parse_elapsed_time
    combine_elapsed_time

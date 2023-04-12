open Seq
open Sequence
open Index

let _ = S.empty ()

(*
   let ii, parse_elapsed_time =
     Time_utils.time Inverted_index.parse "index_data/test_index_1000000.txt"

   let index, combine_elapsed_time = Time_utils.time Inverted_index.combine ii

   let _ =
     Printf.printf "Parse: %f, Combine: %f\n" parse_elapsed_time
       combine_elapsed_time
*)

let index, elapsed_time =
  Time_utils.time Inverted_index.make_index "index_data/test_index_1000000.txt"

let _ = Printf.printf "%f\n" elapsed_time

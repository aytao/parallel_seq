open Seq
open Sequence
open Utils

let a1 = [|2; 1; 4; -2; 3; 4; -1; 2|]
let a2 = [|2; 1; 4; -2; 3; 4; -1|]
let a3 = [|2; 1; 4; -2; 3; 4|]

let print_prefix_sum (a: int array): unit = 
  let s = ParallelSeq.seq_of_array a in
  let prefix_sum = ParallelSeq.scan (+) 0 s in
  print_sequence string_of_int prefix_sum;
  print_newline ()

let _ = print_prefix_sum a1
let _ = print_prefix_sum a2
let _ = print_prefix_sum a3

let ss = ParallelSeq.seq_of_array [|ParallelSeq.seq_of_array a1; ParallelSeq.seq_of_array a2; ParallelSeq.seq_of_array a3|]
let _ = print_sequence string_of_int (ParallelSeq.flatten ss)

let _ = print_newline ();;

let s = ParallelSeq.seq_of_array [||];;

let l, r = ParallelSeq.split s 0;;

let _ = print_sequence string_of_int l;;
let _ = print_newline ();;
let _ = print_sequence string_of_int r;;
let _ = print_newline ();;
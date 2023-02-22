open Seq
open Sequence
open Utils

let a1 = [|2; 1; 4; -2; 3; 4; -1; 2|]
let a2 = [|2; 1; 4; -2; 3; 4; -1|]
let a3 = [|2; 1; 4; -2; 3; 4|]

let print_prefix_sum (a: int array): unit = 
  let s = FlatArraySeq.seq_of_array a in
  let prefix_sum = FlatArraySeq.scan (+) 0 s in
  print_sequence string_of_int prefix_sum;
  print_newline ()

let _ = print_prefix_sum a1
let _ = print_prefix_sum a2
let _ = print_prefix_sum a3

let ss = FlatArraySeq.seq_of_array [|FlatArraySeq.seq_of_array a1; FlatArraySeq.seq_of_array a2; FlatArraySeq.seq_of_array a3|]
let _ = print_sequence string_of_int (FlatArraySeq.flatten ss)

let _ = print_newline ();;

let s = FlatArraySeq.seq_of_array [|0.; 1.;2.;3.;|];;

let l, r = FlatArraySeq.split s 2;;

let _ = print_sequence string_of_float l;;
let _ = print_newline ();;
let _ = print_sequence string_of_float r;;
let _ = print_newline ();;

let s = NestedArraySeq.tabulate (fun i -> i) 128;;

print_int (NestedArraySeq.reduce (+) 0 s);;
let _ = print_newline ();;

print_int (NestedArraySeq.map_reduce (fun i -> i mod 2) (+) 0 s);;
let _ = print_newline ();;
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

let s = FlatArraySeq.tabulate (fun _ -> 1) 128;;

let a, size = FlatArraySeq.build_fenwick_tree (+) 0 5 s;;

Array.iter (fun i -> print_string ((string_of_int i)^",")) a;;
print_newline ();;
print_int size;;
print_newline ();;
print_int (FlatArraySeq.reduce_alt (+) 0 s);;
print_newline ();;
print_sequence string_of_int (FlatArraySeq.scan_alt (+) 0 s);;
print_newline ();;

let list_seq = FlatArraySeq.tabulate (fun i -> [i]) 128;;
List.iter (fun i -> print_string ((string_of_int i)^", ")) (FlatArraySeq.reduce_alt (@) [] list_seq);;
print_newline ();;

let f_arr1: float array = Array_handler.get_uninitialized 100;;
let f_arr2: float array = Array.create_float 100;;
let f_arr3: float array = Array.make 100 0.0;;

print_int (Obj.tag (Obj.repr f_arr1));;
print_newline ();;
print_int (Obj.tag (Obj.repr f_arr2));;
print_newline ();;
print_int (Obj.tag (Obj.repr f_arr3));;
print_newline ();;
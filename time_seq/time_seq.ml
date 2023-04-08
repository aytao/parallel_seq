open Seq
open Sequence
open Time_seq_utils

(* let n = 11;;

   let rec kill_time i =
     if i = 0 then ()
     else
       (
         kill_time (i - 1);
         kill_time (i - 1)
       )

   let idle () = kill_time n

   let s = ParallelSeq.tabulate (fun x -> kill_time n; x) 1_000_000;;
*)

let gen_int_seq (n : int) : int NestedArraySeq.t =
  NestedArraySeq.tabulate (fun i -> i) n

let n = 10_000
let gen_weak () : int Weak.t = Weak.create n
let gen_norm () = Array.make n 0
let gen_float () = Array.create_float n
let ss = NestedArraySeq.tabulate (fun _ -> gen_int_seq n) n

let test () : int =
  NestedArraySeq.map_reduce (NestedArraySeq.reduce ( + ) 0) ( + ) 0 ss

(* let s = gen_int_seq n *)
let _ = print_endline (string_of_float (time (fun _ -> ignore (test ())) 100))
let _ = print_endline (string_of_int (test ()))
(* let _ = print_endline (string_of_float (time (fun _ -> ignore (gen_norm ())) 10))
   let _ = print_endline (string_of_float (time (fun _ -> ignore (gen_float ())) 10)) *)

(* let _ = print_int (Obj.size (Obj.repr (gen_float ()))) *)

(* let n_arr:nativeint array = Obj.magic (gen_norm ())

   print_endline "Hi"
   let i:int = Nativeint.to_int n_arr.(0)

   let _ = print_int i *)

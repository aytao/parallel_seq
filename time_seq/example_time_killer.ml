
open Seq
open Sequence


let n:int = 10_000_000;;

let g i =
  let x = ref 0 in
  for j = 0 to (100 - 1) do x := !x + 1 done;
  !x

let f ():int =
  let s = ParallelSeq.tabulate (fun i -> (i * i) + (g n) - (g n) - 1) n in
  let s' = ParallelSeq.map (fun i -> if i mod 5 = 0 then 1 else 0) s
  in
  ParallelSeq.reduce (+) 0 s'

let _ = print_endline (string_of_int (f ()));;

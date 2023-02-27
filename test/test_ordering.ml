open Seq
open Sequence

let n: int = 100;;

let check_ascending l =
  let rec aux l i =
    match l with
    | [] -> true
    | hd :: tl ->
      if hd != i then false
      else aux tl (i + 1)
  in
  aux l 0

let test_reduce_ordering n: bool =
  NestedArraySeq.tabulate (fun i -> [i]) n |>
  NestedArraySeq.reduce (@) [] |>
  check_ascending

let test_scan_ordering n: bool =
  NestedArraySeq.tabulate (fun i -> [i]) n |>
  NestedArraySeq.scan (@) [] |>
  NestedArraySeq.map check_ascending |>
  NestedArraySeq.reduce (&&) true

let _ = if not (test_scan_ordering n) then
  print_endline "Ordering inconsistent!"
else
  print_endline "Ordering consistent!"
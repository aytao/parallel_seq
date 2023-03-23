open Seq
open Sequence
open Quicksort

let n = 64000;;

let test_quicksort n =
  let s = S.tabulate (fun _ -> Random.float Float.max_float) n
    |> quicksort Float.compare in
  let check_less i =
    assert (Float.compare (S.nth s i) (S.nth s (i + 1)) <= 0);
    ()
  in
  ignore (S.tabulate check_less (S.length s - 1))

let _ = print_endline "Running quicksort test";;
let _ = test_quicksort n;;
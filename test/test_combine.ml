open Seq
open Sequence
open Sequence_provider

let n : int = Defaults.sequential_cutoff * Defaults.num_domains * 10

type interval = Empty | Interval of (int * int)

let string_of_interval (interval : interval) : string =
  match interval with
  | Empty -> "empty"
  | Interval (b, e) -> Printf.sprintf "(%d, %d)" b e

let singleton (i : int) : interval = Interval (i, i + 1)

let combine_intervals (i1 : interval) (i2 : interval) : interval =
  match (i1, i2) with
  | Empty, _ -> i2
  | _, Empty -> i1
  | Interval (b1, e1), Interval (b2, e2) ->
      if e1 != b2 then (
        Printf.eprintf "Intervals: (%d, %d), (%d, %d)\n" b1 e1 b2 e2;
        failwith "Invalid interval")
      else Interval (b1, e2)

let test_reduce_ordering n : unit =
  let interval = S.tabulate singleton n |> S.reduce combine_intervals Empty in
  match interval with
  | Empty -> assert (n = 0)
  | Interval (b, e) -> assert (b = 0 && e = n)

let test_scan_ordering n : unit =
  let check_interval i interval : unit =
    match interval with
    | Empty -> assert false
    | Interval (b, e) -> assert (b = 0 && e = i + 1)
  in
  S.tabulate singleton n
  |> S.scan combine_intervals Empty
  |> S.iteri check_interval

let _ = Printexc.record_backtrace true
let _ = print_endline "Running reduce test"
let _ = try test_reduce_ordering n with exn -> Printexc.print_backtrace stdout
let _ = print_endline "Running scan test"
let _ = test_scan_ordering n

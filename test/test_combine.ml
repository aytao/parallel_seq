open Parallel_seq

let n : int = Defaults.sequential_cutoff * Defaults.num_domains_total * 10

type interval = Empty | Interval of (int * int)

let singleton (i : int) : interval = Interval (i, i + 1)

let combine_intervals (i1 : interval) (i2 : interval) : interval =
  match (i1, i2) with
  | Empty, _ -> i2
  | _, Empty -> i1
  | Interval (b1, e1), Interval (b2, e2) ->
      if e1 <> b2 then (
        Printf.eprintf "Intervals: (%d, %d), (%d, %d)\n" b1 e1 b2 e2;
        failwith "Invalid interval")
      else Interval (b1, e2)

module Test (S : S) = struct
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

  let run_tests n : unit =
    let _ = test_reduce_ordering n in
    let _ = test_scan_ordering n in
    ()
end

module Parallel_tester = Test (Sequence_provider.Parallel)
module Sequential_tester = Test (Sequence_provider.Sequential)

let _ = Parallel_tester.run_tests n
let _ = Sequential_tester.run_tests n

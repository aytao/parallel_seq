open Seq
open Sequence_provider

let n : int = Defaults.sequential_cutoff * Defaults.num_domains * 10

let f (counter : int ref) (v : int) : unit =
  assert (v = !counter);
  counter := !counter + 1

module Test (S : Sequence.S) = struct
  let test_iter n : unit =
    let r = ref 0 in
    S.tabulate (fun i -> i) n |> S.iter (f r);
    assert (!r = n)

  let test_iteri n : unit =
    let r, r' = (ref 0, ref 0) in
    let s = S.tabulate (fun i -> n + i) n in
    S.iteri
      (fun i v ->
        f r' i;
        f r (v - n))
      s;
    assert (!r = n);
    assert (!r' = n)

  let run_tests n : unit =
    let _ = test_iter n in
    let _ = test_iteri n in
    ()
end

module ParallelTester = Test (ParallelS)
module SequentialTester = Test (SequentialS)

let _ = ParallelTester.run_tests n
let _ = SequentialTester.run_tests n
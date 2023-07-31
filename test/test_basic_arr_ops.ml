open Parallel_seq

let n : int = Defaults.sequential_cutoff * Defaults.num_domains_total

module Test (S : S) = struct
  let test_length n =
    let s = S.tabulate (fun i -> i) n in
    assert (S.length s = n)

  let test_empty () =
    let s = S.empty () in
    assert (S.length s = 0)

  let test_singleton n =
    let s = S.singleton n in
    assert (S.length s = 1);
    assert (S.nth s 0 = n)

  let test_nth n =
    let s = S.tabulate (fun i -> i) n in
    for i = 0 to n - 1 do
      assert (S.nth s i = i)
    done

  let run_tests n =
    let _ = test_length n in
    let _ = test_empty () in
    let _ = test_singleton n in
    let _ = test_nth n in
    ()
end

module Parallel_tester = Test (Sequence_provider.Parallel)
module Sequential_tester = Test (Sequence_provider.Sequential)

let _ = Parallel_tester.run_tests n
let _ = Sequential_tester.run_tests n

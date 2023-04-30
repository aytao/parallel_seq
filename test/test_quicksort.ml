open Seq
open Sequence_provider
open Quicksort

let n = 64000

module Test (S : Sequence.S) = struct
  module Sort = Quicksort (S)

  let test_quicksort n =
    let s =
      S.tabulate (fun _ -> Random.float Float.max_float) n
      |> Sort.sort Float.compare
    in
    let check_less i =
      assert (Float.compare (S.nth s i) (S.nth s (i + 1)) <= 0);
      ()
    in
    ignore (S.tabulate check_less (S.length s - 1))

  let run_tests n = test_quicksort n
end

module ParallelTester = Test (ParallelS)
module SequentialTester = Test (SequentialS)

let _ = ParallelTester.run_tests n
let _ = SequentialTester.run_tests n

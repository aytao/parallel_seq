open Parallelseq
open Sequence
open Sequence_provider

let n : int = (Defaults.sequential_cutoff * Defaults.num_domains_total) + 1
let weak_arr = Weak.create n

let f i =
  let v = Some i in
  Weak.set weak_arr i (Some v);
  v

module Test (S : Sequence.S) = struct
  let is_all_full wa =
    S.tabulate (fun i -> Weak.check wa i) (Weak.length wa)
    |> S.reduce ( && ) true

  let run_tests n =
    let s = S.tabulate f n in
    assert (is_all_full weak_arr);
    Gc.full_major ();
    assert (is_all_full weak_arr);
    (* Prevent compiler from throwing away sequence s *)
    ignore
      (S.map_reduce (fun x -> match x with None -> 0 | Some v -> v) ( + ) 0 s)
end

module ParallelTester = Test (ParallelS)
module SequentialTester = Test (SequentialS)

let _ = ParallelTester.run_tests n
let _ = SequentialTester.run_tests n
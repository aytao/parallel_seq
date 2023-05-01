open Parallelseq
open Sequence
open Sequence_provider

let n : int = (Defaults.sequential_cutoff * Defaults.num_domains_total) + 1
let num_trials : int = 1_000

module Test (S : Sequence.S) = struct
  let assert_val_is_index s = S.iteri (fun i x -> assert (x = i)) s

  let test_cons n =
    let s = S.tabulate (fun i -> i + 1) (n - 1) |> S.cons 0 in
    assert_val_is_index s

  let test_tabulate n =
    let s = S.tabulate (fun i -> i) n in
    assert_val_is_index s

  let test_repeat n =
    let ptr = Some 0 in
    S.repeat ptr n |> S.iter (fun x -> assert (x == ptr))

  let test_append n =
    let s = S.tabulate (fun i -> i) n in
    let s' = S.tabulate (fun i -> i + n) n in
    let appended = S.append s s' in
    assert_val_is_index appended

  let test_seq_of_array n =
    let a = Array.init n (fun i -> i) in
    let s = S.seq_of_array a in
    assert_val_is_index s;
    let _ = a.(0) <- -1 in
    assert_val_is_index s

  let test_array_of_seq n =
    let s = S.tabulate (fun i -> i) n in
    let a = S.array_of_seq s in
    Array.iteri (fun i x -> assert (x = i)) a;
    let _ = a.(0) <- -1 in
    assert_val_is_index s

  let test_zip n =
    let s = S.tabulate (fun i -> i) n in
    let s' = S.tabulate (fun i -> i + n) n in
    let z = S.zip (s, s') in
    S.iteri (fun i (x, x') -> assert (x = i && x' = i + n)) z

  let test_split n =
    let s = S.tabulate (fun i -> i) n in

    let check_split idx : unit =
      let check_section section offset : unit =
        let _ =
          S.tabulate
            (fun idx -> assert (S.nth section idx = idx + offset))
            (S.length section)
        in
        ()
      in
      let l, r = S.split s idx in
      assert (S.length l = idx);
      assert (S.length r = n - idx);
      check_section l 0;
      check_section r idx
    in
    check_split 0;
    for _ = 1 to num_trials do
      let idx = Random.full_int (n + 1) in
      check_split idx
    done;
    check_split n;
    ()

  let test_map n =
    S.tabulate (fun i -> i) n
    |> S.map (fun x -> x * 2)
    |> S.iteri (fun i x -> assert (x = i * 2))

  let run_tests n =
    let _ = test_cons n in
    let _ = test_tabulate n in
    let _ = test_repeat n in
    let _ = test_append n in
    let _ = test_seq_of_array n in
    let _ = test_array_of_seq n in
    let _ = test_zip n in
    let _ = test_split n in
    let _ = test_map n in
    ()
end

module ParallelTester = Test (ParallelS)
module SequentialTester = Test (SequentialS)

let _ = ParallelTester.run_tests n
let _ = SequentialTester.run_tests n
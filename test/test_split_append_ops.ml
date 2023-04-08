open Seq
open Sequence

let n : int = (Defines.sequential_cutoff * Defines.num_domains) + 1
let num_trials : int = 1_000

let test_split n =
  let s = S.tabulate (fun i -> i) n in

  let check_split idx : unit =
    let check_section section offset : unit =
      let _ =
        S.tabulate
          (fun idx -> assert (idx + offset = S.nth section idx))
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

let test_cons n =
  let s = S.tabulate (fun i -> i + 1) (n - 1) |> S.cons 0 in
  let correct_vals =
    S.tabulate (fun i -> S.nth s i = i) n |> S.reduce ( && ) true
  in
  assert correct_vals

let _ = print_endline "Running split test"
let _ = test_split n
let _ = print_endline "Running cons test"
let _ = test_cons n

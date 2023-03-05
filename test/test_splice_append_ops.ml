open Seq
open Sequence

let n: int = (Defines.sequential_cutoff * Defines.num_domains) + 1;;

let test_split n =
  let s = S.tabulate (fun i -> i) n in
  let s_lr = S.tabulate (fun i -> S.split s i) (n + 1) in
  
  let check_split idx: unit =
    let check_section section offset: unit =
      let _ = S.tabulate (fun idx -> assert (idx + offset = S.nth section idx)) (S.length section) in
      ()
    in
    let l,r = S.nth s_lr idx in
    assert (S.length l = idx);
    assert (S.length r = n - idx);
    check_section l 0;
    check_section r idx
  in
  let _ = S.tabulate check_split n in
  ()

let _ = print_endline "Running split test";;
let _ = test_split n;;
open Seq
open Sequence

let n: int = Defines.sequential_cutoff * Defines.num_domains * 10;;

let f (counter: int ref) (v: int): unit =
  assert (v = !counter);
  counter := !counter + 1

let test_iter n: unit =
  let r = ref 0 in
  S.tabulate (fun i -> i) n |>
  S.iter (f r);
  assert (!r = n)

let test_iteri n: unit =
  let r = ref 0 in
  let s = S.tabulate (fun i -> n + i) n in
  S.iteri (fun _ v -> f r (v - n)) s;
  assert (!r = n);

  let r' = ref 0 in
  S.iteri (fun i _ -> f r' i) s;
  assert (!r' = n)

let _ = test_iter n;;
let _ = test_iteri n;;

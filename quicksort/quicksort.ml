open Seq
open Sequence

type 'a cmp = 'a -> 'a -> int

let insertion_cutoff = Defines.sequential_cutoff

let insertion_sort (cmp: 'a cmp) (s: 'a S.t): ('a S.t) =
  let arr = S.array_of_seq s in
  let swap i j =
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp;
  in
  let rec iter i =
    if i = Array.length arr then ()
    else
      let rec insert i = 
        if i != 0 && cmp arr.(i) arr.(i - 1) < 0 then (
          swap i (i - 1); insert (i - 1)
        ) else ()
      in
      insert i;
      iter (i + 1)
  in
  iter 0;
  S.seq_of_array arr

let get_median (s: 'a S.t): 'a =
  S.nth s (S.length s / 2)

let partition (cmp: 'a cmp) (s: 'a S.t) (p: 'a): ('a S.t * 'a S.t * 'a S.t) =
  let lt = S.filter (fun x -> cmp x p < 0) s in
  let eq = S.filter (fun x -> cmp x p = 0) s in
  let gt = S.filter (fun x -> cmp x p > 0) s in
  lt, eq, gt

let rec quicksort (cmp: 'a cmp) (s: 'a S.t): ('a S.t) =
  if S.length s <= insertion_cutoff then insertion_sort cmp s else
  let lt, eq, gt = partition cmp s (get_median s) in
  S.append (S.append (quicksort cmp lt) eq) (quicksort cmp gt)
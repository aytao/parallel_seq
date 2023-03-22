open Seq
open Sequence

type 'a cmp = 'a -> 'a -> int

let get_median (s: 'a S.t): 'a =
  S.nth s (S.length s / 2)

let partition (cmp: 'a cmp) (s: 'a S.t) (p: 'a): ('a S.t * 'a S.t * 'a S.t) =
  let lt = S.filter (fun x -> cmp x p < 0) s in
  let eq = S.filter (fun x -> cmp x p = 0) s in
  let gt = S.filter (fun x -> cmp x p > 0) s in
  lt, eq, gt

let rec quicksort (cmp: 'a cmp) (s: 'a S.t): ('a S.t) =
  if S.length s <= 1 then s else
  let lt, eq, gt = partition cmp s (get_median s) in
  S.append (S.append (quicksort cmp lt) eq) (quicksort cmp gt)
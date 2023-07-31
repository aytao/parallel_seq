open Parallel_seq

let n : int = (Defaults.sequential_cutoff * Defaults.num_domains_total) + 1

module Test (S : S) = struct
  let test_map_reduce n =
    let s = S.tabulate (fun _ -> Random.bits ()) n in
    let mpr = S.map_reduce (fun x -> x / 2) ( + ) 0 s in
    let mpr' = S.map (fun x -> x / 2) s |> S.reduce ( + ) 0 in
    assert (mpr = mpr')

  let test_flatten n =
    let get_random_partition n max_size : int S.t S.t =
      let rec aux acc n : (int * int) list =
        if n <= max_size then (n, 0) :: acc
        else
          let size = Random.int max_size in
          let remaining = n - size in
          aux ((size, remaining) :: acc) remaining
      in
      let sizes = aux [] n |> Array.of_list |> S.seq_of_array in
      S.map (fun (size, tot) -> S.tabulate (fun i -> i + tot) size) sizes
    in
    let s = get_random_partition n 5 in
    let flat = S.flatten s in
    S.iteri (fun i x -> assert (x = i)) flat

  let test_filter n =
    let skip = 3 in
    let s = S.tabulate (fun i -> i) (n * skip) in
    let s' = S.filter (fun x -> x mod skip = 0) s in
    assert (S.length s' = n);
    S.iteri (fun i v -> assert (v = i * 3)) s'

  let run_tests n =
    let _ = test_map_reduce n in
    let _ = test_flatten n in
    let _ = test_filter n in
    ()
end

module Parallel_tester = Test (Sequence_provider.Parallel)
module Sequential_tester = Test (Sequence_provider.Sequential)

let _ = Parallel_tester.run_tests n
let _ = Sequential_tester.run_tests n

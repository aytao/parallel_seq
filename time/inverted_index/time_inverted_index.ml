open Inverted_index

let index (seq_type : Parallel_seq.seq_type) filename =
  let num_chunks =
    match seq_type with
    | Sequential -> 1
    | Parallel { pool; sequential_cutoff = _ } ->
        Domainslib.Task.get_num_domains pool
  in
  let open Indexer ((val Parallel_seq.get_module seq_type)) in
  Time_test.time
    (fun filename -> make_index ~num_chunks filename |> ignore)
    filename

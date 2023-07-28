open Parallelseq
open Inverted_index

let index seq_type filename =
  let open Indexer ((val Parallelseq.Sequence.get_module seq_type)) in
  Time_test.time (fun filename -> make_index filename |> ignore) filename

let ensure_no_pool test_name =
  if Domainslib.Task.lookup_pool "parallel_seq_pool" |> Option.is_some then
    failwith ("Test " ^ test_name ^ " must be called with [-f]")

let sequential _seq_mod ?n () =
  ensure_no_pool "scan_sequential";
  let n = Option.value ~default:100000 n in
  let arr = Array.init n (fun x -> x) in
  Time_test.time
    (fun () ->
      let copy = Array.copy arr in
      for i = 1 to Array.length copy - 1 do
        copy.(i) <- copy.(i - 1) + copy.(i)
      done)
    ()

let copy _seq_mod ?n () =
  ensure_no_pool "array_copy";
  let n = Option.value ~default:100000 n in
  let arr = Array.init n (fun x -> x) in
  Time_test.time (fun () -> Array.copy arr |> ignore) ()

let domainslib _seq_mod ?n () =
  ensure_no_pool "scan_domainslib";
  let n = Option.value ~default:100000 n in
  let pool =
    Domainslib.Task.setup_pool
      ~num_domains:(Parallelseq.Defaults.num_domains_total - 1)
      ()
  in
  let arr = Array.init n (fun x -> [ x ]) in
  Time_test.time
    (fun () ->
      Domainslib.Task.run pool (fun () ->
          Domainslib.Task.parallel_scan pool ( @ ) arr |> ignore))
    ()

let parallel_seq seq_mod ?n () =
  let open (val seq_mod : Parallelseq.Sequence.S) in
  let n = Option.value ~default:100000 n in
  let s = tabulate (fun x -> [ x ]) n in
  Time_test.time (fun () -> scan ( @ ) [] s |> ignore) ()

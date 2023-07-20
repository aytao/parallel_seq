let rec spin x = match x with 0 -> () | x -> spin (x - 1)

let sequential _seq_mod ?n () =
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
  let n = Option.value ~default:100000 n in
  let arr = Array.init n (fun x -> x) in
  Time_test.time (fun () -> Array.copy arr |> ignore) ()

let parallel _seq_mod ?n () =
  let n = Option.value ~default:100000 n in
  let pool =
    Domainslib.Task.setup_pool
      ~num_domains:(Parallelseq.Defaults.num_domains_total - 1)
      ()
  in
  let arr = Array.init n (fun x -> x) in
  Time_test.time
    (fun () ->
      Domainslib.Task.run pool (fun () ->
          Domainslib.Task.parallel_scan pool ( + ) arr |> ignore))
    ()

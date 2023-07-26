let ensure_no_pool test_name =
  if Domainslib.Task.lookup_pool Time_test.time_test_pool_name |> Option.is_some
  then failwith ("Test " ^ test_name ^ " must be called with [-f]")

let get_pool test_name =
  match Domainslib.Task.lookup_pool Time_test.time_test_pool_name with
  | None -> failwith ("Test " ^ test_name ^ " cannot be called with [-f]")
  | Some pool -> pool

let sequential (_seq_type, n) =
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

let array_copy (_seq_type, n) =
  ensure_no_pool "array_copy";
  let n = Option.value ~default:100000 n in
  let arr = Array.init n (fun x -> x) in
  Time_test.time (fun () -> Array.copy arr |> ignore) ()

module Int_map = Map.Make (Int)

let domainslib (_seq_type, n) =
  let pool = get_pool "scan_domainslib" in
  let n = Option.value ~default:1000000000 n in
  let arr = Array.init n (fun x -> x) in
  Time_test.time
    (fun () ->
      Domainslib.Task.run pool (fun () ->
          Domainslib.Task.parallel_scan pool ( + ) arr |> ignore))
    ()

let in_place (_seq_type, n) =
  let pool = get_pool "scan_in_place" in
  let num_domains = Domainslib.Task.get_num_domains pool in
  let n = Option.value ~default:1000000000 n in
  let arr = Array.init n (fun x -> x) in
  Time_test.time
    (fun () ->
      Domainslib.Task.run pool (fun () ->
          Mutating_scan.parallel_scan pool num_domains ( + ) arr |> ignore))
    ()

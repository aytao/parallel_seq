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
  let n = Option.value ~default:1000000000 n in
  let arr = Array.init n (fun x -> x) in
  Time_test.time
    (fun () ->
      Domainslib.Task.run pool (fun () ->
          Mutating_scan.parallel_scan pool ( + ) arr |> ignore))
    ()

type interval = Empty | Interval of (int * int)

let string_of_interval (interval : interval) : string =
  match interval with
  | Empty -> "empty"
  | Interval (b, e) -> Printf.sprintf "(%d, %d)" b e

let singleton (i : int) : interval = Interval (i, i + 1)

let combine_intervals (i1 : interval) (i2 : interval) : interval =
  match (i1, i2) with
  | Empty, _ -> i2
  | _, Empty -> i1
  | Interval (b1, e1), Interval (b2, e2) ->
      if e1 != b2 then (
        Printf.eprintf "Intervals: (%d, %d), (%d, %d)\n" b1 e1 b2 e2;
        failwith "Invalid interval")
      else Interval (b1, e2)

let test_mutating_scan (_seq_type, n) =
  let check_interval i interval =
    match interval with
    | Empty -> assert false
    | Interval (b, e) -> assert (b = 0 && e = i + 1)
  in
  let pool = get_pool "test_mutating_scan" in
  let n = Option.value ~default:1000000000 n in
  let arr = Array.init n singleton in
  Time_test.time
    (fun () ->
      Domainslib.Task.run pool (fun () ->
          Mutating_scan.parallel_scan pool combine_intervals arr
          |> Array.iteri check_interval;
          print_endline "All good"))
    ()

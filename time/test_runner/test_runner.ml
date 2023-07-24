let tests : (string * Time_test.t) list =
  [
    ("scan_sequential", Time_scan.sequential);
    ("scan_domainslib", Time_scan.domainslib);
    ("array_copy", Time_scan.copy);
    ("scan_parallel_seq", Time_scan.parallel_seq);
  ]

let run test_name config =
  let test = List.find_opt (fun (name, _test) -> name = test_name) tests in
  match test with
  | None -> failwith ("No test named \"" ^ test_name ^ "\" found")
  | Some (_name, test) -> test config

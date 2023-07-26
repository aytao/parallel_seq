let tests : (string * Time_test.t) list =
  [
    ("scan_sequential", Time_scan.sequential);
    ("scan_domainslib", Time_scan.domainslib);
    ("array_copy", Time_scan.array_copy);
    ("scan_in_place", Time_scan.in_place);
  ]

let run test_name config =
  let test = List.find_opt (fun (name, _test) -> name = test_name) tests in
  match test with
  | None -> failwith ("No test named \"" ^ test_name ^ "\" found")
  | Some (_name, test) -> test config

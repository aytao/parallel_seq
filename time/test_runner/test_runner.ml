let tests : (string * Time_test.t) list =
  [
    ("scan_sequential", With_length Time_scan.sequential);
    ("scan_domainslib", With_length Time_scan.domainslib);
    ("array_copy", With_length Time_scan.array_copy);
    ("scan_in_place", With_length Time_scan.in_place);
    ("block_matrix_mul", With_length Time_matrix_mul.block);
  ]

let run ?filename ?n seq_type test_name =
  let test = List.find_opt (fun (name, _test) -> name = test_name) tests in
  match test with
  | None -> failwith ("No test named \"" ^ test_name ^ "\" found")
  | Some (_name, With_length test) -> test seq_type n
  | Some (_name, With_file test) -> test seq_type (Option.get filename)

let tests : (string * Time_test.t) list =
  [
    ("array_sequential", Array_init.sequential);
    ("array_parallel", Array_init.parallel);
  ]

let run seq_mod test_name ?n () =
  let test = List.find_opt (fun (name, _test) -> name = test_name) tests in
  match test with
  | None -> failwith ("No test named \"" ^ test_name ^ "\" found")
  | Some (_name, test) -> test seq_mod ?n ()

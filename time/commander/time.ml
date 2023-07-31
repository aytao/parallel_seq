open Domainslib
open Parallel_seq

let domains_arg = ref Defaults.num_domains_total
let cutoff_arg = ref Defaults.sequential_cutoff
let force_sequential_arg = ref false
let test_name = ref ""
let n_arg = ref (-1)
let filename_arg = ref None
let usage_str = "./bin/time.exe TESTNAME [OPTION...]"

let speclist =
  [
    ( "-num_domains",
      Arg.Set_int domains_arg,
      "Manually set the number of domains used" );
    ("-s", Arg.Set_int cutoff_arg, "Manually adjust the sequential cutoff");
    ("-f", Arg.Set force_sequential_arg, "Force sequencial implementation");
    ("-n", Arg.Set_int n_arg, "Set the length of the array (for array tests)");
    ( "-i",
      Arg.String (fun filename -> filename_arg := Some filename),
      "Set the index file to be used" );
  ]

let () = Arg.parse speclist (fun test -> test_name := test) usage_str;;

if !domains_arg <= 0 then failwith "Must have at least one domain"
else if !cutoff_arg <= 0 then failwith "Sequential cutoff must be positive"

let (seq_type : seq_type) =
  if !force_sequential_arg then Sequential
  else
    let pool =
      Task.setup_pool ~name:Time_test.time_test_pool_name
        ~num_domains:(!domains_arg - 1) ()
    in
    Parallel { pool; sequential_cutoff = !cutoff_arg }

let n = if !n_arg < 0 then None else Some !n_arg
let filename = !filename_arg

let _ =
  Test_runner.run ?filename ?n seq_type !test_name |> print_float;
  print_newline ()

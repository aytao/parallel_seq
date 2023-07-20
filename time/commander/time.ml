open Domainslib
open Parallelseq
open Sequence
open Arg

let domains_arg = ref Defaults.num_domains_total
let cutoff_arg = ref Defaults.sequential_cutoff
let force_sequential_arg = ref false
let test_name = ref ""
let n_arg = ref (-1)
let usage_str = "./bin/time.exe TESTNAME [OPTION...]"

let speclist =
  [
    ( "-num_domains",
      Arg.Set_int domains_arg,
      "Manually set the number of domains used" );
    ("-s", Arg.Set_int cutoff_arg, "Manually adjust the sequential cutoff");
    ("-f", Arg.Set force_sequential_arg, "Force sequencial implementation");
    ("-n", Arg.Set_int n_arg, "Set the size of the test ran");
  ]

let () = Arg.parse speclist (fun test -> test_name := test) usage_str;;

if !domains_arg <= 0 then failwith "Must have at least one domain"
else if !cutoff_arg <= 0 then failwith "Sequential cutoff must be positive"

let seq_type =
  if !force_sequential_arg then Sequential
  else
    let pool = Task.setup_pool ~num_domains:(!domains_arg - 1) () in
    Parallel (pool, !cutoff_arg)

let m = Sequence.get_module seq_type

module S = (val m : Sequence.S)

let n = if !n_arg < 0 then None else Some !n_arg

let _ =
  Test_runner.run (module S) !test_name ?n () |> print_float;
  print_newline ()

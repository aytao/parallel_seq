open Arg

let domains_arg = ref (Domain.recommended_domain_count ())
let cutoff_arg = ref 100
let force_sequential_arg = ref false

let speclist =
  [
    ( "-num_domains",
      Arg.Set_int domains_arg,
      "Manually set the number of domains used" );
    ("-s", Arg.Set_int cutoff_arg, "Manually adjust the sequential cutoff");
    ("-f", Arg.Set force_sequential_arg, "Force sequencial implementation");
  ]

let _ = Arg.parse speclist (fun _ -> ()) "[num_domains] [s]";;

if !domains_arg <= 0 then failwith "Must have at least one domain"
else if !cutoff_arg <= 0 then failwith "Sequential cutoff must be positive"

let num_domains = !domains_arg
let sequential_cutoff = !cutoff_arg
let force_sequential = !force_sequential_arg

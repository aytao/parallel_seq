open Domainslib
open Parallelseq
open Sequence
open Arg

let domains_arg = ref Defaults.num_domains_total
let cutoff_arg = ref Defaults.sequential_cutoff
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

let seq_type =
  if !force_sequential_arg then Sequential
  else
    let pool = Task.setup_pool ~num_domains:(!domains_arg - 1) () in
    Parallel { pool; sequential_cutoff = !cutoff_arg }

let m = Sequence.get_module seq_type

module S = (val m : Sequence.S)

let time (f : 'a -> 'b) (x : 'a) : 'b * float =
  let t = Unix.gettimeofday () in
  let fx = f x in
  let t' = Unix.gettimeofday () in
  (fx, t' -. t)

open Arg

let n = 11;;

let domains_arg = ref (Domain.recommended_domain_count ())

let speclist = ["-num_domains", (Arg.Set_int domains_arg), "Manually set the number of domains used"];;

let _ = Arg.parse speclist (fun _ -> ()) "[num_domains]"

(* TODO: Ensure value is non-negative *)

let num_domains = !domains_arg

let sequential_cutoff = 1000

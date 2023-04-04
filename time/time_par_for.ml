open Seq
open Domainslib

let n = 100000;;

let pool = Task.setup_pool ~num_domains:(Defines.num_domains - 1) ()

let run (task: 'a Task.task): 'a =
  Task.run pool task

let parallel_for (n: int) (body: (int -> unit)): unit =
  let chunk_size = min Defines.sequential_cutoff (n / (Task.get_num_domains pool)) in
  run (fun _ ->
    Task.parallel_for ~chunk_size:chunk_size ~start:0 ~finish:(n - 1) ~body:body pool  
  )

let slow_divide n i =
  let i = Random.float 1. in
  let rec aux n acc num =
    if acc > n then num else aux n (acc +. (i)) (num + 1)
  in
  ignore (aux n 1. 0)

let s, elapsed_time = Time_utils.time (parallel_for n) (slow_divide (1000.));;

Printf.printf "%f\n" elapsed_time

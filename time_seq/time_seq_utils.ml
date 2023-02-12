let time (f: unit -> unit) (num_trials: int): float =
  let start = Sys.time () in
  for i = 1 to num_trials do f () done;
  let elapsed = (Sys.time ()) -. start in
  elapsed /. (float_of_int num_trials)
open Seq
open Sequence

let n = 11;;

let rec kill_time i =
  if i = 0 then ()
  else
    (
      kill_time (i - 1);
      kill_time (i - 1)
    )

let idle () = kill_time n

let s = ParallelSeq.tabulate (fun x -> kill_time n; x) 1_000_000;;
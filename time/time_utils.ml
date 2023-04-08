let time (f : 'a -> 'b) (x : 'a) : 'b * float =
  let t = Unix.gettimeofday () in
  let fx = f x in
  let t' = Unix.gettimeofday () in
  (fx, t' -. t)

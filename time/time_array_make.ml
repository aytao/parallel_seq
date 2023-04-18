let n = 1000000000
let array_init n : int array = Array.make (-1) n
let _, elapsed_time = Time_utils.time array_init n
let _ = Printf.printf "%f\n" elapsed_time
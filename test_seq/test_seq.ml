open Seq
open Sequence
open Utils

let s = S.tabulate (fun i -> i) 1000;;

let evens = S.filter (fun x -> x mod 2 == 0) s;;

Utils.print_sequence string_of_int evens;;
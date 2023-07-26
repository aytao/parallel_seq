open Domainslib.Task

val parallel_scan : pool -> int -> ('a -> 'a -> 'a) -> 'a array -> 'a array

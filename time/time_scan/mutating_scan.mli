open Domainslib.Task

val parallel_scan : pool -> ('a -> 'a -> 'a) -> 'a array -> 'a array

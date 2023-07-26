open Domainslib.Task

let parallel_scan pool num_domains op elements =
  let n = Array.length elements in
  let p = min (n - 1) num_domains in
  let scan_part op elements start finish =
    assert (Array.length elements > finish - start);
    for i = start + 1 to finish do
      elements.(i) <- op elements.(i - 1) elements.(i)
    done
  in
  if p < 2 then (
    (* Do a sequential scan when number of domains or array's length is less
       than 2 *)
    scan_part op elements 0 (n - 1);
    elements)
  else
    let add_offset op elements offset start finish =
      assert (Array.length elements > finish - start);
      for i = start to finish do
        elements.(i) <- op offset elements.(i)
      done
    in

    parallel_for pool ~chunk_size:1 ~start:0 ~finish:(p - 1) ~body:(fun i ->
        let s = i * n / p in
        let e = ((i + 1) * n / p) - 1 in
        scan_part op elements s e);

    let x = ref elements.((n / p) - 1) in
    for i = 2 to p do
      let ind = (i * n / p) - 1 in
      x := op !x elements.(ind);
      elements.(ind) <- !x
    done;

    parallel_for pool ~chunk_size:1 ~start:1 ~finish:(p - 1) ~body:(fun i ->
        let s = i * n / p in
        let e = ((i + 1) * n / p) - 2 in
        let offset = elements.(s - 1) in
        add_offset op elements offset s e);

    elements

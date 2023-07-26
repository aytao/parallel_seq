open Domainslib.Task

let parallel_scan pool num_domains op elements =
  let n = Array.length elements in
  let p = min (n - 1) num_domains in
  let prefix_s = Array.copy elements in
  let scan_part op elements prefix_sum start finish =
    assert (Array.length elements > finish - start);
    for i = start + 1 to finish do
      prefix_sum.(i) <- op prefix_sum.(i - 1) elements.(i)
    done
  in
  if p < 2 then (
    (* Do a sequential scan when number of domains or array's length is less
       than 2 *)
    scan_part op elements prefix_s 0 (n - 1);
    prefix_s)
  else
    let add_offset op prefix_sum offset start finish =
      assert (Array.length prefix_sum > finish - start);
      for i = start to finish do
        prefix_sum.(i) <- op offset prefix_sum.(i)
      done
    in

    parallel_for pool ~chunk_size:1 ~start:0 ~finish:(p - 1) ~body:(fun i ->
        let s = i * n / p in
        let e = ((i + 1) * n / p) - 1 in
        scan_part op elements prefix_s s e);

    let x = ref prefix_s.((n / p) - 1) in
    for i = 2 to p do
      let ind = (i * n / p) - 1 in
      x := op prefix_s.(ind) !x;
      prefix_s.(ind) <- !x
    done;

    parallel_for pool ~chunk_size:1 ~start:1 ~finish:(p - 1) ~body:(fun i ->
        let s = i * n / p in
        let e = ((i + 1) * n / p) - 2 in
        let offset = prefix_s.(s - 1) in
        add_offset op prefix_s offset s e);

    prefix_s

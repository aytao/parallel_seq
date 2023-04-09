type 'a node = { v : 'a; mutable next : 'a node option }
type 'a falist = { front : 'a node; back : 'a node }

let singleton (x : 'a) : 'a falist =
  let node = { v = x; next = None } in
  { front = node; back = node }

let cons (x : 'a) ({ front; back } : 'a falist) : 'a falist =
  let node = { v = x; next = Some front } in
  { front = node; back }

let back_cons (x : 'a) ({ front; back } : 'a falist) : 'a falist =
  let new_back = { v = x; next = None } in
  back.next <- Some new_back;
  { front; back = new_back }

let append ({ front = f1; back = b1 } : 'a falist)
    ({ front = f2; back = b2 } : 'a falist) : 'a falist =
  b1.next <- Some f2;
  { front = f1; back = b2 }

let iter (f : 'a -> unit) ({ front } : 'a falist) =
  let rec aux n =
    f n.v;
    match n.next with Some node -> aux node | None -> ()
  in
  aux front

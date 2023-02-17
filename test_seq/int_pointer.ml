type thing = Foo of string | Bar of int

module WeakPointer = struct
  type 'a t = {
    weakArr: 'a Weak.t;
  }

  let create (x: 'a): 'a t =
    let weakArr = Weak.create 1 in
    Weak.set weakArr 0 (Some x);
    {weakArr}
  
  let check (x: 'a t): bool =
    Weak.check x.weakArr 0
end

let f (na: int array) i : thing WeakPointer.t =
  let w = Bar i in
  let na_repr = Obj.repr na in
  let bar_repr = Obj.repr w in
  Obj.set_field na_repr 0 bar_repr;
  WeakPointer.create w

let norm_arr: int array = [|0|];;


let wp = f norm_arr 10

let _ = if WeakPointer.check wp then
  print_endline "full"
else
  print_endline "empty"
;;

Gc.full_major ();;

let _ = if WeakPointer.check wp then
  print_endline "full"
else
  print_endline "empty"
;;

norm_arr.(0) <- 0;;
Gc.full_major ();;

let _ = if WeakPointer.check wp then
  print_endline "full"
else
  print_endline "empty"
;;
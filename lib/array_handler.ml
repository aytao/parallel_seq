external get_memset_array : int -> 'a array = "caml_make_uninitialized_vect"

let empty_int_array (len : int) : 'a array = Obj.magic (Array.make len (-1))
let get_uninitialized = get_memset_array

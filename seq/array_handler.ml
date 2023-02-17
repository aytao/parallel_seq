let empty_int_array (len: int): int array =
  Array.make len 0

let get_uninitialized (len: int): 'a array =
  Obj.magic (empty_int_array len)
  
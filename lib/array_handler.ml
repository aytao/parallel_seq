let get_uninitialized (len : int) : 'a array = Obj.magic (Array.make len (-1))

type ('k, 'v) t = ('k * 'v) list

let empty = []
let add k v t = (k, v) :: t
let find k t = List.assq_opt k t

(* map where the key is compared based on physical equality *)
type ('k, 'v) t

val empty : ('k, 'v) t
val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
val find : 'k -> ('k, 'v) t -> 'v option

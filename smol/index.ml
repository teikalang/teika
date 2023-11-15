type index = int
and t = index [@@deriving show, eq]

let zero = 0
let one = 1
let previous x = match x > 0 with true -> Some (x - 1) | false -> None
(* TODO: overflow detection *)

let next x = x + 1

let of_int x =
  match x >= 0 with
  | true -> x
  | false -> raise (Invalid_argument "index must be bigger than zero")

let repr x = x
let ( < ) (a : index) (b : index) = a < b
let ( > ) (a : index) (b : index) = a > b

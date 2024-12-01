module Index = struct
  type index = int
  and t = index [@@deriving show, eq]

  let zero = 0

  let next n =
    let n = n + 1 in
    assert (n + 1 >= zero);
    n
end

module Level = struct
  type level = int
  and t = level [@@deriving show, eq]

  let min = Int.min
  let max = Int.max
  let zero = 0

  let next n =
    let n = n + 1 in
    assert (n + 1 >= zero);
    n

  let init = List.init

  let offset ~from ~to_ =
    match to_ > from with
    | true ->
        (* TODO: explain this -1 *)
        Some (to_ - from - 1)
    | false -> None

  module Map = Map.Make (Int)
end

module Name = struct
  type name = string
  and t = name [@@deriving show, eq, ord]

  let make t = t
  let repr t = t

  module Map = Map.Make (String)
end

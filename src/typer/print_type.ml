open Type
open Type_utils

(* TODO: is this function the best that I can do? *)

let rec to_string acc int =
  let diff = int mod 26 in
  let char = Char.chr (97 + diff) in
  let acc = char :: acc in
  let next = int / 26 in
  if next > 0 then to_string acc (next - 1) else String.of_seq (List.to_seq acc)

let to_string int = to_string [] int

let rec pp_type next_name vars fmt type_ =
  let pp_type fmt type_ = pp_type next_name vars fmt type_ in
  let fprintf s = Format.fprintf fmt s in
  let name var =
    match List.find_opt (fun (key, _name) -> same key var) !vars with
    | Some (_key, name) -> name
    | None ->
        let id = !next_name in
        let name = to_string id in

        next_name := id + 1;
        vars := (var, name) :: !vars;
        name
  in

  match desc type_ with
  | T_weak_var -> fprintf "_%s" (name type_)
  | T_bound_var _ -> fprintf "%s" (name type_)
  | T_forall { forall; body } ->
      let vars = forall_vars ~forall body in
      let vars = List.rev vars in
      let vars = List.map name vars |> String.concat " " in
      fprintf "forall %s. %a" vars pp_type body
  | T_arrow { param; return } ->
      let parens =
        match desc param with
        | T_weak_var | T_bound_var _ -> false
        | T_forall _ | T_arrow _ -> true
        | T_link _ -> assert false
      in
      if parens then fprintf "(%a) -> %a" pp_type param pp_type return
      else fprintf "%a -> %a" pp_type param pp_type return
  | T_link _ -> assert false

let with_pp_type f =
  let next_name = ref 0 in
  let vars = ref [] in
  let pp_type = pp_type next_name vars in
  f pp_type

let pp_type fmt type_ = pp_type (ref 0) (ref []) fmt type_

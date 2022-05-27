open Utils
open Type
open Helpers
open Format

(* TODO: is this function the best that I can do? *)

let rec to_string acc int =
  let diff = int mod 26 in
  let char = Char.chr (65 + diff) in
  let acc = char :: acc in
  let next = int / 26 in
  if next > 0 then to_string acc (next - 1) else String.of_seq (List.to_seq acc)

let to_string int = to_string [] int

(* TODO: print link optionally *)
(* TODO: print rank optionally *)
type ctx = {
  debug : bool;
  mutable id : int;
  mutable name : int;
  mutable types : (type_, string) Mem_map.t;
}

let new_ctx ~debug = { debug; id = 0; name = 0; types = Mem_map.empty }

let new_name ctx =
  let name = ctx.name in
  ctx.name <- name + 1;
  to_string name

let register_name ctx type_ =
  let id = ctx.id in
  ctx.id <- id + 1;

  let name =
    match desc type_ with
    | T_forall _ | T_arrow _ | T_struct _ | T_type _ -> sprintf "[%d]" id
    | T_var var ->
        (* TODO: use bound name *)
        let prefix = match var with Weak _ -> "_" | Bound _ -> "" in
        let suffix =
          if ctx.debug then
            match var with
            | Weak _ -> sprintf "[%d]" id
            | Bound { forall; name = _ } ->
                asprintf "[%d:%a]" id Forall_id.pp forall
          else ""
        in
        prefix ^ new_name ctx ^ suffix
  in
  ctx.types <- Mem_map.add type_ name ctx.types

let rec pp_type ctx fmt type_ =
  match Mem_map.find type_ ctx.types with
  | Some name -> fprintf fmt "%s" name
  | None ->
      register_name ctx type_;
      pp_type_desc ctx fmt type_

and pp_type_desc ctx fmt type_ =
  let pp_type fmt type_ = pp_type ctx fmt type_ in
  match desc type_ with
  (* just print name *)
  | T_var _ -> pp_type fmt type_
  | T_forall { forall; body } ->
      let vars = forall_vars ~forall body in

      (* not rev_map because order matters here *)
      List.rev vars |> List.iter (fun var -> fprintf fmt "{%a} -> " pp_type var);
      fprintf fmt "%a" pp_type body
  | T_arrow { param; return } ->
      let parens =
        match desc param with
        | T_var _ -> false
        (* TODO: t_struct should need parens? *)
        | T_forall _ | T_arrow _ | T_struct _ | T_type _ -> true
      in
      if parens then fprintf fmt "(%a) -> %a" pp_type param pp_type return
      else fprintf fmt "%a -> %a" pp_type param pp_type return
  | T_struct { fields } ->
      let pp_fields fmt fields =
        List.iter
          (fun { name; type_ } ->
            fprintf fmt "%a: %a; " Name.pp name pp_type type_)
          fields
      in
      fprintf fmt "{ %a }" pp_fields fields
  | T_type { forall = _; type_ } -> fprintf fmt "#(%a)" pp_type type_

let with_pp_type ?(debug = false) f =
  let ctx = new_ctx ~debug in
  let pp_type fmt type_ = pp_type ctx fmt type_ in
  f pp_type

let pp_type fmt type_ = with_pp_type (fun pp_type -> pp_type fmt type_)

let pp_type_debug fmt type_ =
  with_pp_type ~debug:true (fun pp_type -> pp_type fmt type_)

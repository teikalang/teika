open Typer.Typed_tree

let pp formatter term =
  let rec name used base index =
    let candidate = if index = 0 then base else base ^ string_of_int index in
    if List.mem candidate used then name used base (index + 1) else candidate
  in
  let find_name bound var =
    match List.find_opt (fun (found, _) -> v_same found var) bound with
    | Some (_, name) -> name
    | None -> Format.asprintf "%a" Typer.Name.pp (v_name var)
  in
  let bind bound var =
    let name = name (List.map snd bound) (find_name [] var) 0 in
    ((var, name) :: bound, name)
  in
  let rec print bound depth formatter term =
    if depth = 0 then Format.pp_print_string formatter "..."
    else
      let term = whnf term in
      let pp = print bound (depth - 1) in
      match t_head term with
      | T_type -> Format.pp_print_string formatter "Type"
      | T_flex -> Format.pp_print_string formatter "?"
      | T_var { var } -> Format.pp_print_string formatter (find_name bound var)
      | T_lambda { bound = var; body } ->
          let bound, name = bind bound var in
          Format.fprintf formatter "(lambda %s. %a)" name
            (print bound (depth - 1))
            body
      | T_pi { param; body } -> (
          match t_head (whnf body) with
          | T_lambda { bound = var; body } ->
              let bound, name = bind bound var in
              Format.fprintf formatter "(Pi %s : %a. %a)" name pp param
                (print bound (depth - 1))
                body
          | _ -> Format.fprintf formatter "(Pi %a %a)" pp param pp body)
      | T_self { body } | T_fix { body } -> (
          let label =
            match t_head term with T_self _ -> "self" | _ -> "fix"
          in
          match t_head (whnf body) with
          | T_lambda { bound = var; body } ->
              let bound, name = bind bound var in
              Format.fprintf formatter "(%s %s. %a)" label name
                (print bound (depth - 1))
                body
          | _ -> Format.fprintf formatter "(%s %a)" label pp body)
      | T_apply { funct; arg } ->
          Format.fprintf formatter "(%a %a)" pp funct pp arg
      | T_let _ -> Typer.error (t_loc term) "printer: unreduced let"
  in
  print [] 100 formatter term

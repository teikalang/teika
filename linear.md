```ocaml

module Linear = struct
  module Context = Map.Make (String)

  type lambda =
    | L_var of { var : string }
    | L_abs of { var : string; return : lambda }
    | L_app of { lambda : lambda; arg : lambda }

  let find_and_remove ctx var =
    match Context.find_opt var ctx with
    | Some lambda -> Either.Left (Context.remove var ctx, lambda)
    | None -> Either.Right ctx

  type self_lambda = { k : self_lambda Context.t -> self_lambda }
  [@@ocaml.unboxed]

  type never = |

  type 'a context =
    | Nil : never context
    | Cons : ('a * 'b context) -> ('a * 'b) context

  let s_zero ctx =
    (* TODO: case rejected *)
    let (Cons (value, ctx)) = ctx in
    (value, ctx)

  let s_succ var ctx =
    (* TODO: case rejected *)
    let (Cons (head, ctx)) = ctx in
    let value, ctx = var ctx in
    (value, Cons (head, ctx))

  let s_abs ~return ctx arg = return (Cons (arg, ctx))

  let s_app ~lambda ~arg ctx =
    let apply, ctx = lambda ctx in
    apply ctx arg

  let s_id () = s_abs ~return:(s_abs ~return:s_zero) Nil

  let s_pair ctx =
    let return =
      s_app
        ~lambda:(s_app ~lambda:s_zero ~arg:(s_succ @@ s_succ s_zero))
        ~arg:(s_succ s_zero)
    in
    (* left => right => k => return *)
    s_abs ~return:(s_abs ~return:(s_abs ~return)) ctx

  let id ctx value = (value, ctx)
  let s_id ctx = s_abs ~return:s_zero ctx
  let pair left right = close s_pair left right
  let id value = close (s_app ~lambda:s_id ~arg:value)
  let s_fix ctx = s_abs ~return:(s_app ~lambda:s_zero ~arg:s_zero) ctx
  let s_true ctx = s_abs ~return:(s_abs ~return:(s_succ s_zero)) ctx
  let s_false ctx = s_abs ~return:(s_abs ~return:s_zero) ctx

  (* let rec subst ~from ~to_ lambda =
     match lambda with
     | L_var { var } -> (

         match String.equal with
         | Some (ctx,lambda) -> (ctx, lambda)
         | None -> assert false) *)

  let normalize_var ~normalize ctx0 var0 =
    match find_and_remove ctx0 var0 with
    | Either.Left (ctx1, lambda1) -> normalize ctx1 lambda1
    | Either.Right ctx2 ->
        (* TODO: var appears twice  *)
        (ctx2, L_var { var = var0 })

  let normalize_abs ~normalize ctx3 var3 return3 =
    let ctx4, return4 = normalize ctx3 return3 in
    (ctx4, L_abs { var = var3; return = return4 })

  let apply ~normalize ctx5 lambda5 arg5 =
    (* TODO: better name / structure *)
    (* lambda5 is already normal *)
    let apply =
      match lambda5 with
      | L_var { var = var6 } ->
          fun ctx6 arg6 ->
            let lambda6 = L_var { var = var6 } in
            (ctx6, L_app { lambda = lambda6; arg = arg6 })
      | L_abs { var = var7; return = return7 } ->
          fun ctx7 arg7 ->
            let ctx8 = Context.add var7 arg7 ctx7 in
            normalize ctx8 return7
      | L_app { lambda = lambda9; arg = arg9 } ->
          fun ctx10 arg10 ->
            let lambda10 = L_app { lambda = lambda9; arg = arg9 } in
            (ctx10, L_app { lambda = lambda10; arg = arg10 })
    in
    apply ctx5 arg5

  let normalize_app ~normalize ctx11 ~lambda:lambda11 ~arg:arg11 =
    let ctx12, lambda12 = normalize ctx11 lambda11 in
    apply ~normalize ctx12 lambda12 arg11

  let rec normalize ctx12 lambda12 =
    let apply =
      match lambda12 with
      | L_var { var = var13 } ->
          fun ctx13 -> normalize_var ~normalize ctx13 var13
      | L_abs { var = var14; return = return14 } ->
          fun ctx14 -> normalize_abs ~normalize ctx14 var14 return14
      | L_app { lambda = lambda15; arg = arg15 } ->
          fun ctx15 ->
            normalize_app ~normalize ctx15 ~lambda:lambda15 ~arg:arg15
    in
    apply ctx12
end
```

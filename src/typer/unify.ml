open Type
open Type_utils
open Instance

type error =
  | Type_clash of { expected : type_; received : type_ }
  | Occur_check of { var : type_; type_ : type_ }
  | Escape_check of { var : type_; type_ : type_ }
[@@deriving show]

exception Error of { loc : Location.t; error : error }

(* TODO: where to put this? *)
(* TODO: use the loc*)
let () =
  Printexc.register_printer (function
    | Error { loc = _; error } -> Some (Format.asprintf "%a\n%!" pp_error error)
    | _ -> None)

(* TODO: this likely should be removed *)
type ctx = {
  loc : Location.t;
  env : Env.t;
  bound_forall : (Forall_id.t * Rank.t) list;
}

let raise ctx error = raise (Error { loc = ctx.loc; error })
let instance ctx ~forall type_ = instance ctx.env ~forall type_

let with_expected_forall ctx ~forall =
  let { loc; env; bound_forall } = ctx in
  (* no bound var at initial env rank *)
  let env = Env.enter_rank env in
  let rank = Env.current_rank env in
  let bound_forall = (forall, rank) :: bound_forall in
  { loc; env; bound_forall }

let forall_rank ctx ~forall = List.assoc_opt forall ctx.bound_forall

let forall_rank_exn ctx ~forall =
  match forall_rank ctx ~forall with
  | Some rank -> rank
  | None -> failwith "found forall but not introduced"

let make_env env ~loc = { loc; env; bound_forall = [] }

let occur_check env ~var type_ =
  if in_type ~var type_ then raise env (Occur_check { var; type_ })

let rec min_rank ctx rank foralls type_ =
  let min_rank rank foralls type_ = min_rank ctx rank foralls type_ in
  let min a b = if Rank.(a > b) then b else a in
  match desc type_ with
  | T_var (Weak var_rank) -> min rank var_rank
  (* TODO: remove this *)
  | T_var (Bound _) when Type.same type_ Env.int_type -> rank
  | T_var (Bound { forall; name = _ }) ->
      let ignore =
        List.exists (fun forall' -> Forall_id.equal forall forall') foralls
      in
      if ignore then rank
      else
        let var_rank = forall_rank_exn ctx ~forall in
        min var_rank rank
  | T_forall { forall; body } ->
      let foralls = forall :: foralls in
      min_rank rank foralls body
  | T_arrow { param; return } ->
      let rank = min_rank rank foralls param in
      min_rank rank foralls return

let min_rank ctx rank type_ = min_rank ctx rank [] type_

let rec update_rank ctx ~var ~rank type_ =
  let update_rank type_ = update_rank ctx ~var ~rank type_ in
  match desc type_ with
  | T_var (Weak _) -> lower ~var:type_ rank
  | T_var (Bound { forall; name = _ }) -> (
      match forall_rank ctx ~forall with
      (* None implies this var is not behind *)
      | None -> ()
      | Some var_rank ->
          (* received is introduced after rank is incremented so > *)
          if Rank.(rank >= var_rank) then ()
          else raise ctx (Escape_check { var; type_ }))
  | T_forall { forall = _; body } -> update_rank body
  | T_arrow { param; return } ->
      update_rank param;
      update_rank return

(* also escape check *)
let update_rank ctx ~var type_ =
  let var_rank =
    (* TODO: invariant var is weak var *)
    match desc var with T_var (Weak rank) -> rank | _ -> assert false
  in
  let rank = min_rank ctx var_rank type_ in
  update_rank ctx ~var ~rank type_

let unify_var ctx ~var type_ =
  occur_check ctx ~var type_;
  update_rank ctx ~var type_;

  link ~to_:type_ var

let rec unify ctx ~expected ~received =
  (* 1: same  *)
  if same expected received then () else unify_desc ctx ~expected ~received

and unify_desc ctx ~expected ~received =
  match (desc expected, desc received) with
  (* 2: weak vars *)
  | T_var (Weak _), _ -> unify_var ctx ~var:expected received
  | _, T_var (Weak _) -> unify_var ctx ~var:received expected
  (* 3: expected forall *)
  | T_forall { forall; body }, _ ->
      let ctx = with_expected_forall ctx ~forall in
      unify ctx ~expected:body ~received
  (* 4: received forall *)
  | _, T_forall { forall; body } ->
      let body = instance ctx ~forall body in
      unify ctx ~expected ~received:body
  (* simple *)
  | ( T_arrow { param = expected_param; return = expected_return },
      T_arrow { param = received_param; return = received_return } ) ->
      unify ctx ~expected:received_param ~received:expected_param;
      unify ctx ~expected:expected_return ~received:received_return
  | T_var (Bound _), _ | _, T_var (Bound _) ->
      raise ctx (Type_clash { expected; received })

let unify ~loc env ~expected ~received =
  let ctx = make_env env ~loc in
  unify ctx ~expected ~received

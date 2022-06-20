open Type
open Instance

let pp_type_ = Print.pp_type_debug

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

let raise loc error = raise (Error { loc; error })

let occur_check loc ~var type_ =
  if Helpers.in_type ~var type_ then raise loc (Occur_check { var; type_ })

let rec update_rank loc ~var ~max_forall type_ =
  let update_rank type_ = update_rank loc ~var ~max_forall type_ in
  match desc type_ with
  | T_var var_desc ->
      let forall, is_bound =
        match var_desc with
        | Weak { forall } -> (forall, false)
        | Bound { forall } -> (forall, true)
      in
      (* received is introduced after rank is incremented so >
               generic is the lowest, so it will always match this *)
      (* TODO: why isn't this > ? *)
      if Rank.(Forall.rank max_forall < Forall.rank forall) then
        if is_bound then raise loc (Escape_check { var; type_ })
        else (* TODO: assert is weak var *)
          lower_var ~to_:max_forall type_
  | T_forall { forall = _; return } -> update_rank return
  | T_arrow { param; return } ->
      update_rank param;
      update_rank return
  | T_record { fields } ->
      (* TODO: also check name *)
      List.iter (fun { name = _; type_ } -> update_rank type_) fields
  | T_type { forall = _; type_ } -> (* TODO: is this right? *) update_rank type_

(* also escape check *)
let update_rank loc ~var type_ =
  let max_forall =
    (* TODO: invariant var is weak var *)
    match desc var with T_var (Weak { forall }) -> forall | _ -> assert false
  in
  update_rank loc ~var ~max_forall type_

let unify_var loc ~var type_ =
  occur_check loc ~var type_;
  update_rank loc ~var type_;

  link ~to_:type_ var

let unify_var env ~var type_ = unify_var (Env.current_loc env) ~var type_

let rec unify env rank ~expected ~received =
  (* 1: same  *)
  if same expected received then () else unify_desc env rank ~expected ~received

and unify_desc env rank ~expected ~received =
  match (desc expected, desc received) with
  (* 2: weak vars *)
  | T_var (Weak _), _ -> unify_var env ~var:expected received
  | _, T_var (Weak _) -> unify_var env ~var:received expected
  (* 3: expected forall *)
  | T_forall { forall; return }, _ ->
      (* TODO: tag + clear is weird *)
      let rank = Rank.next rank in
      let env =
        (* TODO: neede because instance_weaken below, is this right? *)
        let forall = Forall.weak rank in
        Env.with_forall forall env
      in
      Forall.with_rank
        (fun () -> unify env rank ~expected:return ~received)
        rank forall
  (* 4: received forall *)
  | _, T_forall { forall; return } ->
      let return = instance_weaken env ~forall return in
      unify env rank ~expected ~received:return
  (* simple *)
  | ( T_arrow { param = expected_param; return = expected_return },
      T_arrow { param = received_param; return = received_return } ) ->
      unify env rank ~expected:received_param ~received:expected_param;
      unify env rank ~expected:expected_return ~received:received_return
  | T_record { fields = expected_fields }, T_record { fields = received_fields }
    ->
      if List.length expected_fields <> List.length received_fields then
        raise (Env.current_loc env) (Type_clash { expected; received });
      (* TODO: proper subtyping *)
      List.iter2
        (fun expected_field received_field ->
          let { name = _; type_ = expected } = expected_field in
          let { name = _; type_ = received } = received_field in
          unify env rank ~expected ~received)
        expected_fields received_fields
  | ( T_type { forall = _; type_ = expected },
      T_type { forall = _; type_ = received } ) ->
      (* TODO: proper unification of types? *)
      (* TODO: does this make sense *)
      unify env rank ~expected ~received
  | T_record _, _
  | _, T_record _
  | T_var _, _
  | _, T_var _
  | T_type _, _
  | _, T_type _ ->
      raise (Env.current_loc env) (Type_clash { expected; received })

(* TODO: probably env could be removed here *)
let unify env ~expected ~received =
  let rank = Env.current_rank env in
  unify env rank ~expected ~received

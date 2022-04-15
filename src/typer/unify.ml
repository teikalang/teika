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

(* TODO: this is unify env *)
type env = {
  loc : Location.t;
  rank : int;
  bound_forall : (Forall_id.t * int) list;
  weak_vars : (type_ * int) list;
}

let make_env ~loc = { loc; rank = 0; bound_forall = []; weak_vars = [] }
let raise env error = raise (Error { loc = env.loc; error })

let with_expected_forall env ~forall =
  let { loc; rank; bound_forall; weak_vars } = env in
  let rank = rank + 1 in
  (* no bound var at 0 *)
  (* TODO: is this okay? *)
  let bound_forall = (forall, rank) :: bound_forall in
  { loc; rank; bound_forall; weak_vars }

let with_received_forall env ~vars =
  let { loc; rank; bound_forall; weak_vars } = env in
  let vars = List.map (fun var -> (var, rank)) vars in
  let weak_vars = vars @ weak_vars in
  { loc; rank; bound_forall; weak_vars }

let forall_rank env ~forall =
  match List.assoc_opt forall env.bound_forall with
  | Some rank -> rank
  | None -> failwith "found forall but not introduced"

let weak_var_rank env ~var =
  match List.assoc_opt var env.weak_vars with
  | Some rank -> rank
  (* TODO: 0 is okay, because expected forall is eliminated first
     so 0 is before any *)
  | None -> 0

let occur_check env ~var type_ =
  if in_type ~var type_ then raise env (Occur_check { var; type_ })

let rec min_bound_forall env foralls min_rank type_ =
  let min_bound_forall foralls min_rank type_ =
    min_bound_forall env foralls min_rank type_
  in
  match desc type_ with
  | T_int -> min_rank
  | T_weak_var -> min_rank
  | T_bound_var { forall } -> (
      let ignore =
        List.exists (fun forall' -> Forall_id.equal forall forall') foralls
      in
      if ignore then min_rank
      else
        let forall_rank = forall_rank env ~forall in
        match min_rank with
        | Some min_rank -> Some (min min_rank forall_rank)
        | None -> Some forall_rank)
  | T_forall { forall; body } ->
      let foralls = forall :: foralls in
      min_bound_forall foralls min_rank body
  | T_arrow { param; return } ->
      let min_rank = min_bound_forall foralls min_rank param in
      min_bound_forall foralls min_rank return
  | T_link _ -> assert false

let min_bound_forall env type_ = min_bound_forall env [] None type_

let escape_check env ~var type_ =
  let type_rank = min_bound_forall env type_ in
  let var_rank = weak_var_rank env ~var in

  match type_rank with
  | Some type_rank ->
      (* received is introduced after rank is incremented so > instead of >=*)
      if var_rank >= type_rank then ()
      else raise env (Escape_check { var; type_ })
  | None -> ()

let unify_var env ~var type_ =
  (* TODO: invariant var is var *)
  occur_check env ~var type_;
  escape_check env ~var type_;
  link ~to_:type_ var

let rec unify env ~expected ~received =
  (* 1: same  *)
  if same expected received then () else unify_desc env ~expected ~received

and unify_desc env ~expected ~received =
  match (desc expected, desc received) with
  (* 2: weak vars *)
  | T_weak_var, _ -> unify_var env ~var:expected received
  | _, T_weak_var -> unify_var env ~var:received expected
  (* 3: expected forall *)
  | T_forall { forall; body }, _ ->
      let env = with_expected_forall env ~forall in
      unify env ~expected:body ~received
  (* 4: received forall *)
  | _, T_forall { forall; body } ->
      let body, vars = instance ~forall body in
      let env = with_received_forall env ~vars in
      unify env ~expected ~received:body
  (* simple *)
  | T_int, T_int -> ()
  | ( T_arrow { param = expected_param; return = expected_return },
      T_arrow { param = received_param; return = received_return } ) ->
      unify env ~expected:received_param ~received:expected_param;
      unify env ~expected:expected_return ~received:received_return
  | T_bound_var _, _ | _, T_bound_var _ | T_int, _ | _, T_int ->
      raise env (Type_clash { expected; received })
  | T_link _, _ | _, T_link _ -> assert false

let unify ~loc ~expected ~received =
  let env = make_env ~loc in
  unify env ~expected ~received

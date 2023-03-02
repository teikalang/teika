open Ltree
open Ttree

(* TODO: remove all failwith *)
let rec pat_var : type a. a pat -> _ =
 fun pat ->
  match pat with
  | TP_loc { pat; loc = _ } -> pat_var pat
  | TP_typed { pat; type_ = _ } -> pat_var pat
  | TP_var { var } -> var

let lazy_subst_term ~from ~to_ term = TT_subst { from; to_; term }

let rec subst_term : type a. from:_ -> to_:_ -> a term -> ex_term =
 fun ~from ~to_ term ->
  let subst_term term = subst_term ~from ~to_ term in
  (* TODO: to go further on lazy substitutions, rename the function below *)
  let lazy_subst_term term = lazy_subst_term ~from ~to_ term in
  let subst_pat pat = subst_pat ~from ~to_ pat in
  match term with
  | TT_loc { term; loc } ->
      let (Ex_term term) = subst_term term in
      Ex_term (TT_loc { term; loc })
  | TT_typed { term; type_ } ->
      let (Ex_term term) = subst_term term in
      let type_ = lazy_subst_term type_ in
      Ex_term (TT_typed { term; type_ })
  | TT_subst _ as term -> Ex_term (lazy_subst_term term)
  | TT_var { var } -> (
      match Var.equal var from with
      | true -> Ex_term to_
      | false -> Ex_term (TT_var { var }))
  | TT_arrow { param; return } ->
      let param = subst_pat param in
      let (Ex_term return) =
        match Var.equal from (pat_var param) with
        | true -> Ex_term return
        | false -> subst_term return
      in
      Ex_term (TT_arrow { param; return })
  | TT_lambda { param; return } ->
      let param = subst_pat param in
      let (Ex_term return) =
        match Var.equal from (pat_var param) with
        | true -> Ex_term return
        | false -> subst_term return
      in
      Ex_term (TT_lambda { param; return })
  | TT_apply { lambda; arg } ->
      let (Ex_term lambda) = subst_term lambda in
      let (Ex_term arg) = subst_term arg in
      Ex_term (TT_apply { lambda; arg })

and subst_pat : type a. from:_ -> to_:_ -> a pat -> a pat =
 fun ~from ~to_ pat ->
  let lazy_subst_term term = TT_subst { from; to_; term } in
  let subst_pat pat = subst_pat ~from ~to_ pat in
  match pat with
  | TP_loc { pat; loc } ->
      let pat = subst_pat pat in
      TP_loc { pat; loc }
  | TP_typed { pat; type_ } ->
      let pat = subst_pat pat in
      let type_ = lazy_subst_term type_ in
      TP_typed { pat; type_ }
  | TP_var _ as pat -> pat

let rec expand_head : type a. a term -> _ =
 fun term ->
  match term with
  | TT_loc { term; loc = _ } -> expand_head term
  | TT_typed { term; type_ = _ } -> expand_head term
  | TT_subst { from; to_; term } ->
      let (Ex_term term) = subst_term ~from ~to_ term in
      expand_head term
  | TT_var _ as term -> term
  | TT_arrow _ as term -> term
  | TT_lambda _ as term -> term
  | TT_apply { lambda; arg } -> (
      match expand_head lambda with
      | TT_lambda { param; return } ->
          let (Ex_term term) =
            subst_term ~from:(pat_var param) ~to_:arg return
          in
          expand_head term
      | lambda -> TT_apply { lambda; arg })

let rename ~from ~to_ term =
  let to_ = TT_var { var = to_ } in
  subst_term ~from ~to_ term

let split_pat pat =
  let (TP_typed { pat; type_ }) = pat in
  (pat_var pat, Ex_term type_)

(* TODO: maybe Var.copy *)
let copy_var var = Var.create (Var.name var)

(* equal1 checks for physical equality
   equal2 does structural equality *)
let rec equal1 : type r e. received:r term -> expected:e term -> _ =
 fun ~received ~expected ->
  let received = expand_head received in
  let expected = expand_head expected in
  match expected == received with
  | true -> ()
  | false -> equal2 ~received ~expected

and equal2 ~received ~expected =
  match (received, expected) with
  | TT_var { var = received }, TT_var { var = expected } -> (
      match Var.equal received expected with
      | true -> ()
      | false -> failwith "var clash")
  | ( TT_arrow { param = received_param; return = received_return },
      TT_arrow { param = expected_param; return = expected_return } ) ->
      equal_arrow_lambda ~received_param ~received_return ~expected_param
        ~expected_return
  | ( TT_lambda { param = received_param; return = received_return },
      TT_lambda { param = expected_param; return = expected_return } ) ->
      equal_arrow_lambda ~received_param ~received_return ~expected_param
        ~expected_return
  | ( TT_apply { lambda = received_lambda; arg = received_arg },
      TT_apply { lambda = expected_lambda; arg = expected_arg } ) ->
      equal1 ~received:received_lambda ~expected:expected_lambda;
      equal1 ~received:received_arg ~expected:expected_arg
  | ( (TT_var _ | TT_arrow _ | TT_lambda _ | TT_apply _),
      (TT_var _ | TT_arrow _ | TT_lambda _ | TT_apply _) ) ->
      failwith "type clash"

and equal_arrow_lambda :
    type r e.
    received_param:_ ->
    received_return:r term ->
    expected_param:_ ->
    expected_return:e term ->
    _ =
 fun ~received_param ~received_return ~expected_param ~expected_return ->
  let received_var, Ex_term received_type = split_pat received_param in
  let expected_var, Ex_term expected_type = split_pat expected_param in
  (* TODO: is this checking needed? Maybe a flag *)
  equal1 ~received:expected_type ~expected:received_type;

  let skolem_var = copy_var expected_var in
  let (Ex_term received_return) =
    rename ~from:received_var ~to_:skolem_var received_return
  in
  let (Ex_term expected_return) =
    rename ~from:expected_var ~to_:skolem_var expected_return
  in
  equal1 ~received:received_return ~expected:expected_return

let typeof_term term =
  let (TT_typed { term = _; type_ }) = term in
  Ex_term type_

let typeof_pat term =
  let (TP_typed { pat = _; type_ }) = term in
  Ex_term type_

let wrap_term type_ term = TT_typed { term; type_ }
let wrap_pat type_ pat = TP_typed { pat; type_ }

let tt_type =
  let type_ = TT_var { var = Var.type_ } in
  wrap_term type_ @@ TT_var { var = Var.type_ }

module Context : sig
  type context
  type t = context

  val initial : context
  val enter_param : param:typed pat -> context -> context
  val enter_alias : bound:typed pat -> value:typed term -> context -> context
  val lookup : name:Name.t -> context -> typed term option
end = struct
  type context = typed term Name.Map.t
  type t = context

  let initial =
    let open Name.Map in
    add (Var.name Var.type_) tt_type empty

  let enter_param ~param ctx =
    let var, Ex_term type_ = split_pat param in
    let name = Var.name var in
    let term = TT_typed { term = TT_var { var }; type_ } in
    Name.Map.add name term ctx

  let enter_alias ~bound ~value ctx =
    let var = pat_var bound in
    let name = Var.name var in
    (* TODO: preserve aliasing on lookup *)
    Name.Map.add name value ctx

  let lookup ~name ctx = Name.Map.find_opt name ctx
end

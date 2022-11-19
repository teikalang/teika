open Ttree

module Level : sig
  type level
  type t = level [@@deriving eq]

  val zero : level
  val next : level -> level
  val ( < ) : level -> level -> bool
end = struct
  type level = int
  and t = level [@@deriving eq]

  let zero = 0

  (* TODO: check for overflows *)
  let next n = n + 1
  let ( < ) : level -> level -> bool = ( < )
end

module Context : sig
  (* TODO: maybe call it var_desc? *)
  (** This describes which kind of variable are we dealing with
      - V_rigid are universal variables, they cannot move
      - V_alias are used when binding two rigid variables
          it is also used when unifying two hole variables
      - V_link are hole variables that were unified 
      - V_hole are existential variables, they can move 
          they can also be linked *)
  type var_info = private
    | V_rigid of { level : Level.t }
    | V_alias of { var : Var.t }
    (* TODO: this shouldy be type_ : type_ *)
    | V_link of { type_ : term_desc }
    | V_hole of { level : Level.t }

  type error = private CError of { loc : Location.t; desc : error_desc }

  and error_desc = private
    (* unification *)
    | CError_occurs_check of { var : Var.t; in_ : Var.t }
    | CError_escape_check of { var : Var.t; to_ : Var.t }
    | CError_var_constrained of { var : Var.t; by_ : term_desc }
    | CError_var_clash of { expected : Var.t; received : Var.t }
    | CError_type_clash of { expected : term_desc; received : term_desc }
    (* invariants *)
    | CError_unknown_var_repr of { var : Var.t }
    | CError_unknown_var_link of { var : Var.t }
    | CError_unknown_var_lower of { var : Var.t }
    | CError_unknown_var_alias of { var : Var.t }
    | CError_duplicated_var_rigid of { var : Var.t }
    | CError_duplicated_var_alias of { var : Var.t }
    | CError_duplicated_var_hole of { var : Var.t }
    | CError_invalid_var_link of { var : Var.t; info : var_info }
    | CError_invalid_var_lower of { var : Var.t; info : var_info }
    | CError_invalid_var_alias of { var : Var.t; info : var_info }
    | CError_lowering_to_higher_level of { var : Var.t }

  type 'a context
  type 'a t = 'a context

  val return : 'a -> 'a context
  val ( >>= ) : 'a context -> ('a -> 'b context) -> 'b context
  val fail_occurs_check : Var.t -> in_:Var.t -> 'a context
  val fail_escape_check : Var.t -> to_:Var.t -> 'a context
  val fail_var_constrained : Var.t -> by_:term_desc -> 'a context
  val fail_var_clash : expected:Var.t -> received:Var.t -> 'a context
  val fail_type_clash : expected:term_desc -> received:term_desc -> 'a context

  (* read *)
  val loc : Location.t context
  val level : Level.t context
  val repr : Var.t -> var_info context

  (* update *)
  val with_loc : Location.t -> (unit -> 'a context) -> 'a context
  val with_region : (unit -> 'a context) -> 'a context
  val with_var_rigid : Var.t -> (unit -> 'a context) -> 'a context
  val with_var_alias : Var.t -> of_:Var.t -> (unit -> 'a context) -> 'a context

  (* unification *)
  val enter_var_hole : Var.t -> unit context
  val var_link : Var.t -> to_:term_desc -> unit context
  val var_lower : Var.t -> to_:Level.t -> unit context
  val var_alias : Var.t -> of_:Var.t -> unit context
  (* TODO: generalize *)
end = struct
  type var_info =
    | V_rigid of { level : Level.t }
    | V_alias of { var : Var.t }
    | V_link of { type_ : term_desc }
    | V_hole of { level : Level.t }

  type error = CError of { loc : Location.t; desc : error_desc }

  and error_desc =
    | CError_occurs_check of { var : Var.t; in_ : Var.t }
    | CError_escape_check of { var : Var.t; to_ : Var.t }
    | CError_var_constrained of { var : Var.t; by_ : term_desc }
    | CError_var_clash of { expected : Var.t; received : Var.t }
    | CError_type_clash of { expected : term_desc; received : term_desc }
    (* invariants *)
    | CError_unknown_var_repr of { var : Var.t }
    | CError_unknown_var_link of { var : Var.t }
    | CError_unknown_var_lower of { var : Var.t }
    | CError_unknown_var_alias of { var : Var.t }
    | CError_duplicated_var_rigid of { var : Var.t }
    | CError_duplicated_var_alias of { var : Var.t }
    | CError_duplicated_var_hole of { var : Var.t }
    | CError_invalid_var_link of { var : Var.t; info : var_info }
    | CError_invalid_var_lower of { var : Var.t; info : var_info }
    | CError_invalid_var_alias of { var : Var.t; info : var_info }
    | CError_lowering_to_higher_level of { var : Var.t }

  type data = {
    loc : Location.t;
    (* unification *)
    level : Level.t;
    (* TODO: for variables this could be done using an array
        as even in big programs the number of variables is relatively
        small, unless we start generating variables during typing *)
    vars : var_info Var.Map.t;
  }

  (* TODO: I'm bad with monads *)
  type 'a context = Context of (data -> ('a * data, error * data) result)
  [@@ocaml.unboxed]

  type 'a t = 'a context

  let return content = Context (fun data -> Ok (content, data))

  let fail desc =
    Context
      (fun data ->
        let { loc; level = _; vars = _ } = data in
        Error (CError { loc; desc }, data))

  let apply context data f =
    let (Context context) = context in
    match context data with
    | Ok (content, data) -> f content data
    | Error _error as error -> error

  let ( >>= ) context f =
    Context
      (fun data ->
        let (Context context) = context in
        match context data with
        | Ok (content, data) ->
            let (Context f) = f content in
            f data
        | Error _error as error -> error)

  let fail_occurs_check var ~in_ = fail (CError_occurs_check { var; in_ })
  let fail_escape_check var ~to_ = fail (CError_escape_check { var; to_ })
  let fail_var_constrained var ~by_ = fail (CError_var_constrained { var; by_ })

  let fail_var_clash ~expected ~received =
    fail (CError_var_clash { expected; received })

  let fail_type_clash ~expected ~received =
    fail (CError_type_clash { expected; received })

  let loc =
    Context
      (fun data ->
        let { loc; level = _; vars = _ } = data in
        Ok (loc, data))

  let level =
    Context
      (fun data ->
        let { loc = _; level; vars = _ } = data in
        Ok (level, data))

  let repr var =
    Context
      (fun data ->
        let { loc; level = _; vars } = data in
        match Var.Map.find_opt var vars with
        | Some var_info -> Ok (var_info, data)
        | None ->
            let desc = CError_unknown_var_repr { var } in
            Error (CError { loc; desc }, data))

  let with_loc loc f =
    Context
      (fun data ->
        let { loc = initial_loc; level; vars } = data in
        apply (f ()) { loc; level; vars } @@ fun content data ->
        let { loc = _closing_loc; level; vars } = data in
        Ok (content, { loc = initial_loc; level; vars }))

  let with_region f =
    Context
      (fun data ->
        let { loc; level = initial_level; vars } = data in
        let level = Level.next initial_level in
        apply (f ()) { loc; level; vars } @@ fun content data ->
        let { loc; level = _closing_level; vars } = data in
        (* TODO: check no variable remaining on the level before closing *)
        Ok (content, { loc; level = initial_level; vars }))

  (* TODO: duplicated code *)
  let with_var_rigid var f =
    Context
      (fun data ->
        let { loc; level; vars } = data in
        match Var.Map.mem var vars with
        | true ->
            let desc = CError_duplicated_var_rigid { var } in
            Error (CError { loc; desc }, data)
        | false ->
            let info = V_rigid { level } in
            let vars = Var.Map.add var info vars in
            apply (f ()) { loc; level; vars } @@ fun content data ->
            let { loc; level; vars } = data in
            (* TODO: check variable did not escape it's scope *)
            let vars = Var.Map.remove var vars in
            Ok (content, { loc; level; vars }))

  (* TODO: duplicated code *)
  let with_var_alias var ~of_ f =
    Context
      (fun data ->
        let { loc; level; vars } = data in
        match Var.Map.mem var vars with
        | true ->
            let desc = CError_duplicated_var_alias { var } in
            Error (CError { loc; desc }, data)
        | false ->
            let info = V_alias { var = of_ } in
            let vars = Var.Map.add var info vars in
            apply (f ()) { loc; level; vars } @@ fun content data ->
            let { loc; level; vars } = data in
            (* TODO: check variable did not escape it's scope *)
            let vars = Var.Map.remove var vars in
            Ok (content, { loc; level; vars }))

  let enter_var_hole var =
    Context
      (fun data ->
        let { loc; level; vars } = data in
        match Var.Map.mem var vars with
        | true ->
            let desc = CError_duplicated_var_hole { var } in
            Error (CError { loc; desc }, data)
        | false ->
            let info = V_hole { level } in
            let vars = Var.Map.add var info vars in
            Ok ((), { loc; level; vars }))

  let var_link var ~to_ =
    (* TODO: path compression *)
    Context
      (fun data ->
        let { loc; level; vars } = data in
        (* TODO: we could track substitution locations here for LSP *)
        match Var.Map.find_opt var vars with
        | Some (V_hole { level = _ }) ->
            let info = V_link { type_ = to_ } in
            let vars = Var.Map.add var info vars in
            Ok ((), { loc; level; vars })
        | Some ((V_rigid _ | V_alias _ | V_link _) as info) ->
            let desc = CError_invalid_var_link { var; info } in
            Error (CError { loc; desc }, data)
        | None ->
            let desc = CError_unknown_var_link { var } in
            Error (CError { loc; desc }, data))

  let var_lower var ~to_ =
    Context
      (fun data ->
        let { loc; level; vars } = data in
        (* TODO: we could track lowering locations here for LSP *)
        match Var.Map.find_opt var vars with
        | Some (V_hole { level = hole_level }) -> (
            match Level.(hole_level < to_) with
            | true ->
                let desc = CError_lowering_to_higher_level { var } in
                Error (CError { loc; desc }, data)
            | false ->
                let info = V_hole { level = to_ } in
                let vars = Var.Map.add var info vars in
                Ok ((), { loc; level; vars }))
        | Some ((V_rigid _ | V_alias _ | V_link _) as info) ->
            let desc = CError_invalid_var_lower { var; info } in
            Error (CError { loc; desc }, data)
        | None ->
            let desc = CError_unknown_var_lower { var } in
            Error (CError { loc; desc }, data))

  let var_alias var ~of_ =
    Context
      (fun data ->
        let { loc; level; vars } = data in
        (* TODO: we could track lowering locations here for LSP *)
        match Var.Map.find_opt var vars with
        | Some (V_hole { level = _ }) ->
            (* TODO: check level is equal or higher than of_ *)
            let info = V_alias { var = of_ } in
            let vars = Var.Map.add var info vars in
            Ok ((), { loc; level; vars })
        | Some ((V_rigid _ | V_alias _ | V_link _) as info) ->
            let desc = CError_invalid_var_alias { var; info } in
            Error (CError { loc; desc }, data)
        | None ->
            let desc = CError_unknown_var_alias { var } in
            Error (CError { loc; desc }, data))
end

module Unify : sig
  val unify_term : expected:term -> received:term -> unit Context.t
  val unify_type : expected:type_ -> received:type_ -> unit Context.t
end = struct
  open Context

  (* TODO: maybe some quality of life, guarantee that unification always
      prefer the top level expected type, by tracking variance *)
  (* Reject cases where a variable would be unified with itself,
       this would allow to create negatively recursive types.
     Example: (f => f f)

     Also reject cases where a variable would escape it's scope,
       this would allow to violate abstractions.
     Example: (x => A => (x : A)) *)
  (* TODO: if every constructor had max_level,
      this could be short circuited, avoiding traversing
      constructors that have max_level < hole_level *)
  let rec occurs_and_escape_check_term ~hole_level ~hole_var ~in_ =
    let (TTerm { loc = _; desc; type_ }) = in_ in
    (* TODO: why is this needed? *)
    occurs_and_escape_check_type ~hole_level ~hole_var ~in_:type_ >>= fun () ->
    occurs_and_escape_check_desc ~hole_level ~hole_var ~in_:desc

  and occurs_and_escape_check_type ~hole_level ~hole_var ~in_ =
    let (TType { loc = _; desc }) = in_ in
    occurs_and_escape_check_desc ~hole_level ~hole_var ~in_:desc

  and occurs_and_escape_check_annot ~hole_level ~hole_var ~in_ f =
    let (TAnnot { loc = _; var; annot }) = in_ in
    occurs_and_escape_check_type ~hole_level ~hole_var ~in_:annot >>= fun () ->
    (* TODO: is region needed here? *)
    with_region @@ fun () -> with_var_rigid var f

  and occurs_and_escape_check_bind ~hole_level ~hole_var ~in_ f =
    let (TBind { loc = _; var; value }) = in_ in
    occurs_and_escape_check_term ~hole_level ~hole_var ~in_:value >>= fun () ->
    (* TODO: is region needed here? *)
    with_region @@ fun () -> with_var_rigid var f

  and occurs_and_escape_check_desc ~hole_level ~hole_var ~in_ =
    match in_ with
    | TT_var { var } ->
        occurs_and_escape_check_var ~hole_level ~hole_var ~in_:var
    | TT_forall { param; return } ->
        occurs_and_escape_check_annot ~hole_level ~hole_var ~in_:param
        @@ fun () ->
        occurs_and_escape_check_type ~hole_level ~hole_var ~in_:return
    | TT_lambda { param; return } ->
        occurs_and_escape_check_annot ~hole_level ~hole_var ~in_:param
        @@ fun () ->
        occurs_and_escape_check_term ~hole_level ~hole_var ~in_:return
    | TT_apply { lambda; arg } ->
        occurs_and_escape_check_term ~hole_level ~hole_var ~in_:lambda
        >>= fun () ->
        occurs_and_escape_check_term ~hole_level ~hole_var ~in_:arg
    | TT_exists { left; right } ->
        occurs_and_escape_check_annot ~hole_level ~hole_var ~in_:left
        @@ fun () ->
        occurs_and_escape_check_annot ~hole_level ~hole_var ~in_:right
        @@ fun () -> return ()
    | TT_pair { left; right } ->
        occurs_and_escape_check_bind ~hole_level ~hole_var ~in_:left
        @@ fun () ->
        occurs_and_escape_check_bind ~hole_level ~hole_var ~in_:right
        @@ fun () -> return ()
    | TT_unpair { left; right; pair; return } ->
        occurs_and_escape_check_term ~hole_level ~hole_var ~in_:pair
        >>= fun () ->
        (* TODO: is region needed here? *)
        with_region @@ fun () ->
        with_var_rigid left @@ fun () ->
        (* TODO: is region needed here? *)
        with_region @@ fun () ->
        with_var_rigid right @@ fun () ->
        occurs_and_escape_check_term ~hole_level ~hole_var ~in_:return
    | TT_let { bound; return } ->
        occurs_and_escape_check_bind ~hole_level ~hole_var ~in_:bound
        @@ fun () ->
        occurs_and_escape_check_term ~hole_level ~hole_var ~in_:return
    | TT_annot { value; annot } ->
        occurs_and_escape_check_type ~hole_level ~hole_var ~in_:annot
        >>= fun () ->
        occurs_and_escape_check_term ~hole_level ~hole_var ~in_:value

  and occurs_and_escape_check_var ~hole_level ~hole_var ~in_ =
    match Var.equal hole_var in_ with
    | true -> fail_occurs_check hole_var ~in_
    | false -> (
        repr in_ >>= function
        | V_rigid { level } -> (
            match Level.(hole_level < level) with
            | true -> fail_escape_check in_ ~to_:hole_var
            | false -> return ())
        | V_alias { var = in_ } ->
            occurs_and_escape_check_var ~hole_level ~hole_var ~in_
        | V_link { type_ = in_ } ->
            occurs_and_escape_check_desc ~hole_level ~hole_var ~in_
        | V_hole { level } -> (
            match Level.(hole_level < level) with
            | true -> var_lower in_ ~to_:hole_level
            | false -> return ()))

  let rec unify_term ~expected ~received =
    (* TODO: use those locations for something? *)
    let (TTerm { loc = _; desc = expected_desc; type_ = expected_type }) =
      expected
    in
    let (TTerm { loc = _; desc = received_desc; type_ = received_type }) =
      received
    in
    (* TODO: why is this needed? *)
    unify_type ~expected:expected_type ~received:received_type >>= fun () ->
    unify_desc ~expected:expected_desc ~received:received_desc

  and unify_type ~expected ~received =
    (* TODO: use those locations for something? *)
    let (TType { loc = _; desc = expected }) = expected in
    let (TType { loc = _; desc = received }) = received in
    unify_desc ~expected ~received

  and unify_annot ~expected ~received f =
    let (TAnnot { loc = _; var = expected_var; annot = expected }) = expected in
    let (TAnnot { loc = _; var = received_var; annot = received }) = received in
    unify_type ~expected ~received >>= fun () ->
    with_region @@ fun () ->
    with_var_rigid expected_var @@ fun () ->
    with_var_alias received_var ~of_:expected_var f

  and unify_bind ~expected ~received f =
    let (TBind { loc = _; var = expected_var; value = expected }) = expected in
    let (TBind { loc = _; var = received_var; value = received }) = received in
    unify_term ~expected ~received >>= fun () ->
    with_region @@ fun () ->
    with_var_rigid expected_var @@ fun () ->
    with_var_alias received_var ~of_:expected_var f

  and unify_desc ~expected ~received =
    match (expected, received) with
    | TT_var { var = expected }, _ -> unify_expected_var ~expected ~received
    | _, TT_var { var = received } -> unify_received_var ~expected ~received
    (* TODO: track whenever it is unified and locations, visualizing inference *)
    | ( TT_forall { param = expected_param; return = expected_return },
        TT_forall { param = received_param; return = received_return } ) ->
        unify_annot ~expected:received_param ~received:expected_param
        @@ fun () ->
        unify_type ~expected:expected_return ~received:received_return
    | ( TT_lambda { param = expected_param; return = expected_return },
        TT_lambda { param = received_param; return = received_return } ) ->
        unify_annot ~expected:received_param ~received:expected_param
        @@ fun () ->
        unify_term ~expected:expected_return ~received:received_return
    | ( TT_apply { lambda = expected_lambda; arg = expected_arg },
        TT_apply { lambda = received_lambda; arg = received_arg } ) ->
        unify_term ~expected:expected_lambda ~received:received_lambda
        >>= fun () -> unify_term ~expected:expected_arg ~received:received_arg
    | ( TT_exists { left = expected_left; right = expected_right },
        TT_exists { left = received_left; right = received_right } ) ->
        unify_annot ~expected:expected_left ~received:received_left @@ fun () ->
        unify_annot ~expected:expected_right ~received:received_right
        @@ fun () -> return ()
    | ( TT_pair { left = expected_left; right = expected_right },
        TT_pair { left = received_left; right = received_right } ) ->
        unify_bind ~expected:expected_left ~received:received_left @@ fun () ->
        unify_bind ~expected:expected_right ~received:received_right
        @@ fun () -> return ()
    | ( TT_unpair
          {
            left = expected_left;
            right = expected_right;
            pair = expected_pair;
            return = expected_return;
          },
        TT_unpair
          {
            left = received_left;
            right = received_right;
            pair = received_pair;
            return = received_return;
          } ) ->
        with_region @@ fun () ->
        with_var_rigid expected_left @@ fun () ->
        with_var_alias received_left ~of_:expected_left @@ fun () ->
        with_region @@ fun () ->
        with_var_rigid expected_right @@ fun () ->
        with_var_alias received_right ~of_:expected_right @@ fun () ->
        unify_term ~expected:expected_pair ~received:received_pair >>= fun () ->
        unify_term ~expected:expected_return ~received:received_return
    | ( TT_let { bound = expected_bound; return = expected_return },
        TT_let { bound = received_bound; return = received_return } ) ->
        unify_bind ~expected:expected_bound ~received:received_bound
        @@ fun () ->
        unify_term ~expected:expected_return ~received:received_return
    | ( TT_annot { value = expected_value; annot = expected_annot },
        TT_annot { value = received_value; annot = received_annot } ) ->
        unify_type ~expected:expected_annot ~received:received_annot
        >>= fun () ->
        unify_term ~expected:expected_value ~received:received_value
    | ( ( TT_forall _ | TT_lambda _ | TT_apply _ | TT_exists _ | TT_pair _
        | TT_unpair _ | TT_let _ | TT_annot _ ),
        ( TT_forall _ | TT_lambda _ | TT_apply _ | TT_exists _ | TT_pair _
        | TT_unpair _ | TT_let _ | TT_annot _ ) ) ->
        fail_type_clash ~expected ~received

  and unify_expected_var ~expected ~received =
    match received with
    | TT_var { var = received } -> unify_var ~expected ~received
    | TT_forall _ | TT_lambda _ | TT_apply _ | TT_exists _ | TT_pair _
    | TT_unpair _ | TT_let _ | TT_annot _ -> (
        repr expected >>= function
        | V_rigid { level = _ } -> fail_var_constrained expected ~by_:received
        | V_alias { var = expected } -> unify_expected_var ~expected ~received
        | V_link { type_ = expected } -> unify_desc ~expected ~received
        | V_hole { level = hole_level } ->
            occurs_and_escape_check_desc ~hole_level ~hole_var:expected
              ~in_:received
            >>= fun () -> var_link expected ~to_:received)

  and unify_received_var ~expected ~received =
    match expected with
    | TT_var { var = expected } -> unify_var ~expected ~received
    | TT_forall _ | TT_lambda _ | TT_apply _ | TT_exists _ | TT_pair _
    | TT_unpair _ | TT_let _ | TT_annot _ -> (
        repr received >>= function
        | V_rigid { level = _ } -> fail_var_constrained received ~by_:expected
        | V_alias { var = received } -> unify_received_var ~expected ~received
        | V_link { type_ = received } -> unify_desc ~expected ~received
        | V_hole { level = hole_level } ->
            occurs_and_escape_check_desc ~hole_level ~hole_var:received
              ~in_:expected
            >>= fun () -> var_link received ~to_:expected)

  and unify_var ~expected ~received =
    match Var.equal expected received with
    | true -> return ()
    | false -> (
        repr expected >>= fun expected_info ->
        repr received >>= fun received_info ->
        match (expected_info, received_info) with
        | V_rigid { level = _ }, V_rigid { level = _ } ->
            fail_var_clash ~expected ~received
        | V_alias { var = expected }, _ -> unify_var ~expected ~received
        | _, V_alias { var = received } -> unify_var ~expected ~received
        | V_link { type_ = expected }, _ ->
            unify_received_var ~expected ~received
        | _, V_link { type_ = received } ->
            unify_expected_var ~expected ~received
        (* TODO: is it okay when expected_level == received_level? *)
        | V_hole { level = expected_level }, V_hole { level = received_level }
          -> (
            match Level.(received_level < expected_level) with
            | true -> var_alias expected ~of_:received
            | false -> var_alias received ~of_:expected)
        | V_hole { level = expected_level }, V_rigid { level = received_level }
          -> (
            match Level.(expected_level < received_level) with
            | true -> fail_escape_check received ~to_:expected
            | false -> var_alias expected ~of_:received)
        | V_rigid { level = expected_level }, V_hole { level = received_level }
          -> (
            match Level.(received_level < expected_level) with
            | true -> fail_escape_check expected ~to_:received
            | false -> var_alias received ~of_:expected))
end

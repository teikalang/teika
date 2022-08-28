open Type

exception Var_clash of { expected : Var.t; received : Var.t }
exception Type_clash of { expected : type_; received : type_ }
exception Not_a_function of { funct : type_ }
exception Not_an_extractable_pair of { pair : type_ }
exception Not_a_wrapped_type of { type_ : type_ }

let rec subst ~from ~to_ type_ =
  let subst type_ = subst ~from ~to_ type_ in
  match type_ with
  | T_type -> t_type
  | T_var { var } -> if Var.equal var from then to_ else type_
  | T_arrow { var; param; return } ->
      let param = subst param in
      let return = if Var.equal var from then return else subst return in
      t_arrow ~var ~param ~return
  | T_pair { left; right } ->
      let left = subst left in
      let right = subst right in
      t_pair ~left ~right
  | T_exists { var; right } ->
      let right = if Var.equal var from then right else subst right in
      t_exists ~var ~right
  | T_alias { type_ } ->
      let type_ = subst type_ in
      t_alias ~type_

(* TODO: this is undecidable on forget *)
let rec subtype ~expected ~received =
  match (expected, received) with
  | T_type, T_type -> ()
  (* TODO: this is very weird *)
  | T_type, T_alias _ -> ()
  | T_var { var = expected }, T_var { var = received } ->
      if Var.equal expected received then ()
      else raise (Var_clash { expected; received })
  | ( T_arrow
        { var = expected_var; param = expected_param; return = expected_return },
      T_arrow
        { var = received_var; param = received_param; return = received_return }
    ) ->
      subtype ~expected:received_param ~received:expected_param;

      let internal =
        match received_param with
        | T_alias { type_ } -> type_
        | _param -> t_var ~var:expected_var
      in
      let expected_return =
        subst ~from:expected_var ~to_:internal expected_return
      in
      let received_return =
        subst ~from:received_var ~to_:internal received_return
      in
      subtype ~expected:expected_return ~received:received_return
  | ( T_pair { left = expected_left; right = expected_right },
      T_pair { left = received_left; right = received_right } ) ->
      subtype ~expected:expected_left ~received:received_left;
      subtype ~expected:expected_right ~received:received_right
  | ( T_exists { var = expected_var; right = expected_right },
      T_exists { var = received_var; right = received_right } ) ->
      let expected_right =
        subst ~from:expected_var ~to_:(t_var ~var:received_var) expected_right
      in
      subtype ~expected:expected_right ~received:received_right
  | ( T_exists { var = expected_var; right = expected_right },
      T_pair
        { left = T_alias { type_ = received_type }; right = received_right } )
    ->
      (* TODO: limit this to achieve decidability *)
      let expected_right =
        subst ~from:expected_var ~to_:received_type expected_right
      in
      subtype ~expected:expected_right ~received:received_right
  | T_pair _, T_exists _ -> failwith "not implemented"
  | T_alias { type_ = expected }, T_alias { type_ = received } ->
      subtype ~expected ~received
  | ( (T_type | T_var _ | T_arrow _ | T_pair _ | T_exists _ | T_alias _),
      (T_type | T_var _ | T_arrow _ | T_pair _ | T_exists _ | T_alias _) ) ->
      raise (Type_clash { expected; received })

let extract ~wrapped:type_ =
  match type_ with
  | T_alias { type_ } -> type_
  | T_type | T_var _ | T_arrow _ | T_pair _ | T_exists _ ->
      raise (Not_a_wrapped_type { type_ })

let apply ~funct ~arg =
  match funct with
  | T_arrow { var; param; return } ->
      subtype ~expected:param ~received:arg;
      let arg =
        (* TODO: super hacky *)
        match param with
        | T_type | T_alias _ -> extract ~wrapped:arg
        | T_var _ | T_arrow _ | T_pair _ | T_exists _ -> arg
      in
      subst ~from:var ~to_:arg return
  | T_type | T_var _ | T_pair _ | T_exists _ | T_alias _ ->
      raise (Not_a_function { funct })

let unpair ~pair =
  match pair with
  | T_pair { left; right } -> (left, right)
  | _ -> raise (Not_an_extractable_pair { pair })

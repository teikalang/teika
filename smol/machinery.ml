open Type

exception Var_clash of { expected : Var.t; received : Var.t }
exception Type_clash of { expected : type_; received : type_ }
exception Not_a_function of { funct : type_ }
exception Not_a_type of { type_ : type_ }

let rec subst ~from ~to_ type_ =
  let subst type_ = subst ~from ~to_ type_ in
  match type_ with
  | T_var { var } -> if Var.equal var from then to_ else type_
  | T_arrow { param; return } ->
      let param = subst param in
      let return = subst return in
      t_arrow ~param ~return
  | T_forall { var; return } ->
      let return = if Var.equal var from then return else subst return in
      t_forall ~var ~return
  | T_pair { left; right } ->
      let left = subst left in
      let right = subst right in
      t_pair ~left ~right
  | T_exists { var; right } ->
      let right = if Var.equal var from then right else subst right in
      t_exists ~var ~right
  | T_type { type_ } ->
      let type_ = subst type_ in
      t_type ~type_

(* TODO: this is undecidable on forget *)
let rec subtype ~expected ~received =
  match (expected, received) with
  | T_var { var = expected }, T_var { var = received } ->
      if Var.equal expected received then ()
      else raise (Var_clash { expected; received })
  | ( T_arrow { param = expected_param; return = expected_return },
      T_arrow { param = received_param; return = received_return } ) ->
      subtype ~expected:received_param ~received:expected_param;
      subtype ~expected:expected_return ~received:received_return
  | ( T_forall { var = expected_var; return = expected_return },
      T_forall { var = received_var; return = received_return } ) ->
      let received_return =
        subst ~from:received_var ~to_:(t_var ~var:expected_var) received_return
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
      T_pair { left = T_type { type_ = received_type }; right = received_right }
    ) ->
      (* TODO: limit this to achieve decidability *)
      let expected_right =
        subst ~from:expected_var ~to_:received_type expected_right
      in
      subtype ~expected:expected_right ~received:received_right
  | T_pair _, T_exists _ -> failwith "not implemented"
  | T_type { type_ = expected }, T_type { type_ = received } ->
      subtype ~expected ~received
  | ( (T_var _ | T_arrow _ | T_forall _ | T_pair _ | T_exists _ | T_type _),
      (T_var _ | T_arrow _ | T_forall _ | T_pair _ | T_exists _ | T_type _) ) ->
      raise (Type_clash { expected; received })

let extract_type ~wrapped:type_ =
  match type_ with
  | T_type { type_ } -> type_
  | T_var _ | T_forall _ | T_pair _ | T_exists _ | T_arrow _ ->
      raise (Not_a_type { type_ })

let apply ~funct ~arg =
  match funct with
  | T_arrow { param; return } ->
      subtype ~expected:param ~received:arg;
      return
  | T_forall { var; return } ->
      let type_ = extract_type ~wrapped:arg in
      subst ~from:var ~to_:type_ return
  | _ -> raise (Not_a_function { funct })

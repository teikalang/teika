(* TODO: remove all failwith *)

module Machinery = struct
  open Stree

  let rec open_term ~from ~to_ term =
    let open_term ~from term = open_term ~from ~to_ term in
    let open_ty_pat ~from pat = open_ty_pat ~from ~to_ pat in
    let open_pat ~from pat = open_pat ~from ~to_ pat in
    match term with
    | ST_loc { term; loc } ->
        let term = open_term ~from term in
        ST_loc { term; loc }
    | ST_free_var { level } -> ST_free_var { level }
    | ST_bound_var { index } -> (
        match Index.equal from index with
        | true -> to_
        | false -> ST_bound_var { index })
    | ST_forall { param; return } ->
        let param = open_ty_pat ~from param in
        let return =
          (* TODO: what if pairs in patterns *)
          let from = Index.next from in
          open_term ~from return
        in
        ST_forall { param; return }
    | ST_lambda { param; return } ->
        let param = open_ty_pat ~from param in
        let return =
          let from = Index.next from in
          open_term ~from return
        in
        ST_lambda { param; return }
    | ST_apply { lambda; arg } ->
        let lambda = open_term ~from lambda in
        let arg = open_term ~from arg in
        ST_apply { lambda; arg }
    | ST_self { self; body } ->
        let self = open_pat ~from self in
        let body =
          let from = Index.next from in
          open_term ~from body
        in
        ST_self { self; body }
    | ST_fix { self; body } ->
        let self = open_ty_pat ~from self in
        let body =
          let from = Index.next from in
          open_term ~from body
        in
        ST_fix { self; body }
    | ST_unroll { term } ->
        let term = open_term ~from term in
        ST_unroll { term }
    | ST_let { bound; value; return } ->
        let bound = open_ty_pat ~from bound in
        let value = open_term ~from value in
        let return =
          let from = Index.next from in
          open_term ~from return
        in
        ST_let { bound; value; return }
    | ST_annot { term; annot } ->
        let term = open_term ~from term in
        let annot = open_term ~from annot in
        ST_annot { term; annot }

  and open_ty_pat ~from ~to_ pat =
    let (SP_typed { pat; type_ }) = pat in
    let pat = open_pat ~from ~to_ pat in
    let type_ = open_term ~from ~to_ type_ in
    SP_typed { pat; type_ }

  and open_pat ~from ~to_ pat =
    match pat with
    | SP_loc { pat; loc } ->
        let pat = open_pat ~from ~to_ pat in
        SP_loc { pat; loc }
    | SP_var { var } -> SP_var { var }
    | SP_erasable { pat } ->
        let pat = open_pat ~from ~to_ pat in
        SP_erasable { pat }
    | SP_annot { pat; annot } ->
        let pat = open_pat ~from ~to_ pat in
        let annot = open_term ~from ~to_ annot in
        SP_annot { pat; annot }

  let open_term ~to_ term = open_term ~from:Index.zero ~to_ term

  let rec close_term ~from ~to_ term =
    let close_term ~to_ term = close_term ~from ~to_ term in
    let close_ty_pat ~to_ pat = close_ty_pat ~from ~to_ pat in
    let close_pat ~to_ pat = close_pat ~from ~to_ pat in
    match term with
    | ST_loc { term; loc } ->
        let term = close_term ~to_ term in
        ST_loc { term; loc }
    | ST_free_var { level } -> (
        match Level.equal from level with
        | true -> ST_bound_var { index = to_ }
        | false -> ST_free_var { level })
    | ST_bound_var { index } -> ST_bound_var { index }
    | ST_forall { param; return } ->
        let param = close_ty_pat ~to_ param in
        let return =
          let to_ = Index.next to_ in
          close_term ~to_ return
        in
        ST_forall { param; return }
    | ST_lambda { param; return } ->
        let param = close_ty_pat ~to_ param in
        let return =
          let to_ = Index.next to_ in
          close_term ~to_ return
        in
        ST_lambda { param; return }
    | ST_apply { lambda; arg } ->
        let lambda = close_term ~to_ lambda in
        let arg = close_term ~to_ arg in
        ST_apply { lambda; arg }
    | ST_self { self; body } ->
        let self = close_pat ~to_ self in
        let body =
          let to_ = Index.next to_ in
          close_term ~to_ body
        in
        ST_self { self; body }
    | ST_fix { self; body } ->
        let self = close_ty_pat ~to_ self in
        let body =
          let to_ = Index.next to_ in
          close_term ~to_ body
        in
        ST_fix { self; body }
    | ST_unroll { term } ->
        let term = close_term ~to_ term in
        ST_unroll { term }
    | ST_let { bound; value; return } ->
        let bound = close_ty_pat ~to_ bound in
        let value = close_term ~to_ value in
        let return =
          let to_ = Index.next to_ in
          close_term ~to_ return
        in
        ST_let { bound; value; return }
    | ST_annot { term; annot } ->
        let term = close_term ~to_ term in
        let annot = close_term ~to_ annot in
        ST_annot { term; annot }

  and close_ty_pat ~from ~to_ pat =
    let (SP_typed { pat; type_ }) = pat in
    let pat = close_pat ~from ~to_ pat in
    let type_ = close_term ~from ~to_ type_ in
    SP_typed { pat; type_ }

  and close_pat ~from ~to_ pat =
    match pat with
    | SP_loc { pat; loc } ->
        let pat = close_pat ~from ~to_ pat in
        SP_loc { pat; loc }
    | SP_var { var } -> SP_var { var }
    | SP_erasable { pat } ->
        let pat = close_pat ~from ~to_ pat in
        SP_erasable { pat }
    | SP_annot { pat; annot } ->
        let pat = close_pat ~from ~to_ pat in
        let annot = close_term ~from ~to_ annot in
        SP_annot { pat; annot }

  (* TODO: expansion of unroll *)
  let rec expand_head_term term =
    match term with
    (* TODO: use this loc during equality?*)
    | ST_loc { term; loc = _ } -> expand_head_term term
    (* TODO: equality expansion *)
    | ST_free_var _ as term -> term
    | ST_bound_var _ as term -> term
    | ST_forall _ as term -> term
    | ST_lambda _ as term -> term
    | ST_apply { lambda; arg } -> (
        match expand_head_term lambda with
        (* TODO: use pattern when moving to subst *)
        | ST_lambda { param = _; return } -> open_term ~to_:arg return
        | lambda -> ST_apply { lambda; arg })
    | ST_self _ as term -> term
    | ST_fix _ as term -> term
    | ST_unroll _ as term -> term
    (* TODO: use pattern when moving to subst *)
    | ST_let { bound = _; value; return } ->
        expand_head_term @@ open_term ~to_:value return
    | ST_annot { term; annot = _ } -> expand_head_term term

  (* TODO: document multi step equality *)
  let rec equal_term ~received ~expected =
    match received == expected with
    | true -> ()
    | false -> equal_term_structural ~received ~expected

  and equal_term_structural ~received ~expected =
    let received = expand_head_term received in
    let expected = expand_head_term expected in
    match (received, expected) with
    (* TODO: locs? *)
    | ST_loc { term = received; loc = _ }, expected
    | received, ST_loc { term = expected; loc = _ } ->
        equal_term ~received ~expected
    | ST_free_var { level = received }, ST_free_var { level = expected } -> (
        match Level.equal received expected with
        | true -> ()
        | false -> failwith "free var clash")
    | ST_bound_var { index = received }, ST_bound_var { index = expected } -> (
        match Index.equal received expected with
        | true -> ()
        | false -> failwith "bound var clash")
    | ( ST_forall { param = received_param; return = received_return },
        ST_forall { param = expected_param; return = expected_return } ) ->
        let () =
          equal_ty_pat ~received:received_param ~expected:expected_param
        in
        equal_term ~received:received_return ~expected:expected_return
    | ( ST_lambda { param = received_param; return = received_return },
        ST_lambda { param = expected_param; return = expected_return } ) ->
        let () =
          equal_ty_pat ~received:received_param ~expected:expected_param
        in
        equal_term ~received:received_return ~expected:expected_return
    | ( ST_apply { lambda = received_lambda; arg = received_arg },
        ST_apply { lambda = expected_lambda; arg = expected_arg } ) ->
        let () =
          equal_term ~received:received_lambda ~expected:expected_lambda
        in
        equal_term ~received:received_arg ~expected:expected_arg
    | ( ST_self { self = received_self; body = received_body },
        ST_self { self = expected_self; body = expected_body } ) ->
        let () = equal_pat ~received:received_self ~expected:expected_self in
        equal_term ~received:received_body ~expected:expected_body
    | ( ST_fix { self = received_self; body = received_body },
        ST_fix { self = expected_self; body = expected_body } ) ->
        let () = equal_ty_pat ~received:received_self ~expected:expected_self in
        equal_term ~received:received_body ~expected:expected_body
    | ST_unroll { term = received }, ST_unroll { term = expected } ->
        equal_term ~received ~expected
    (* TODO: document why let here *)
    | ( ST_let
          {
            bound = received_bound;
            value = received_value;
            return = received_return;
          },
        ST_let
          {
            bound = expected_bound;
            value = expected_value;
            return = expected_return;
          } ) ->
        let () =
          equal_ty_pat ~received:received_bound ~expected:expected_bound
        in
        let () = equal_term ~received:received_value ~expected:expected_value in
        equal_term ~received:received_return ~expected:expected_return
    (* TODO: document why annot here *)
    (* TODO: should check also for annot equality? *)
    | ST_annot { term = received; annot = _ }, expected
    | received, ST_annot { term = expected; annot = _ } ->
        equal_term ~received ~expected
    | ( ( ST_free_var _ | ST_bound_var _ | ST_forall _ | ST_lambda _
        | ST_apply _ | ST_self _ | ST_fix _ | ST_unroll _ | ST_let _ ),
        ( ST_free_var _ | ST_bound_var _ | ST_forall _ | ST_lambda _
        | ST_apply _ | ST_self _ | ST_fix _ | ST_unroll _ | ST_let _ ) ) ->
        failwith "type clash"

  and equal_ty_pat ~received ~expected =
    let (SP_typed { pat = received_pat; type_ = received_type }) = received in
    let (SP_typed { pat = expected_pat; type_ = expected_type }) = expected in
    let () = equal_pat ~received:received_pat ~expected:expected_pat in
    equal_term ~received:received_type ~expected:expected_type

  and equal_pat ~received ~expected =
    (* TODO: normalize pattern *)
    (* TODO: check pat? *)
    match (received, expected) with
    (* TODO: locs *)
    | SP_loc { pat = received; loc = _ }, expected
    | received, SP_loc { pat = expected; loc = _ } ->
        equal_pat ~received ~expected
    | SP_var { var = _ }, SP_var { var = _ } -> ()
    | SP_erasable { pat = received }, SP_erasable { pat = expected } ->
        equal_pat ~received ~expected
    | SP_annot { pat = received; annot = _ }, expected
    | received, SP_annot { pat = expected; annot = _ } ->
        equal_pat ~received ~expected
    | (SP_var _ | SP_erasable _), (SP_var _ | SP_erasable _) ->
        failwith "pattern clash"

  let typeof_pat pat =
    let (SP_typed { pat; type_ }) = pat in
    let rec is_erasable pat =
      match pat with
      (* TODO: weird *)
      | SP_loc { pat; loc = _ } -> is_erasable pat
      | SP_var { var = _ } -> false
      | SP_erasable { pat = _ } -> true
      | SP_annot { pat; annot = _ } -> is_erasable pat
    in
    (type_, `Erasability (is_erasable pat))
end

module Assume = struct
  (* TODO: document assumption mode *)
  open Syntax
  open Ltree
  open Stree

  (* TODO: linearity on assume? *)
  module Context : sig
    type 'a context
    type 'a t = 'a context

    (* monad *)
    (* TODO: this should not be exposed *)
    val run :
      level:Level.t ->
      loc:Location.t ->
      names:Level.t Name.Map.t ->
      (unit -> 'a context) ->
      'a

    val pure : 'a -> 'a context
    val ( let* ) : 'a context -> ('a -> 'b context) -> 'b context

    (* locs *)
    val with_loc : Location.t -> (unit -> 'a context) -> 'a context

    (* vars *)
    val enter : Name.t -> (unit -> 'a context) -> 'a context
    val lookup : Name.t -> Level.t context

    (* machinery *)
    val close_term : term -> term context
  end = struct
    open Machinery

    (* TODO: names map vs names list / stack *)
    type 'a context =
      level:Level.t -> loc:Location.t -> names:Level.t Name.Map.t -> 'a

    type 'a t = 'a context

    let run ~level ~loc ~names f = f () ~level ~loc ~names
    let pure x ~level:_ ~loc:_ ~names:_ = x

    let ( let* ) ctx f ~level ~loc ~names =
      f (ctx ~level ~loc ~names) ~level ~loc ~names

    let with_loc loc f ~level ~loc:_ ~names = f () ~level ~loc ~names

    let enter name f ~level ~loc ~names =
      let level = Level.next level in
      let names = Name.Map.add name level names in
      f () ~level ~loc ~names

    let lookup name ~level:_ ~loc:_ ~names =
      match Name.Map.find_opt name names with
      | Some level -> level
      | None -> failwith "unknown name"

    let close_term term ~level ~loc:_ ~names:_ =
      close_term ~from:level ~to_:Index.zero term
  end

  open Context

  let rec assume_term term =
    match term with
    | LT_loc { term; loc } ->
        let* term = with_loc loc @@ fun () -> assume_term term in
        pure @@ ST_loc { term; loc }
    | LT_var { var } ->
        let* level = lookup var in
        pure @@ ST_free_var { level }
    | LT_extension _ -> failwith "extension not supported"
    | LT_forall { param; return } ->
        let* param, enter = assume_ty_pat param in
        let* return = enter @@ fun () -> assume_term return in
        pure @@ ST_forall { param; return }
    | LT_lambda { param; return } ->
        let* param, enter = assume_ty_pat param in
        let* return = enter @@ fun () -> assume_term return in
        pure @@ ST_lambda { param; return }
    | LT_apply { lambda; arg } ->
        let* lambda = assume_term lambda in
        let* arg = assume_term arg in
        pure @@ ST_apply { lambda; arg }
    | LT_self { self; body } -> assume_self ~self ~body
    | LT_fix { self; body } -> assume_fix ~self ~body
    | LT_unroll { term } ->
        let* term = assume_term term in
        pure @@ ST_unroll { term }
    | LT_let { bound; return } ->
        (* TODO: assume bind? *)
        (* TODO: use this loc *)
        let (LBind { loc = _; pat = bound; value }) = bound in
        (* TODO: should let always be typed here *)
        let* bound, enter = assume_ty_pat bound in
        let* value = assume_term value in
        let* return = enter @@ fun () -> assume_term return in
        pure @@ ST_let { bound; value; return }
    | LT_annot { term; annot } ->
        let* annot = assume_term annot in
        let* term = assume_term term in
        pure @@ ST_annot { term; annot }
    | LT_string _ -> failwith "string not supported"

  and assume_self ~self ~body =
    let* self, enter = assume_pat self in
    let* body = enter @@ fun () -> assume_term body in
    pure @@ ST_self { self; body }

  and assume_fix ~self ~body =
    let* self, enter = assume_ty_pat self in
    let* body = enter @@ fun () -> assume_term body in
    pure @@ ST_fix { self; body }

  and assume_ty_pat pat =
    let wrap ~enter ~type_ pat = pure @@ (SP_typed { pat; type_ }, enter) in
    match pat with
    | LP_loc { pat; loc } ->
        let* SP_typed { pat; type_ }, enter =
          with_loc loc @@ fun () -> assume_ty_pat pat
        in
        wrap ~enter ~type_ @@ SP_loc { pat; loc }
    | LP_var _ -> failwith "missing annotation"
    | LP_unroll _ -> failwith "unroll patterns are not supported"
    | LP_erasable _ ->
        let* SP_typed { pat; type_ }, enter = assume_ty_pat pat in
        wrap ~enter ~type_ @@ SP_erasable { pat }
    | LP_annot { pat; annot } ->
        let* annot = assume_term annot in
        let* pat, enter = assume_pat pat in
        wrap ~enter ~type_:annot @@ SP_annot { pat; annot }

  and assume_pat pat =
    (* TODO: with should do auto close *)
    match pat with
    | LP_loc { pat; loc } ->
        with_loc loc @@ fun () ->
        let* pat, enter = assume_pat pat in
        let pat = SP_loc { pat; loc } in
        pure @@ (pat, enter)
    | LP_var { var } ->
        let enter k =
          enter var @@ fun () ->
          (* TODO: better place or name for close term*)
          let* term = k () in
          close_term term
        in
        pure @@ (SP_var { var }, enter)
    | LP_erasable { pat } ->
        let* pat, enter = assume_pat pat in
        pure @@ (SP_erasable { pat }, enter)
    | LP_unroll _ -> failwith "unroll patterns are not supported"
    | LP_annot { pat; annot } ->
        let* annot = assume_term annot in
        let* pat, enter = assume_pat pat in
        let pat = SP_annot { pat; annot } in
        pure @@ (pat, enter)
end

open Syntax
open Ltree
open Stree
open Machinery

(* TODO: this being hard coded is bad *)
let st_type = ST_free_var { level = Level.zero }

module Context : sig
  type 'a context
  type 'a t = 'a context

  (* monad *)
  val run : (unit -> 'a context) -> 'a
  val pure : 'a -> 'a context
  val ( let* ) : 'a context -> ('a -> 'b context) -> 'b context

  (* locs *)
  val with_loc : Location.t -> (unit -> 'a context) -> 'a context

  (* mode *)
  val enter_erasable_zone : (unit -> 'a context) -> 'a context

  (* vars *)
  val enter :
    Name.t -> erasable:bool -> type_:term -> (unit -> 'a context) -> 'a context

  val lookup : Name.t -> ([ `Type of term ] * Level.t) context

  (* machinery *)
  val assume_self : self:Ltree.pat -> body:Ltree.term -> term context
  val assume_fix : self:Ltree.pat -> body:Ltree.term -> term context
  val subst_term : to_:term -> term -> term context
  val open_term : term -> term context
  val close_term : term -> term context
end = struct
  open Machinery

  type status = Var_pending | Var_used

  (* TODO: names map vs names list / stack *)
  (* TODO: vars map vs vars list / stack *)
  type 'a context =
    level:Level.t ->
    loc:Location.t ->
    names:Level.t Name.Map.t ->
    types:term Level.Map.t ->
    grades:status Level.Map.t ->
    status Level.Map.t * 'a

  type 'a t = 'a context

  let run k =
    let level = Level.(next zero) in
    (* TODO: better than none here? *)
    let loc = Location.none in
    (* TODO: move this to Name? *)
    let names = Name.Map.(add (Name.make "Type") Level.zero empty) in
    let types = Level.Map.(add Level.zero st_type empty) in
    let grades = Level.Map.(add Level.zero Var_used empty) in
    let _grades, x = k () ~level ~loc ~names ~types ~grades in
    (* TODO: check grades here *)
    x

  let pure x ~level:_ ~loc:_ ~names:_ ~types:_ ~grades = (grades, x)

  let ( let* ) ctx f ~level ~loc ~names ~types ~grades =
    let grades, x = ctx ~level ~loc ~names ~types ~grades in
    f x ~level ~loc ~names ~types ~grades

  let with_loc loc f ~level ~loc:_ ~names ~types ~grades =
    f () ~level ~loc ~names ~types ~grades

  let enter_erasable_zone f ~level ~loc ~names ~types ~grades =
    let _grades, x = f () ~level ~loc ~names ~types ~grades:Level.Map.empty in
    (* TODO: check grades to be empty here *)
    (grades, x)

  let enter_linear name ~type_ f ~level ~loc ~names ~types ~grades =
    let level = Level.next level in
    let names = Name.Map.add name level names in
    let types = Level.Map.add level type_ types in
    let grades = Level.Map.add level Var_pending grades in
    let grades, x = f () ~level ~loc ~names ~types ~grades in
    match Level.Map.find_opt level grades with
    | Some Var_pending -> failwith "variable not used"
    | Some Var_used -> (Level.Map.remove level grades, x)
    | None -> failwith "invariant violated"

  let enter_erasable name ~type_ f ~level ~loc ~names ~types ~grades =
    let level = Level.next level in
    let names = Name.Map.add name level names in
    let types = Level.Map.add level type_ types in
    (* TODO: explain why it enters variable as used *)
    (* TODO: Var_erasable? *)
    let grades = Level.Map.add level Var_used grades in
    let grades, x = f () ~level ~loc ~names ~types ~grades in
    (Level.Map.remove level grades, x)

  let enter name ~erasable ~type_ f ~level ~loc ~names ~types ~grades =
    match erasable with
    | true -> enter_erasable name ~type_ f ~level ~loc ~names ~types ~grades
    | false -> enter_linear name ~type_ f ~level ~loc ~names ~types ~grades

  let lookup name ~level:_ ~loc:_ ~names ~types ~grades =
    match Name.Map.find_opt name names with
    | Some level -> (
        match Level.Map.find_opt level types with
        | Some type_ -> (
            match Level.Map.find_opt level grades with
            | Some Var_pending ->
                let grades = Level.Map.add level Var_used grades in
                (grades, (`Type type_, level))
            | Some Var_used -> failwith "var already used"
            | None ->
                (* removed by erasable zone *)
                (grades, (`Type type_, level)))
        | None -> failwith "vars invariant violation")
    | None -> failwith "unknown name"

  let assume_self ~self ~body ~level ~loc ~names ~types:_ ~grades =
    let x =
      let open Assume in
      Context.run ~level ~loc ~names @@ fun () -> assume_self ~self ~body
    in
    (grades, x)

  let assume_fix ~self ~body ~level ~loc ~names ~types:_ ~grades =
    let x =
      let open Assume in
      Context.run ~level ~loc ~names @@ fun () -> assume_fix ~self ~body
    in
    (grades, x)

  let subst_term ~to_ term ~level:_ ~loc:_ ~names:_ ~types:_ ~grades =
    (grades, open_term ~to_ term)

  let open_term term ~level ~loc:_ ~names:_ ~types:_ ~grades =
    (grades, open_term ~to_:(ST_free_var { level }) term)

  let close_term term ~level ~loc:_ ~names:_ ~types:_ ~grades =
    (grades, close_term ~from:level ~to_:Index.zero term)
end

open Context

(* TODO: think better about enter pat *)
let rec enter_pat ~erasable pat ~type_ k =
  match pat with
  (* TODO: weird *)
  | SP_loc { pat; loc = _ } -> enter_pat pat ~erasable ~type_ k
  | SP_var { var } -> enter ~erasable var ~type_ k
  | SP_erasable { pat } -> enter_pat ~erasable:true pat ~type_ k
  | SP_annot { pat; annot = _ } -> enter_pat pat ~erasable ~type_ k

let enter_ty_pat ~erasable pat k =
  let (SP_typed { pat; type_ }) = pat in
  enter_pat pat ~erasable ~type_ k

(* TODO: this is clearly bad *)
let enter_erasable_zone_conditional ~erasable k =
  match erasable with true -> enter_erasable_zone k | false -> k ()

let rec infer_term term =
  let wrap ~type_ term = pure @@ ST_typed { term; type_ } in
  match term with
  | LT_loc { term; loc } ->
      let* (ST_typed { term; type_ }) =
        with_loc loc @@ fun () -> infer_term term
      in
      wrap ~type_ @@ ST_loc { term; loc }
  | LT_var { var } ->
      let* `Type type_, level = lookup var in
      wrap ~type_ @@ ST_free_var { level }
  | LT_extension _ -> failwith "extension not supported"
  | LT_forall { param; return } ->
      let* param = infer_ty_pat param in
      let* return =
        enter_erasable_zone @@ fun () ->
        check_term_with_ty_pat ~erasable:true param return ~expected:st_type
      in
      wrap ~type_:st_type @@ ST_forall { param; return }
  | LT_lambda { param; return } ->
      let* param = infer_ty_pat param in
      let* (ST_typed { term = return; type_ = return_type }) =
        infer_term_with_ty_pat ~erasable:false param return
      in
      let type_ = ST_forall { param; return = return_type } in
      wrap ~type_ @@ ST_lambda { param; return }
  | LT_apply { lambda; arg } -> (
      let* (ST_typed { term = lambda; type_ = forall }) = infer_term lambda in
      (* TODO: maybe machinery to eliminate forall *)
      match expand_head_term forall with
      | ST_forall { param; return } ->
          let* arg =
            let expected, `Erasability erasable = typeof_pat param in
            enter_erasable_zone_conditional ~erasable @@ fun () ->
            check_term arg ~expected
          in
          let* type_ = subst_term ~to_:arg return in
          wrap ~type_ @@ ST_apply { lambda; arg }
      (* TODO: expand cases *)
      | _ -> failwith "expected forall")
  | LT_self { self; body } ->
      let* assumed_self = assume_self ~self ~body in
      let* self = check_pat self ~expected:assumed_self in
      let* body =
        enter_erasable_zone @@ fun () ->
        check_term_with_pat ~erasable:true self ~type_:assumed_self body
          ~expected:st_type
      in
      let self = ST_self { self; body } in
      (* this equality is about peace of mind *)
      let () = equal_term ~received:self ~expected:assumed_self in
      wrap ~type_:st_type @@ self
  | LT_fix { self; body } ->
      let* self = infer_ty_pat self in
      let type_, `Erasability erasable = typeof_pat self in
      let* body =
        enter_erasable_zone_conditional ~erasable @@ fun () ->
        check_term_with_ty_pat ~erasable:false self body ~expected:type_
      in
      wrap ~type_ @@ ST_fix { self; body }
  | LT_unroll { term } -> (
      (* TODO: rename to fix *)
      let* (ST_typed { term; type_ = self }) = infer_term term in
      (* TODO: maybe machinery to eliminate forall *)
      match expand_head_term self with
      | ST_self { self = _; body } ->
          let* type_ = subst_term ~to_:term body in
          wrap ~type_ @@ ST_unroll { term }
      (* TODO: expand cases *)
      | _ -> failwith "expected self")
  | LT_let { bound; return } ->
      (* TODO: check bind? *)
      (* TODO: use this loc *)
      let (LBind { loc = _; pat = bound; value }) = bound in
      (* TODO: remove need for typing of let *)
      let* bound = infer_ty_pat bound in
      let* value =
        let value_type, `Erasability erasable = typeof_pat bound in
        enter_erasable_zone_conditional ~erasable @@ fun () ->
        check_term value ~expected:value_type
      in
      let* (ST_typed { term = return; type_ = return_type }) =
        infer_term_with_ty_pat ~erasable:false bound return
      in
      (* TODO: could use let at type level *)
      let* type_ = subst_term ~to_:value return_type in
      wrap ~type_ @@ ST_let { bound; value; return }
  | LT_annot { term; annot } ->
      let* annot =
        enter_erasable_zone @@ fun () -> check_term annot ~expected:st_type
      in
      let* term = check_term term ~expected:annot in
      wrap ~type_:annot @@ ST_annot { term; annot }
  | LT_string _ -> failwith "string not supported"

and check_term term ~expected =
  (* TODO: check term equality for nested annot ((x : A) : B)? *)
  (* TODO: propagate *)
  match (term, expand_head_term expected) with
  | LT_loc { term; loc }, expected ->
      let* term = with_loc loc @@ fun () -> check_term term ~expected in
      pure @@ ST_loc { term; loc }
  | ( LT_lambda { param; return },
      ST_forall { param = expected_param; return = expected_return } ) ->
      let* param =
        (* TODO: use this erasable? *)
        let expected_param_type, `Erasability _erasable =
          typeof_pat expected_param
        in
        check_ty_pat param ~expected:expected_param_type
      in
      let () = equal_ty_pat ~received:param ~expected:expected_param in
      let* return =
        check_term_with_ty_pat ~erasable:false param return
          ~expected:expected_return
      in
      pure @@ ST_lambda { param; return }
  | ( LT_fix { self; body },
      (ST_self { self = expected_self; body = expected_body } as expected) ) ->
      let* self = check_ty_pat self ~expected in
      let () =
        let (SP_typed { pat = self; type_ = _ }) = self in
        equal_pat ~received:self ~expected:expected_self
      in
      let* body =
        check_term_with_ty_pat ~erasable:false self body ~expected:expected_body
      in
      pure @@ ST_fix { self; body }
  | term, expected ->
      let* (ST_typed { term; type_ = received }) = infer_term term in
      let () = equal_term ~received ~expected in
      pure term

and infer_ty_pat pat =
  let wrap ~type_ pat = pure @@ SP_typed { pat; type_ } in
  match pat with
  | LP_loc { pat; loc } ->
      with_loc loc @@ fun () ->
      let* (SP_typed { pat; type_ }) = infer_ty_pat pat in
      wrap ~type_ @@ SP_loc { pat; loc }
  | LP_var _ -> failwith "missing annotation"
  | LP_erasable { pat } ->
      let* (SP_typed { pat; type_ }) = infer_ty_pat pat in
      wrap ~type_ @@ SP_erasable { pat }
  | LP_unroll _ -> failwith "unroll patterns are not supported"
  | LP_annot { pat; annot } ->
      let* annot =
        enter_erasable_zone @@ fun () -> check_term annot ~expected:st_type
      in
      check_ty_pat pat ~expected:annot

and check_ty_pat pat ~expected =
  let* pat = check_pat pat ~expected in
  pure @@ SP_typed { pat; type_ = expected }

and check_pat pat ~expected =
  match pat with
  | LP_loc { pat; loc } ->
      with_loc loc @@ fun () ->
      let* pat = check_pat pat ~expected in
      pure @@ SP_loc { pat; loc }
  | LP_var { var } -> pure @@ SP_var { var }
  | LP_erasable { pat } ->
      let* pat = check_pat pat ~expected in
      pure @@ SP_erasable { pat }
  | LP_unroll _ -> failwith "unroll patterns are not supported"
  | LP_annot { pat; annot } ->
      let* annot =
        enter_erasable_zone @@ fun () -> check_term annot ~expected:st_type
      in
      let* pat = check_pat pat ~expected:annot in
      let () = equal_term ~received:annot ~expected in
      pure @@ SP_annot { pat; annot }

and infer_term_with_ty_pat ~erasable pat term =
  enter_ty_pat ~erasable pat @@ fun () ->
  let* (ST_typed { term; type_ }) = infer_term term in
  let* term = close_term term in
  let* type_ = close_term type_ in
  pure @@ ST_typed { term; type_ }

and check_term_with_ty_pat ~erasable pat term ~expected =
  enter_ty_pat ~erasable pat @@ fun () ->
  (* TODO: open and close should probably not be here *)
  let* expected = open_term expected in
  let* term = check_term term ~expected in
  close_term term

and check_term_with_pat ~erasable pat ~type_ term ~expected =
  enter_pat ~erasable pat ~type_ @@ fun () ->
  let* expected = open_term expected in
  let* term = check_term term ~expected in
  close_term term

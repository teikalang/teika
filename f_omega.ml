module Existentials = struct
  (* packed *)

  type show = Ex_show : { content : 'a; show : 'a -> string } -> show

  let value = Ex_show { content = 1; show = Int.to_string }

  let eval_show ex =
    let (Ex_show { content; show }) = ex in
    show content

  (* let extract ex =
     let (Ex_show { content; show = _ }) = ex in
     content *)

  (* well but first-class modules *)

  module type Show = sig
    type t

    val content : t
    val show : t -> string
  end

  let eval_show package =
    let (module S : Show) = package in
    S.show S.content

  let value =
    (module struct
      type t = int

      let content = 1
      let show = Int.to_string
    end : Show)
end

module Equality = struct
  type (_, _) eq = Refl : ('x, 'x) eq

  (* weak, how to derive sym? *)
  let cast : type a b. (a, b) eq -> a -> b =
   fun eq x ->
    let Refl = eq in
    x

  (* this is how I got here *)
  module Subst (M : sig
    type 'a t
  end) =
  struct
    let subst : type a b. (a, b) eq -> a M.t -> b M.t =
     fun eq x ->
      let Refl = eq in
      x
  end

  (* refutation *)
  let bad : type a. (string, int) eq -> a = fun eq -> match eq with _ -> .

  (* fancier *)
  let dependent_types :
      type a. ((a, int) eq, (a, bool) eq) Either.t -> a -> string =
   fun tag content ->
    match tag with
    | Left Refl -> Int.to_string content
    | Right Refl -> Bool.to_string content

  let show_int = dependent_types (Left Refl)
  let show_string = dependent_types (Right Refl)
end

module Show = struct
  (* this is just many equalities *)
  type _ ty =
    | T_int : int ty
    | T_bool : bool ty
    (* but recursive *)
    | T_list : 'a ty -> 'a list ty

  let rec show : type a. a ty -> a -> string =
   fun tag content ->
    match tag with
    | T_int -> Int.to_string content
    | T_bool -> Bool.to_string content
    | T_list el_tag ->
        let serialized_content = List.map (fun el -> show el_tag el) content in
        Format.sprintf "[%s]" (String.concat ", " serialized_content)

  let () = print_endline @@ show T_int 1
  let () = print_endline @@ show (T_list T_bool) [ true; false ]
end

module Dependent = struct
  (* the dream *)

  type ty = T_int | T_bool | T_list of ty

  (* sadly this doesn't work *)
  type 'tag to_type =
    [%d
      match tag with
      | T_int -> int
      | T_bool -> bool
      | T_list el -> list (to_type el)]

  let rec show tag
      (content :
        [%d
          match tag with
          | T_int -> int
          | T_bool -> bool
          | T_list el -> list (to_type el)]) =
    match tag with
    | T_int -> Int.to_string content
    | T_bool -> Bool.to_string content
    | T_list el_tag ->
        let serialized_content = List.map (fun el -> show el_tag el) content in
        Format.sprintf "[%s]" (String.concat ", " serialized_content)

  let () = print_endline @@ show T_int 1
  let () = print_endline @@ show (T_list T_bool) [ true; false ]
end

module But_some_duplication = struct
  (* duplicating the type definition also on the type level *)
  type t_int = |
  type t_bool = |
  type _ t_list = |

  type _ ty =
    | T_int : t_int ty
    | T_bool : t_bool ty
    | T_list : 'el ty -> 'el t_list ty

  module To_type : sig
    type 'ty t

    val unpack_int : t_int t -> int
  end = struct
    type 'ty t

    let unpack_int = assert false
  end

  module M (W : sig
    type ('a, 'b) l
    type ('a, 'b) r
  end) =
  struct
    type ('a, 'b) t = X : (('a, 'b) W.l, ('a, 'b) W.r) t
  end

  module T = M (struct
    type ('a, 'b) l = 'a
    type ('a, 'b) r = 'a
  end)

  let cast : type a b. (a, b) T.t -> a -> b =
   fun rel x ->
    let X = rel in
    x

  (* expand syntax *)
  type (_, _) to_type_w =
    | W_t_int : (t_int, int) to_type_w
    | W_t_bool : (t_bool, bool) to_type_w
    | W_t_list : ('tag_el, 'r) to_type_w -> ('tag_el t_list, 'r list) to_type_w

  (* add the original as a package *)
  type _ to_type = Ex : ('tag, 'r) to_type_w -> 'tag to_type

  (* term level version *)
  let rec to_type : type tag. tag ty -> tag to_type =
   fun tag ->
    match tag with
    | T_int -> Ex W_t_int
    | T_bool -> Ex W_t_bool
    | T_list el ->
        let (Ex el_w) = to_type el in
        Ex (W_t_list el_w)

  let rec show : type tag content. tag ty -> tag To_type.t -> string =
   fun content_type tag content ->
    match tag with
    | T_int ->
        let W_t_int = content_type in
        Int.to_string content
    | T_bool ->
        let W_t_bool = content_type in
        Bool.to_string content
    | T_list el_tag ->
        let (W_t_list el_type) = content_type in
        let serialized_content =
          List.map (fun el -> show el_type el_tag el) content
        in
        Format.sprintf "[%s]" (String.concat ", " serialized_content)

  let () =
    let tag = T_int in
    let (Ex (type content) (w : (_, content) to_type_w)) = to_type tag in
    let content : content =
      let W_t_int = w in
      1
    in
    print_endline @@ show w tag content

  let () =
    let tag = T_list T_bool in
    let (Ex (type content) (w : (_, content) to_type_w)) = to_type tag in
    let content : content =
      let (W_t_list W_t_bool) = w in
      [ true; false ]
    in
    print_endline @@ show w tag content
end

module Compute_anything = struct
  type z = |
  type _ s = |
  type _ nat = Z : z nat | S : 'a nat -> 'a s nat
  type (_, _) eq = Refl : ('x, 'x) eq

  type ('n, 'm, 'r) add_w =
    | W_n_z : (z, 'm, 'm) add_w
    | W_n_s : ('n, 'm, 'r) add_w -> ('n s, 'm, 'r s) add_w

  type ('n, 'm) add = Ex : 'r nat * ('n, 'm, 'r) add_w -> ('n, 'm) add

  let rec add : type n m. n nat -> m nat -> (n, m) add =
   fun n m ->
    match n with
    | Z -> Ex (m, W_n_z)
    | S n ->
        let (Ex (r, w)) = add n m in
        Ex (S r, W_n_s w)

  let five =
    let (Ex (r, w)) = add (S (S Z)) (S (S (S Z))) in
    let (W_n_s (W_n_s W_n_z)) = w in
    (r : z s s s s s nat)

  (* n + z = n *)
  let rec add_z_is_id : type n r. n nat -> (n, z, r) add_w -> (n, r) eq =
   fun n w ->
    match n with
    | Z ->
        let W_n_z = w in
        Refl
    | S n ->
        let (W_n_s w) = w in
        let Refl = add_z_is_id n w in
        Refl
end

type never = |
type ('a, 'b) equal = Refl : ('x, 'x) equal

type ('l, 'r, 'k) bool = {
  b : 'a. (('l, 'k) equal -> 'a) -> (('r, 'k) equal -> 'a) -> 'a;
}

let true_ = { b = (fun l _r -> l Refl) }
let false_ = { b = (fun _l r -> r Refl) }
(*
   type 'k ex_not = {
     b : 'a. (-> 'a) -> 'a
   }
     | Ex_not_true : ('k, 'r, 'k) bool -> 'k ex_not
     | Ex_not_false : ('l, 'k, 'k) bool -> 'k ex_not *)

(* let not (type l r k) (b : (l, r, k) bool) : (r, l, k) bool =
   b.b (fun Refl -> Ex_not_true false_) (fun Refl -> true_) *)

type ('l, 'r) dec_equal =
  | D_refl : ('l, 'l) dec_equal
  | D_neq : (('l, 'r) equal -> never) -> ('l, 'r) dec_equal

let ( let* ) v f = Option.bind v f

module Nat = struct
  type 'a s = Tag_S
  type z = Tag_Z
  type 'a nat = Z : z nat | S : 'a nat -> 'a s nat
  type ('n, 'm) le = Le_Z : (z, 'm) le | Le_S : ('n, 'm) le -> ('n s, 'm s) le
  type ('n, 'm) lt = ('n s, 'm) le
  type ('n, 'm) gt = ('m, 'n) lt

  type ('n, 'm) compare =
    | LT : ('n, 'm) lt -> ('n, 'm) compare
    | EQ : ('n, 'm) compare
    | GT : ('n, 'm) gt -> ('n, 'm) compare

  let rec equal : type n m. n nat -> m nat -> (n, m) dec_equal =
   fun n m ->
    match (n, m) with
    | Z, Z -> D_refl
    | S n, S m -> (
        match equal n m with
        | D_refl -> D_refl
        | D_neq f -> D_neq (fun Refl -> f Refl))
    | S _, Z -> D_neq (fun eq -> match eq with _ -> .)
    | Z, S _ -> D_neq (fun eq -> match eq with _ -> .)

  let rec compare : type n m. n nat -> m nat -> (n, m) compare =
   fun n m ->
    match n with
    | Z -> ( match m with Z -> EQ | S _m -> LT (Le_S Le_Z))
    | S n -> (
        match m with
        | Z -> GT (Le_S Le_Z)
        | S m -> (
            match compare n m with
            | LT lt -> LT (Le_S lt)
            | EQ -> EQ
            | GT gt -> GT (Le_S gt)))

  (* TODO: duplicated *)
  let rec le_or_gt : type n m. n nat -> m nat -> ((n, m) le, (n, m) gt) Either.t
      =
   fun n m ->
    match n with
    | Z -> Left Le_Z
    | S n -> (
        match m with
        | Z -> Right (Le_S Le_Z)
        | S m -> (
            match le_or_gt n m with
            | Left le -> Left (Le_S le)
            | Right gt -> Right (Le_S gt)))

  type ('x, 'n, 'm) add =
    | W_add_n_z : ('m, z, 'm) add
    | W_add_n_s : ('x, 'n, 'm) add -> ('x s, 'n s, 'm) add

  type ('n, 'm) p_add = P_add : ('x, 'n, 'm) add * 'x nat -> ('n, 'm) p_add

  let rec add : type n m. n nat -> m nat -> (n, m) p_add =
   fun n m ->
    match n with
    | Z -> P_add (W_add_n_z, m)
    | S n ->
        let (P_add (w, x)) = add n m in
        P_add (W_add_n_s w, S x)
end

(* type level *)
module Type = struct
  open Nat

  type ('ctx, 'n, 'k) ty_index = Tag_index
  type ('ctx, 'n, 'k) val_index = Tag_index
  type 'ctx append = Tag_append
  type ('ctx, 'n) lookup = Tag_lookup

  (* type level component *)
  type 'n t_var = Tag_var
  type 'body t_forall = Tag_forall
  type ('param, 'body) t_arrow = Tag_arrow

  type 'w type_ =
    (* STLC *)
    | T_arrow : 'param type_ * 'body type_ -> ('param, 'body) t_arrow type_
    (* F *)
    | T_var : 'n nat -> 'n t_var type_
    | T_forall : 'body type_ -> 'body t_forall type_

  let rec equal : type l r. l type_ -> r type_ -> (l, r) dec_equal =
   fun l r ->
    match (l, r) with
    | T_arrow (param_l, body_l), T_arrow (param_r, body_r) -> (
        match equal param_l param_r with
        | D_refl -> (
            match equal body_l body_r with
            | D_refl -> D_refl
            | D_neq f -> D_neq (fun Refl -> f Refl))
        | D_neq f -> D_neq (fun Refl -> f Refl))
    | T_var l, T_var r -> (
        match Nat.equal l r with
        | D_refl -> D_refl
        | D_neq f -> D_neq (fun Refl -> f Refl))
    | T_forall l, T_forall r -> (
        match equal l r with
        | D_refl -> D_refl
        | D_neq f -> D_neq (fun Refl -> f Refl))
    | T_arrow _, T_var _ -> D_neq (fun eq -> match eq with _ -> .)
    | T_arrow _, T_forall _ -> D_neq (fun eq -> match eq with _ -> .)
    | T_var _, T_arrow _ -> D_neq (fun eq -> match eq with _ -> .)
    | T_var _, T_forall _ -> D_neq (fun eq -> match eq with _ -> .)
    | T_forall _, T_var _ -> D_neq (fun eq -> match eq with _ -> .)
    | T_forall _, T_arrow _ -> D_neq (fun eq -> match eq with _ -> .)

  type ('r, 'by, 'depth, 'w) shift =
    | W_shift_arrow :
        ('param_r, 'by, 'depth, 'param) shift
        * ('body_r, 'by, 'depth, 'body) shift
        -> ( ('param_r, 'body_r) t_arrow,
             'by,
             'depth,
             ('param, 'body) t_arrow )
           shift
    | W_shift_var_free :
        ('depth, 'n) le * ('x, 'n, 'by) add
        -> ('x t_var, 'by, 'depth, 'n t_var) shift
    | W_shift_var_bound :
        ('depth, 'n) gt
        -> ('n t_var, 'by, 'depth, 'n t_var) shift
    | W_shift_forall :
        ('body_r, 'by, 'depth s, 'body) shift
        -> ('body_r t_forall, 'by, 'depth, 'body t_forall) shift

  type ('by, 'depth, 'w) p_shift =
    | P_shift :
        ('r, 'by, 'depth, 'w) shift * 'r type_
        -> ('by, 'depth, 'w) p_shift

  let rec shift :
      type by depth w. by nat -> depth nat -> w type_ -> (by, depth, w) p_shift
      =
   fun by depth type_ ->
    match type_ with
    | T_arrow (param, body) ->
        let (P_shift (param_w, param_r)) = shift by depth param in
        let (P_shift (body_w, body_r)) = shift by depth body in
        P_shift (W_shift_arrow (param_w, body_w), T_arrow (param_r, body_r))
    | T_var n -> (
        match le_or_gt depth n with
        | Left le ->
            let (P_add (x_w, x)) = add n by in
            P_shift (W_shift_var_free (le, x_w), T_var x)
        | Right gt -> P_shift (W_shift_var_bound gt, T_var n))
    | T_forall body ->
        let (P_shift (body_w, body_r)) = shift by (S depth) body in
        P_shift (W_shift_forall body_w, T_forall body_r)

  (* subst *)
  type ('r, 'to_, 'depth, 'w) subst =
    | W_subst_arrow :
        ('param_r, 'to_, 'depth, 'param) subst
        * ('body_r, 'to_, 'depth, 'body) subst
        -> ( ('param_r, 'body_r) t_arrow,
             'to_,
             'depth,
             ('param, 'body) t_arrow )
           subst
    | W_subst_var_bound :
        ('depth, 'n) gt
        -> ('n t_var, 'to_, 'depth, 'n t_var) subst
    | W_subst_var_subst :
        ('x, 'depth, z, 'to_) shift
        -> ('x, 'to_, 'depth, 'n t_var) subst
    | W_subst_var_lower :
        ('depth, 'n s) lt
        -> ('n t_var, 'to_, 'depth, 'n s t_var) subst
    | W_subst_forall :
        ('body_r, 'to_, 'depth s, 'body) subst
        -> ('body_r t_forall, 'to_, 'depth, 'body t_forall) subst

  type ('to_, 'depth, 'w) p_subst =
    | P_subst :
        ('r, 'to_, 'depth, 'w) subst * 'r type_
        -> ('to_, 'depth, 'w) p_subst

  let rec subst :
      type r to_ depth w.
      to_ type_ -> depth nat -> w type_ -> (to_, depth, w) p_subst =
   fun to_ depth type_ ->
    match type_ with
    | T_arrow (param, body) ->
        let (P_subst (param_w, param_r)) = subst to_ depth param in
        let (P_subst (body_w, body_r)) = subst to_ depth body in
        P_subst (W_subst_arrow (param_w, body_w), T_arrow (param_r, body_r))
    | T_var n -> (
        match compare depth n with
        | GT gt -> P_subst (W_subst_var_bound gt, T_var n)
        | EQ ->
            let (P_shift (x_w, x)) = shift depth Z to_ in
            P_subst (W_subst_var_subst x_w, x)
        | LT lt -> (
            match n with
            | Z -> ( match lt with _ -> .)
            | S n -> P_subst (W_subst_var_lower lt, T_var n)))
    | T_forall body ->
        let (P_subst (body_w, body_r)) = subst to_ (S depth) body in
        P_subst (W_subst_forall body_w, T_forall body_r)
end

module Term = struct
  open Nat
  open Type

  type 'n e_var = Tag_e_var
  type ('param, 'body) e_lambda = Tag_e_lambda
  type ('lambda, 'arg) e_apply = Tag_e_apply
  type 'body e_forall = Tag_e_forall
  type ('forall, 'arg) e_type_apply = Tag_e_type_apply

  type 'w expr =
    | E_var : 'n nat -> 'n e_var expr
    | E_lambda : 'param type_ * 'body expr -> ('param, 'body) e_lambda expr
    | E_apply : 'lambda expr * 'arg expr -> ('lambda, 'arg) e_apply expr
    | E_forall : 'body expr -> 'body e_forall expr
    | E_type_apply :
        'lambda expr * 'arg type_
        -> ('lambda, 'arg) e_type_apply expr
end

module Context = struct
  open Nat
  open Type

  type c_type = Tag_c_type
  type 'type_ c_value = Tag_c_value

  type 'w context =
    | C_empty : unit context
    | C_value : 'w type_ * 'ctx context -> ('w c_value * 'ctx) context
    | C_type : 'ctx context -> (c_type * 'ctx) context

  type ('ctx, 'n) lookup_type =
    | W_lookup_type : (c_type * 'ctx, z) lookup_type
    | W_lookup_type_skip :
        ('ctx, 'n) lookup_type
        -> (_ * 'ctx, 'n s) lookup_type

  let rec lookup_type :
      type ctx n. ctx context -> n nat -> (ctx, n) lookup_type option =
   fun ctx n ->
    match (ctx, n) with
    | C_empty, _ -> None
    | C_value (_type_, _ctx), Z -> None
    | C_type _, Z -> Some W_lookup_type
    | C_value (_type, ctx), S n ->
        let* w = lookup_type ctx n in
        Some (W_lookup_type_skip w)
    | C_type ctx, S n ->
        let* w = lookup_type ctx n in
        Some (W_lookup_type_skip w)

  type ('type_, 'ctx, 'n) lookup_value =
    | W_lookup_value : ('type_, 'type_ c_value * 'ctx, z) lookup_value
    | W_lookup_value_skip :
        ('type_, 'ctx, 'n) lookup_value
        -> ('type_, _ * 'ctx, 'n s) lookup_value

  type ('ctx, 'n) p_lookup_value =
    | P_lookup_value :
        ('type_, 'ctx, 'n) lookup_value * 'type_ type_
        -> ('ctx, 'n) p_lookup_value

  let rec lookup_value :
      type ctx n. ctx context -> n nat -> (ctx, n) p_lookup_value option =
   fun ctx n ->
    match (ctx, n) with
    | C_empty, _ -> None
    | C_value (type_, _ctx), Z -> Some (P_lookup_value (W_lookup_value, type_))
    | C_type _, Z -> None
    | C_value (_type, ctx), S n ->
        let* (P_lookup_value (w, type_)) = lookup_value ctx n in
        Some (P_lookup_value (W_lookup_value_skip w, type_))
    | C_type ctx, S n ->
        let* (P_lookup_value (w, type_)) = lookup_value ctx n in
        Some (P_lookup_value (W_lookup_value_skip w, type_))
end

open Nat
open Type
open Term
open Context

(* TODO: bad name, check here means check if is a valid type *)
type ('ctx, 'w) check =
  | W_check_arrow :
      ('ctx, 'param) check * ('ctx, 'body) check
      -> ('ctx, ('param, 'body) t_arrow) check
  | W_check_var_zero : (c_type * 'ctx, z t_var) check
  | W_check_var_succ : ('ctx, 'n t_var) check -> (_ * 'ctx, 'n s t_var) check
  | W_check_forall :
      (c_type * 'ctx, 'body) check
      -> ('ctx, 'body t_forall) check

let rec check : type ctx w. ctx context -> w type_ -> (ctx, w) check option =
 fun ctx type_ ->
  match type_ with
  | T_arrow (param, body) ->
      let* param = check ctx param in
      let* body = check ctx body in
      Some (W_check_arrow (param, body))
  | T_var Z -> (
      match ctx with
      | C_empty -> None
      | C_value (_type, _ctx) -> None
      | C_type _ctx -> Some W_check_var_zero)
  | T_var (S n) -> (
      match ctx with
      | C_empty -> None
      | C_type ctx ->
          (* TODO: duplicated *)
          let* w = check ctx @@ T_var n in
          Some (W_check_var_succ w)
      | C_value (_type, ctx) ->
          let* w = check ctx @@ T_var n in
          Some (W_check_var_succ w))
  | T_forall body ->
      let* body_w = check (C_type ctx) body in
      Some (W_check_forall body_w)

type ('ctx, 'expr, 'type_) infer =
  | W_var :
      ('type_, 'ctx, 'n) lookup_value * ('x, 'n, z, 'type_) shift
      -> ('ctx, 'n e_var, 'x) infer
  | W_lambda :
      ('ctx, 'param) check * ('param c_value * 'ctx, 'body, 'body_type) infer
      -> ('ctx, ('param, 'body) e_lambda, ('param, 'body_type) t_arrow) infer
  | W_apply :
      ('ctx, 'lambda, ('param, 'body) t_arrow) infer
      * ('ctx, 'arg, 'param) infer
      -> ('ctx, ('lambda, 'arg) e_apply, 'body) infer
  | W_forall :
      (c_type * 'ctx, 'body, 'body_type) infer
      -> ('ctx, 'body e_forall, 'body_type t_forall) infer
  | W_type_apply :
      ('ctx, 'forall, 'body t_forall) infer
      * ('ctx, 'arg) check
      * ('x, 'arg, z, 'body) subst
      -> ('ctx, ('forall, 'arg) e_type_apply, 'x) infer

type ('ctx, 'expr) p_infer =
  | P_infer :
      ('ctx, 'expr, 'type_) infer * 'type_ type_
      -> ('ctx, 'expr) p_infer

let rec infer : type ctx w. ctx context -> w expr -> (ctx, w) p_infer option =
 fun ctx expr ->
  match expr with
  | E_var n ->
      let* (P_lookup_value (w, type_)) = lookup_value ctx n in
      let (P_shift (x_w, x)) = shift n Z type_ in
      Some (P_infer (W_var (w, x_w), x))
  | E_lambda (param, body) ->
      let* param_w = check ctx param in
      let* (P_infer (body_w, body_type)) = infer (C_value (param, ctx)) body in
      Some (P_infer (W_lambda (param_w, body_w), T_arrow (param, body_type)))
  | E_apply (lambda, arg) -> (
      let* (P_infer (lambda_w, arrow)) = infer ctx lambda in
      match arrow with
      | T_var _ -> None
      | T_forall _ -> None
      | T_arrow (param, body) -> (
          let* (P_infer (arg_w, arg_type)) = infer ctx arg in
          match Type.equal param arg_type with
          | D_neq _ -> None
          | D_refl -> Some (P_infer (W_apply (lambda_w, arg_w), body))))
  | E_forall body ->
      let* (P_infer (body_w, body_type)) = infer (C_type ctx) body in
      Some (P_infer (W_forall body_w, T_forall body_type))
  | E_type_apply (lambda, arg) -> (
      let* (P_infer (forall_w, forall)) = infer ctx lambda in
      let* arg_w = check ctx arg in
      match forall with
      | T_var _ -> None
      | T_arrow _ -> None
      | T_forall body ->
          let (P_subst (x_w, x)) = subst arg Z body in
          Some (P_infer (W_type_apply (forall_w, arg_w, x_w), x)))

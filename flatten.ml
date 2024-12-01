open Utils
open Ttree
open Ftree

module Context : sig
  type context
  type t = context

  val initial : context
  val solve : context -> Index.t -> var
  val write_hoist : context -> unit
  val set_hoist : context -> Index.t -> code -> unit

  val fork_and_write_lambda :
    context -> (context -> code) -> var * [ `context of var ]

  val write_let : context -> code -> var
  val emit_program : context -> code -> program
end = struct
  type context = {
    (* TODO: come on, do better *)
    mutable program : (Level.t * block) list;
    mutable next_global : Level.t;
    mutable block : code option ref list;
    mutable next_local : Level.t;
    mutable env : (Level.t * code option ref) list;
  }

  type t = context

  let initial =
    (* TODO: this is really ugly *)
    (* TODO: duplicated from Solver *)
    (* TODO: predef somewhere *)
    (* TODO: rename Type to data *)
    let env = (Level.zero, ref None) :: [] in
    {
      program = [];
      next_global = Level.(next zero);
      block = [];
      next_local = Level.(next zero);
      env;
    }

  let emit_block block last =
    let last = B_return { code = last } in
    List.fold_left
      (fun next arg ->
        match !arg with
        | Some arg -> B_define { arg; next }
        | None -> failwith "uninitialized hoist")
      last block

  let emit_program ctx last =
    let main : program =
      let body = emit_block ctx.block last in
      P_main { body }
    in
    List.fold_left
      (fun next (context, body) -> P_lambda { context; body; next })
      main ctx.program

  let fork_and_write_lambda ctx k =
    (* TODO: this is introducing too much on the env *)
    let {
      program = _;
      next_global = _;
      block = block_snapshot;
      next_local = context;
      env = env_snapshot;
    } =
      ctx
    in
    (* TODO: fix of param is weird *)
    ctx.block <- [];
    ctx.next_local <- Level.next context;
    ctx.env <- (context, ref None) :: env_snapshot;
    let last = k ctx in
    let body = emit_block ctx.block last in
    let funct = ctx.next_global in
    ctx.program <- (context, body) :: ctx.program;
    ctx.next_global <- Level.next funct;
    ctx.block <- block_snapshot;
    ctx.next_local <- context;
    ctx.env <- env_snapshot;
    (funct, `context context)

  let write_let ctx code =
    let var = ctx.next_local in
    let cell = ref (Some code) in
    ctx.block <- cell :: ctx.block;
    ctx.next_local <- Level.next var;
    ctx.env <- (var, cell) :: ctx.env;
    var

  let solve ctx var =
    match List.nth_opt ctx.env ((var : Index.t) :> int) with
    | Some (var, _cell) -> var
    | None -> failwith "unknown var somehow"

  let write_hoist ctx =
    let var = ctx.next_local in
    let cell = ref None in
    ctx.block <- cell :: ctx.block;
    ctx.next_local <- Level.next var;
    ctx.env <- (var, cell) :: ctx.env

  let set_hoist ctx var code =
    match List.nth_opt ctx.env ((var : Index.t) :> int) with
    | Some (_var, { contents = Some _ }) -> failwith "duplicated fix somehow"
    | Some (_var, cell) -> cell := Some code
    | None -> failwith "unknown hoist somehow"
end

open Context

let let_or_var ctx funct =
  match funct with
  | C_var { var } -> var
  | C_apply { funct = _; arg = _ }
  | C_closure { ptr = _; context = _ }
  | C_type_stub ->
      write_let ctx funct

let rec flatten ctx term =
  match term with
  (* TODO: lower types *)
  | T_annot { term; annot = _ } -> flatten ctx term
  | T_var { var } ->
      let var = solve ctx var in
      C_var { var }
  | T_let { bound = _; arg; body } ->
      let arg = flatten ctx arg in
      let _arg = write_let ctx arg in
      flatten ctx body
  | T_hoist { bound = _; annot = _; body } ->
      (* TODO: unify hoist, let and fix *)
      write_hoist ctx;
      flatten ctx body
  | T_fix { bound = _; var; arg; body } ->
      (* TODO: should fail if body is not a lambda *)
      let arg = flatten ctx arg in
      set_hoist ctx var arg;
      flatten ctx body
  | T_apply { funct; arg } ->
      let funct = flatten ctx funct in
      let funct = let_or_var ctx funct in
      let arg = flatten ctx arg in
      let arg = let_or_var ctx arg in
      C_apply { funct; arg }
  | T_lambda { bound = _; body } ->
      let funct_ptr, `context context =
        fork_and_write_lambda ctx @@ fun ctx -> flatten ctx body
      in
      C_closure { ptr = funct_ptr; context }
  | T_forall { bound = _; param = _; body = _ } -> C_type_stub
  | T_self { bound = _; self = _; body = _ } -> C_type_stub

type context = Context.t

let initial = Context.initial

let flatten ctx term =
  let code = flatten ctx term in
  emit_program ctx code

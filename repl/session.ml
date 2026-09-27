type result = { type_ : Typer.Typed_tree.term; value : Typer.Typed_tree.term }
type t = Typer.Elaborate.context option

let empty : t = None

let result (type_, value) =
  { type_ = Typer.Typed_tree.whnf type_; value = Typer.Typed_tree.whnf value }

let evaluate source =
  Option.map
    (fun term -> result (Typer.Elaborate.infer term))
    (Lexer.parse source)

let submit session source =
  match Lexer.parse_line source with
  | None -> (session, None)
  | Some (Ctree.CTerm { term = head; loc } as syntax) ->
      let ctx =
        match session with
        | None -> Typer.Elaborate.initial_context loc
        | Some ctx -> Typer.Elaborate.copy_context ctx
      in
      let ctx, output =
        match head with
        | Ctree.CT_bind { bound; value } ->
            let bound = Lparser.parse_pat bound in
            let value = Lparser.parse_term value in
            let name, type_, value =
              Typer.Elaborate.infer_let ctx bound value
            in
            let (Lparser.Syntax_tree.Pat { loc; _ }) = bound in
            let ctx, _ = Typer.Elaborate.with_let loc ctx name type_ value in
            (ctx, result (type_, value))
        | _ ->
            ( ctx,
              result (Typer.Elaborate.infer_in ctx (Lparser.parse_term syntax))
            )
      in
      (Some ctx, Some output)

open Lparser.Syntax_tree

module Precedence = struct
  let sequence = 0
  let abstraction = 1
  let annotation = 2
  let application = 3
  let postfix = 4
  let atom = 5
end

let parenthesize needed formatter print =
  if needed then Format.pp_print_char formatter '(';
  print ();
  if needed then Format.pp_print_char formatter ')'

let rec pp_term_at context formatter (Term { head; _ }) =
  let open Precedence in
  match head with
  | T_hole -> Format.pp_print_char formatter '_'
  | T_type -> Format.pp_print_string formatter "Type"
  | T_var { var } -> Name.pp formatter var
  | T_def { bound; body } ->
      parenthesize (context > sequence) formatter (fun () ->
          Format.fprintf formatter "@[<hov>%a;@ %a@]" pp_pattern bound
            (pp_term_at sequence) body)
  | T_let { bound; arg; body } ->
      parenthesize (context > sequence) formatter (fun () ->
          Format.fprintf formatter "@[<hov>%a@ =@ %a;@ %a@]" pp_pattern bound
            (pp_term_at abstraction) arg (pp_term_at sequence) body)
  | TF_pi { bound; body } -> pp_binder context formatter "->" bound body
  | TI_lambda { bound; body } -> pp_binder context formatter "=>" bound body
  | TE_apply { funct; arg } ->
      parenthesize (context > application) formatter (fun () ->
          Format.fprintf formatter "@[<hov>%a(%a)@]" (pp_term_at application)
            funct (pp_term_at sequence) arg)
  | TE_annot { value; annot } ->
      parenthesize (context > annotation) formatter (fun () ->
          Format.fprintf formatter "@[<hov>%a@ :@ %a@]"
            (pp_term_at (annotation + 1))
            value (pp_term_at abstraction) annot)
  | TF_self { bound; body } ->
      parenthesize (context > abstraction) formatter (fun () ->
          Format.fprintf formatter "@[<hov>self(%a)@ ->@ %a@]" pp_pattern bound
            (pp_term_at abstraction) body)
  | TI_fix { bound; body } ->
      parenthesize (context > abstraction) formatter (fun () ->
          Format.fprintf formatter "@[<hov>fix(%a)@ =>@ %a@]" pp_pattern bound
            (pp_term_at abstraction) body)
  | TE_unfold { fix } -> pp_postfix context formatter fix ".@"
  | TF_unit -> Format.pp_print_string formatter "[]"
  | TI_unit -> Format.pp_print_string formatter "()"
  | TE_drop { arg; body } ->
      parenthesize (context > sequence) formatter (fun () ->
          Format.fprintf formatter "@[<hov>()@ =@ %a;@ %a@]"
            (pp_term_at abstraction) arg (pp_term_at sequence) body)
  | TF_sigma { fst; snd } ->
      Format.fprintf formatter "@[[%a,@ %a]@]" pp_pattern fst
        (pp_term_at sequence) snd
  | TI_pair { fst; snd } ->
      Format.fprintf formatter "@[[%a,@ %a]@]" (pp_term_at sequence) fst
        (pp_term_at sequence) snd
  | TE_fst { pair } -> pp_postfix context formatter pair ".0"
  | TE_snd { pair } -> pp_postfix context formatter pair ".1"

and pp_binder context formatter operator bound body =
  let precedence = Precedence.abstraction in
  parenthesize (context > precedence) formatter (fun () ->
      Format.fprintf formatter "@[<hov>(%a)@ %s@ %a@]" pp_pattern bound operator
        (pp_term_at precedence) body)

and pp_postfix context formatter value operator =
  let precedence = Precedence.postfix in
  parenthesize (context > precedence) formatter (fun () ->
      Format.fprintf formatter "%a%s" (pp_term_at precedence) value operator)

and pp_pattern formatter pattern =
  let rec split_apply pattern arguments =
    let (Pat { head; _ }) = pattern in
    match head with
    | P_apply { funct; arg } -> split_apply funct (arg :: arguments)
    | P_annot _ | P_var _ | P_wildcard | P_unit | P_pair _ ->
        (pattern, arguments)
  in
  let pp_plain formatter (Pat { head; _ }) =
    match head with
    | P_annot { pat; annot } ->
        Format.fprintf formatter "%a : %a" pp_pattern pat
          (pp_term_at Precedence.annotation)
          annot
    | P_var { var } -> Name.pp formatter var
    | P_wildcard -> Format.pp_print_char formatter '_'
    | P_unit -> Format.pp_print_string formatter "()"
    | P_pair { fst; snd } ->
        Format.fprintf formatter "[%a, %a]" pp_pattern fst pp_pattern snd
    | P_apply _ -> assert false
  in
  match split_apply pattern [] with
  | head, [] -> pp_plain formatter head
  | head, arguments ->
      Format.fprintf formatter "%a(%a)" pp_plain head
        (Format.pp_print_list
           ~pp_sep:(fun formatter () -> Format.fprintf formatter ",@ ")
           pp_pattern)
        arguments

let pp = pp_term_at Precedence.sequence
let to_string term = Format.asprintf "%a" pp term
let pattern_to_string pattern = Format.asprintf "%a" pp_pattern pattern

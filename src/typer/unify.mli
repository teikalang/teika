open Type

type error =
  | Type_clash of { expected : type_; received : type_ }
  | Occur_check of { var : type_; type_ : type_ }
  | Escape_check of { var : type_; type_ : type_ }
[@@deriving show]

exception Error of { loc : Location.t; error : error }

val unify : loc:Location.t -> expected:type_ -> received:type_ -> unit

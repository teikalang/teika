open Type

exception Var_clash of { expected : Var.t; received : Var.t }
exception Type_clash of { expected : type_; received : type_ }
exception Not_a_function of { funct : type_ }
exception Not_a_type of { type_ : type_ }

val subtype : expected:type_ -> received:type_ -> unit
val extract_type : wrapped:type_ -> type_
val apply : funct:type_ -> arg:type_ -> type_

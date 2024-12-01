type nat = { n : 'a. 'a -> ('a -> 'a) -> 'a } [@@ocaml.unboxed]

let zero () = { n = (fun z s -> z) }
let succ pred = { n = (fun z s -> s (pred.n z s)) }
let add a b = a.n b succ
let mul a b = a.n (zero ()) (fun n -> add n b)
let one () = succ (zero ())
let two () = succ (one ())
let n4 () = mul (two ()) (two ())
let n8 () = mul (n4 ()) (two ())
let n16 () = mul (n8 ()) (two ())
let n32 () = mul (n16 ()) (two ())
let n64 () = mul (n32 ()) (two ())
let n128 () = mul (n64 ()) (two ())
let n256 () = mul (n128 ()) (two ())
let n512 () = mul (n256 ()) (two ())
let n = n512 ()

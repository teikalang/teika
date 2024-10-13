## Number type problem

The float array access problem in Grain.

```rust
HM + Bidirectional implies in Inference + Subtype

if (x < Array.length x) {
  let element = Array.get x array;
  ()
}

((n : A) where P n);

List<n : Number where n > 0>

Nat -> Int -> Number

List<Nat>

match (x : Number) with
| x instanceof Int => ()
| x instanceof Nat => ()
| _ => raise


enum A = {
  id : Int,
  name : String
}

module M = {
  type t
  val x : t
}
open M
type t;

x : t/2

{.._A id : Int, name : String } : User.t
<.._A id : Int, name : String > : <id : int; name : string>

expected a Record {.. idx : Int, name : String } received a Int

Record {.. idx : Int, name : String } : Record {.. id : Int, name : String }

User.t doesn't have a field "idx",
  maybe you meant to say "id"

record User { id : Int; name : String };

let (User { id; name }) = user;
let user = User { id = 0; name = "Eduardo" };

enum X {.._A id : Int, name : String } : User.t
<.._A id : Int, name : String > : <id : int; name : string>




let user = User { id; name };

module Veggie = {
  enum Veggie { Squash, Cabbage, Broccoli }
  let x = Squash
}

let Squash = Veggie.x;

let (id, name) =
  match user with
  | { id; name } -> (id, name);

let ({ id; name } : User.t) = user;

x =>
  match x with
  | x instanceof Int => Int
```

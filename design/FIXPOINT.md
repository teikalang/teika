## Equality

Checking theThis is important as ideally Teika supports a fully structural calculus.

## The big problem

Checking the equality of two different fixpoint is equivalent to checking if two non-terminating lambda terms are the same, as you can always encode a fixpoint in the untyped lambda calculus and checking if untyped lambda terms are equal seems to be known to be undecidable.

## Solution

Instead of trying to find all the equalities, another alternative is to ask the user to prove that the types are equivalent by relying on univalence, this equality can be known to be irrelevant by proving that it is isormophic to id.

## Undecidable

While there will never be a decidable algorithm that would find all equalities. There can exist undecidable but complete algorithms, those can be used to help the user and potentially generate the equality that allows to convert between them.

This algorithm can be quite simple and maybe even oferred in an opt-in way for developers, literally just expanding everything on demand and checking if the pair is already in the list, it needs to be done carefully as there is some unsoundness opportunities here.

This is likely a future improvement, especially as making it fast may be hard.

## Scope

So instead of finding a general algorithm, it would be nice to find a subset of the problem that matches what developers expects and that will not effect performance that much.

There is mainly two subsets, the first one is first-order recursion, which is known to be decidable and there is an `O(n log n)` which is quite reasonable, the second one is strictly equal, where both are defined the same.

While I think supporting first-order recursion would be nice, it doesn't seems to be worth the complexity for a very niche feature. Additionally the undecidable solution is likely to catch all of this solutions quite quickly. 

Strictly equal should be supported in the future, there is no disadvantage as it is as fast as any other type former and it's a major subset that developers expect to work.

Another small subset is one where the hypothesis are required but only once and/or only once per combination of fixpoint + args.


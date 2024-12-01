## Introduction

- Hey, my name is Eduardo Rafael, just call me Edu.

- Today I will be trying to give an overview on GADT's and also on a general technique for GADT's.

- I would describe GADT's as ADT's equipped with existential types and equalities.

- So, let's go quickly over existentials and equalities

## Existentials

5min

- Existential types are similar to generics aka universal types, but reversed, sadly, I don't have time to focus in this duality, so let's go with examples.

- Here is an example of existential types, let's forget about syntax for a second and focus on the record inside of the ADT.

- Notice that this variable 'a is not mentioned externally anywhere, in fact it is internally bound, meaning that whenever you have a value of type show, you don't know the type of content, you just know that content has a type and that show accepts something that type and return a string.

- This is how you can use it, creating a show is just the same as creating any normal ADT ... but the eval_show function is kind of weird, what is the type of content and show on that pattern matching?

- Well, it is an unknown type introduced by the pattern matching, let's ignore the name, you know, dollar symbol thingy, just notice that the content type matches the parameter type in the show function. We don't know what $Ex_show'a is, but we can manipulate it.

- This can also be done using first-class modules and IMO the syntax is better and slightly more intuitive, you can see that S.show has the type t defined inside of the module, you still don't know what S.t is but you can manipulate it. But this is not a talk on first-class modules.

## Equality

5min

- Now, let's get a bit into equality, this one looks simple ... by having equalities, we can say that two types are equal to each other.

- The way that it works is kind of weird tho, on the equality definition, you can apply a constraint on the type constructor, stating that two types must be equal to each other, so the first paramater of eq and the second parameter of eq, must be the same to instantiate a Refl.

- You can see that it fails to type whenever I try to create an equality where both sides are not the same. So string and string works, but not string and int.

- But I can assume an equality where both sides are not the same, but by knowing that if this function is called with an equality, then both sides were originally the same, it means that I can convert one on the other.

- So, for any a and b, if you give me an equality stating that a is equal to b, then I can convert anything from a to b. Notice that "this equality", is just a value of type eq and we can use it by pattern matching on it. But again, the reason why this works, is because by definition, if you construct a Refl, then both types are equal.

- BTW, equality is stronger, as in it works under type constructors, but we need functors to show that. So, for any t, a and b, if a is equal to b, then I can convert an a t to an b t. In fact any type capable of doing subst, can be considered an equality

- Using equalities you can also do refutation, because the typer knows that int will never be equal to string, we can match on it and use the . to essentially convince tell the typer that this will never happen.

- This is very convenient, also, tip, whenever you see a function that return's just an 'a and that 'a is not mentioned anywhere, either you cannot call this function, it raises an exception or it enters in a loop, such a function will never return.

- We can also get much fancier and this is where this talk start to get messy. Here we have a function that either takes an equality where a is equal to int or an equality where a is equal to bool, then by matching on both cases, we can do to_string on our input of type a.

- Notice that while a seems like a generic type, in fact it is literally either int or string. Yes, like in TypeScript. This also starts to look like some dependent types kind of thing, to know the type of a, you need to know the value of Either.

## But why?

5min

- Ok, now we now what GADT's do, but why are we using them? Well ... mostly to describe simple type equations.

- A common case that people actually use it for, is for serializers and deserializers such as this.

- This is essentially just a couple Either together with a recursive type, it is just a nice way of having those equalities. Here, either the parameter will be an int, a bool or a list with an int, a bool or another list with an int ... you got it, it's recursive.

- You can use it for defining functions such as show, which will take any of the supported types and convert it into a string, we could also do the reverse and extract a value from a string, but that has more noise.

- This works by pattern matching on the tag and as such revealing which type the content is, so using the implicit equalities, note that this is magically solved by the OCaml typer, but sometimes we need more annotations.

## The goal

- That's nice, but I would not say it is very principled, it's like we have all sorts of small relations in the which leads me to the goal, the dream and maybe the future, proper dependent types.

- Here we could define a type constructor that can pattern match on the value and say what each type would be. Notice that no weird relationship, just normal pattern matching here.

- This could be then be used by the show function, which now could even support inference if we're lucky. Still, only using traditional features of the language.

## Duplicating

5min

- But this doesn't work, we cannot have a term value on the type level, tag is not a valid variable, which leads me to my solution. Duplication.

- Instead of having the term value on the type level, I want to have a type that behaves exactly the same as the term, such that any operation on the term is the same as the same operation on the type level.

- Using GADT's this is possible. We're still using equalities, but you can notice that every constructor maps strictly to one type. Such that if you know the type, you also which constructor it is.

- And now we can define our function, here you can see that whenever it is t_int, it will be an int, t_bool, it will be a bool and so on. While this looks messy, it is literally a 1:1 translation of the original code, where the return is done through an equality on 'r.

- Then we can use our function, this works by introducing a content type variable and stating that it is equal to the return of our type level function, then just by doing all the pattern matching, you can see that something similar to the original code was recreated.

- While this may look messy, it's quite simple, you just need to imagine that every function call needs to be bound to a variable, so we introduce variable as needed, this is actually quite common in compilers, such as SSA and ANF.

- An important thing to notice on this code, is that our witness on the term level is irrelevant, all of the pattern matching are acting as if they were unit and not doing anything, they're just dumb lets, this is because the witness is actually just a proposition, this means that technically if the OCaml compiler supported this, this would have zero runtime cost as this witness would not needed to be passed around, maybe on flambda2? Maybe with some modes?

- But of course, we need to be able to call such a function, that's where the duplicating part shows up again, in the same way that anything on the term level needs to have a 1:1 mapping on the type level, any type level function also requires a copy on the term level and an existential wrapper linking both of them together.

- Then by having all the functions required, we can just call it. It looks messy, but again, it's just boilerplate, there is not a single new line of logics, a computer could have generated this.

## But why

- And yes, I'm not going crazy. I just really wanted to do addition on the type level.

- But seriously, this is interesting because while it is work intensive it is not hard, it's a very general technique to do any sort of dependently typed program in OCaml.

- A lot of the patterns that we see for GADT's can be understood as a matter of writing this boilerplate version, then "internalizing" some of the functions, which works and it leads to less boilerplate, but at some point you have the same GADT, describing so many different functions that it become a problem. This technique on the otherhand is always module, you can add a function at the time.

## Future

- I think the bulk of this could actually be done through a ppx or maybe a dependent OCaml version? I'm not working on it right now, but if anyone is interested, let me know.


## Social


But the function also needs to be copied on the term level and be packed on an existential as our return is 'r.

-  All the rest of the code is just 
- We can think of functions as relations, this is also similar to initialization through mutation or destination passing style.

```ocaml
let r = f x
(* it's similar to *)
let r
and f x &r
```

```ocaml
type tag content. (tag, content) to_type_w -> tag ty -> content -> string

type tag.
type content = to_type tag.
tag ty -> content -> string
```

- All ADT's needs to be defined both to the term level and also the type level, such that a 1:1 relationship exists

- The technique, for any function that we want, we now need 2 copies of the same function, one on the type level and the other one on the term level, additionally a package binding them together on the term level.

- But performance, yes, it's bad. But those are just propositions, aka it doesn't matter for computing, so you can just erase them from the binary, but sadly OCaml still doesn't support that. Maybe with flambda2 or modes?

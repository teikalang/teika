# Smol Frontend

## Optimizations

### Explicit Substitutions

This uses explicit substitutions to achieve laziness, similar to λυ.

I think Smol doesn't have metavariables as no unification exists, additionally currently substituions are not used for equality.

Also the failure mode is that it will reject terms not accept terms, which seems to be okay.

- https://drops.dagstuhl.de/opus/volltexte/2014/4858/pdf/34.pdf
- https://www.irif.fr/~kesner/papers/springer-csl07.pdf
- https://hal.inria.fr/inria-00074197/document

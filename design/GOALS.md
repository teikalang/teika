# Goals

This should document the goals of the project, the why and the tradeoff's.

## Assumptions

1. Most code will be read more times than written
2. Most code will be read by proficient developers
3. Most code will be written by proficient developers
4. Most code will be simple even if powerful features exists
5. Beginners developers are only temporarily beginners
6. Tooling can be made to help beginners developers

## Direct

Indirections makes code more complex, a language should be direct. Functions should be known statically and data indirections should be optimized away.

## Succinct

Only local information should be syntactically provided, a language should be succint. Noise should be avoided and contextual information should be provided on demand.

## Powerful

Users should be able to describe abstract and efficient code, while still being able to reason about it locally, a language should be powerful. Effects should be tracked and mutation controlled.

## Flexible

Hacks were needed in the past, are needed today and will be needed in the future, users will need to hack code, a language should be flexible. Abstractions are gonna restrict code, tooling should flexibilize it.

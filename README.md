# c-builder

This library is for generating C programs. The target audience is users
users who want to generate C as a compiler backend. In summary:

* Goal: Generate C programs
* Goal: Avoid unneeded parentheses when operator precedence implies
  the correct interpretation. For example, produce `x->foo->bar` instead
  of `(x->foo)->bar`.
* Goal: Make it straightforward to work with literals of all size classes.
  Suppress annotations when the context implies that they are unnecessary.
* Goal: Support types for vector extensions.
* Non-Goal: Parse C programs
* Non-Goal: Compile C programs

## Design Decisions

This library does not support every possible C construct since some constructs
are redundant. A compiler backend does not need every syntactic shortcut at
its disposal.

* In-place modifications like `+=` and `-=` are not supported. This could
  change in the future.
* Pre-increment, post-increment, pre-decrement, and post-decrement are
  supported.
* Applying `sizeof` to expressions is not supported. It may only be applied
  to types.
* Integer literals include their signedness (signed or unsigned) and their
  size (`short`, `int`, `long`, `long long`, and fixed-width types from
  `stdint.h`). This information is used when encoding the literal. If its
  omission is safe, it is omitted.
* Places that are considered safe for omitting signedness and size of
  a literal:
    * RHS of assignment
    * Function argument
    * Argument of binary comparison operator. This can change the behavior
      of some programs that compare a signed number with an unsigned
      number, where one or both arguments are literals. This cost
      is considered acceptable because it makes a lot of output more
      readable.

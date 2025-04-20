This step introduces [Kernel.sml](Kernel.sml) which contains the
primitive inference rules and the `thm` abstract type.

Since the only way to construct a `thm` are through the primitive
inference rules, if we implement the inference rules correctly, then
all theorems are "correct by construction". Robin Milner designed this
ingenious approach.

In retrospect, we should have probably named the Kernel module `Thm`
with the abstract type `eqtype t` to be consistent with the naming
conventions (that `Thm.t` refers to a theorem instance, as opposed to
`Kernel.thm`).

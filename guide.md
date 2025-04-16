# The Make-a-HOL Process

- [Prefatory Comments](#prefatory-comments)
- [Pick a language](#pick-a-language)
- [The Make-A-HOL Process](#the-make-a-hol-process-1)
  - [Step 0: Tables](#step-0-tables)
  - [Step 1: Substitutions](#step-1-substitutions)
  - [Step 2: Types](#step-2-types)
  - [Step 3: Terms](#step-3-terms)
  - [Step 4: Matching](#step-4-matching)
  - [Step 5: Primitives](#step-5-primitives)
  - [Step 6: Kernel](#step-6-kernel)
  - [Step 7: Natural Deduction Rules](#step-7-natural-deduction-rules)

<a name="prefatory-comments">

## Prefatory Comments

This is an incomplete guide to making a HOL-like proof assistant, with
pointers to other projects and technical papers, loosely inspired by
[Make a Lisp](https://github.com/kanaka/mal).

This is a learning tool, so clarity is preferred to performance.

The design space for LCF-style proof assistants for higher-order logic
is quite large, and has not been systematically studied. We will not
engage in a systematic study of the possible variants in design.

But digressions about different ways to design things will be
made. The design decisions have consequences later on, and we try to
point them out to the reader as they emerge.

This is a draft, so it may be obscure at parts. I apologize. Feedback
is welcome, either left here as a comment, or send me an email
(`pqnelson` at `gmail`).

The main proof assistants in this space include:

- [HOL4](https://github.com/HOL-Theorem-Prover/HOL) "genetic HOL" descended from LCF, handed down on carved
  stone tablets from Andrew Gordon
- [HOL Light](https://github.com/jrh13/hol-light) is a minimal HOL
  written in OCaml, which inspired a generation of HOLs
- [HOL Zero](http://proof-technologies.com/holzero/index.html) is a
  minimalist HOL
- [Candle](https://cakeml.org/candle.html) is a HOL built atop CakeML
- Isabelle/HOL is a HOL built atop the [Isabelle](https://isabelle.in.tum.de/) engine, which is not
  relevant for our exploration (although Isabelle/Pure may be
  interesting for readers who wish to work with intuitionistic
  higher-order logic)

<a name="pick-a-language"></a>

## Pick a language

You might have a language in mind. Technically speaking, any Turing
complete language suffices, but it's probably wise to use a statically
typed functional programming language (e.g., Standard ML, OCaml,
Haskell, Elm, and friends).

I'm just going to assume that your language has typechecking,
something like algebraic data types, and the ability to make data
constructors private. Every functional programming language listed
above satisfies these desiderata.

You may want to also include unit tests, to make sure your code works
as expected. This may force you to write a unit testing framework (for
Standard ML, at least).

The pseudocode provided will be pidgin Standard ML.

<a name="the-make-a-hol-process-1"></a>

## The Make-a-HOL Process

<a name="step-0-tables"></a>

### Step 0: Tables

If your language provides some kind of associative array
(e.g., Haskell's [Data.Map](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Map.html)), then you're good to go to the next step.

For other languages, you should implement a table data structure. This
may involve implementing an "Order" structure. The specification
should look something like:

```sml
signature ORD = sig
  type t;
  val compare : t * t -> order;
  val eq : t -> t -> bool;
end;

signature TABLE = sig
  structure Key : ORD;
  type 'a t;
  type 'a Table = 'a t; (* synonym for type readability *)

  val empty : 'a Table;

  (* [insert] Either insert a new key-value entry into a
     table, or if the key is already present overwrite the
     entry. *)
  val insert : 'a Table -> Key.t -> 'a -> 'a Table;
  
  (* [null] tests if a table is empty or not *)
  val null : 'a Table -> bool;

  (* [size] The number of entries in the table *)
  val size : 'a Table -> int;

  (* [member] Tests if the Table contains the key. *)
  val member : 'a Table -> Key.t -> bool;

  val lookup : 'a Table -> Key.t -> 'a option;
  
  val union : 'a Table -> 'a Table -> 'a Table;
end;
```

Standard ML could use an association list for initial coding (and
later implement something more efficient).

**Optional:**

- Add more quality-of-life functions (like `delete` to remove an
  entry, `map : ('a -> 'b) -> 'a Table -> 'b Table` to apply a
  function to the value of each entry, `merge_into old new` to merge
  new bindings into an old table, etc.)
- You may want to also implement a `Set` data structure. Again, a list
  works fine for initial explorations (provided there are no
  duplicates in the list).

<a name="step-1-substitutions"></a>

### Step 1: Substitutions

We will have substitutions of various kinds in HOL.

A substitution is a list of pairs. Some languages support custom infix
operators, which allows you to write `old |-> new` for a constructor
of a substitution replacing `old` with `new`.

```sml
signature SUBST = sig
  type ('a, 'b) t;

  (* the empty substitution *)
  val empty : ('a, 'b) t;

  (* test if the given object appears in the domain of the
     substitution *)
  val contains : ('a, 'b) t -> 'a -> bool;

  (* Extend a substitution with a new entry *)
  val add : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t;
end;
```

<a name="step-2-types"></a>

### Step 2: Types

HOL is a simply-typed lambda calculus with one or two primitive types.

However, we want to make everything private, so the user can only
construct new HOL Types from the methods we provide.

**Design decision 1:** How pure do you want to make your HOL?

Haskell and friends have no choice, you need to use a State monad to
keep track of the list of defined types.

Standard ML, OCaml, and friends, can use a global `ref` variable for
tracking these definitions in a `Table`.

This decision will impact how we record a new type definition. The
impure global `ref` variable approach could use a function signature
like `new_type : string * int -> unit` which adds a new type
constructor and its arity to the global table.

**Design decision 2:** When you invoke `mk_type`, are you going to
check the table of type definitions to see the arity is respected?

For pure functional programming languages, this arity check could
occur at another stage (e.g., in a state monad).

**Optional**

- You might want to add helper functions like predicates 
  `is_var : Type -> bool` and `is_type : Type -> bool`
- It may be useful to include a `to_string : Type -> string`
  or `serialize : Type -> string` function
- You may also want to include functions to "hide" constructing
  function types (so the same string is used for function type
  operators). For example of some of these functions
  - `mk_fun : Type * Type -> Type` takes a domain and range type,
    then returns the function type of all functions from the domain to
    the range
  - `is_fun : Type -> bool` is a predicate checking the given HOL type
    is a function type
  - `dest_fun : Type -> Type * Type` "unpacks" a function type to
    produce an ordered pair consisting of its domain and range types
  - `domain : Type -> Type` gives the domain for a function type
  - `range : Type -> Type` gives the range for a function type
  
```sml
signature TYPE = sig
  eqtype Type;
  
  (* Make HOL Types an ORD instance *)
  type t = Type;

  val compare : Type * Type -> order;
  val eq : Type -> Type -> bool;

  (* Constructors for HOL Types *)
  val mk_type : string * Type list -> Type;
  val mk_var : string -> Type;

  (* Destructuring functions for HOL Types *)
  (* (Destructorators?) *)
  val dest_type : Type -> string * Type list;
  val dest_var : Type -> string;

  (* Operations on types *)
  (* [subst] will substitute on type variables only *)
  val subst : (Type,Type) Subst.t -> Type -> Type;
end;
```

<a name="step-3-terms"></a>

### Step 3: Terms

The terms of HOL are those of a simply-typed lambda calculus _with constants_.


**Design decision 1:** How do we represent the syntax tree for terms?
There are a lot of different ways to do so.

Jesper Cocx's [1001 Representations of Syntax with Binding](https://jesper.sikanda.be/posts/1001-syntax-representations.html)
surveys the different ways to design data structures for binders and
bound variables.

The specification for the `Term` data structure should not depend on
which representation you choose to use. **But it is highly recommended
to picks some syntax tree which easily distinguishes bound variables
from free variables!**

Note: if you have chosen to use, say, locally nameless variables for
the syntax tree, then `mk_abs(v,t)` should replace all _free instances_
of `v` in `t` by a bound variable. Also note that
`dest_abs(mk_abs(v,t)) = (v,t)` --- i.e., `dest_abs` should "undo"
this and replace all bound variables attached to the outermost binder
with free variables.

**Design decision 2:** Depending on your design decision from step 2,
you either will have a state monad for the proof assistant, or you're
using global mutable variables.

You'll need another table for constant definitions. For global mutable
variables, you can just add another for the constants. You need to
also include a `new_const : string * Type.t -> unit` function to add a
new entry to this table.

If you're using state monads, then when `mk_const(s,ty)` is invoked
you need to check that a constant with identifier `s` and type `ty'`
was previously defined (and `ty` is an instance of `ty'`).

**Design decision 3:** Support explicit substitutions?

That is to say, add a data constructor `LzSub of (Term,Term) subst * Term`
to the data type for `Term`. This allows us to support "explicit substitutions"
which are lazy.

This may be premature optimization, though.

**Alpha congruent terms.** You should implement a function
`aconv : Term -> Term -> bool` testing if the two terms are alpha
congruent or not.

Remember: a free variable is only alpha congruent with itself (and it
must agree with the type!).

So we should expect two free variables with different type variables
`a :: 'a` and `a :: 'b` to **not** be alpha congruent.

**Optional**

- Consider implementing a `to_string : Term -> string` or
  `serialize : Term -> string` to convert a term to some string
  representation 

```sml
(* Assuming we have a `structure Type : TYPE` implemented *)
signature TERM = sig
  eqtype Term;

  (* Terms are an ORD instance *)
  type t = Term;
  val compare : t * t -> order;
  val eq : t -> t -> bool;

  (* constructors *)
  val mk_var : string * Type.t -> Term;
  val mk_const : string * Type.t -> Term;
  (* [mk_abs(v,t)] Fails if `v` is not a variable produced
     from `mk_var`. *) 
  val mk_abs : Term * Term -> Term;
  (* [mk_app] Fails if the types are not compatible. *)
  val mk_app : Term * Term -> Term;

  (* destructuring functions *)
  val dest_var : Term -> string * Type.t;
  val dest_const : Term -> string * Type.t;
  val dest_app : Term -> Term * Term;
  val dest_abs : Term -> Term * Term;

  (* operations *)
  val type_of : Term -> Type.t;

  (* [aconv] Tests two terms if they are alpha convertible *)
  val aconv : Term -> Term -> bool;

  (* [inst] Instantiate type variables in a term *)
  val inst : (Type.t, Type.t) Subst.t -> Term -> Term;

  (* [subst] Substitute term variables in a term *)
  val subst : (Term, Term) Subst.t -> Term -> Term;

  (* [free_vars] Gives a set of free variables appearing in a
term *)
  val free_vars : Term -> Term set;
end;
```

<a name="step-4-matching"></a>

### Step 4: Matching

When we apply a theorem to a goal, we will want to _match_ the formula
of the theorem against the formula for the goal. HOL encodes formulas
as `Term` of a Boolean `Type` (usually called `:bool` in most HOLs).

So towards this end, we need to support matching a "pattern" term
(like the statement of a theorem) against an "object" term (like the
current goal). We will also need to match a "pattern" type against an
"object" type.

This means we will need to **modify** the signatures for `TERM` and
`TYPE` to add some more functions.

#### Type Matching

Given two types `T1` and `T2`, we will process this by creating a list
of equations we need to solve for, initially these will be `[T1]`, `[T2]`.

When both lists are empty, we have solved the equations, successfully
matching the object type against the pattern type, so we return the
substitution we have created.

Otherwise, look at the first element from the pattern list of types.

**Case 1: Type Variables.** If it is a type variable `a`, check if we have an entry in our
substitutions for `a` to be replaced by something else `tau`. If `tau`
is equal to the first element of the object list of types, then we're
good and can return our substitution. (If `a` is in our set of type
variables we should not replace, then check the first element of the
object list of types is equal to `a` and raise an exception if it's different.)

If there is no entry in our substitutions (or set of type variables to
not replace), then we check if the first element of the object list 

Suppose `v::pats` is our list of pattern types we're matching against
`ty::obs` as our list of object types.

- **If** `v |-> ty1` is in our substitution `sigma` _or_ `v = ty1`
  appears in the set of type variables we are not expanding,
  **then** check that `ty1 = ty`.
  - **If** `ty1 = ty`, 
    **then** recursively check `pats` against `obs` using the substitution `sigma` and set of
    type variables we're not expanding.
  - **Else** raise an exception that we would have to bind `v` to two
    different types
- **Else if** `v = ty`, then add `v` to the set of type variables we
  are not expanding, and recursively work on matching `pats` against
  `obs` with `sigma` and the updated set of type variables
- **Else** Add `v |-> ty` to the substitution `sigma`, and recursively
  match `pats` against `obs` with the updated substitution

**Case 2:** We have two type applications `(App(T1,args1))::pats` 
and `(App(T2,args2))::obs` we're trying to match against.

- **If** `c1 = c2`, **then** recursively check `args1++pats` against
  `args2++obs`
- **Else** we have two different type operators we're trying to match
  against, so raise an exception that we've failed.

**Case 3:** We are trying to match a type application against a type
variable, in which case we've failed.

```sml
signature TYPE = sig
  (* ...as before, except the new functions and exception: *)
  exception Match of string;

  (* Apply a substitution to a type *)
  val subst: (Type,Type) Subst.t -> Type -> Type;
  
  (* Given a pattern type and an object type, and a
substitution of type variables to types (and a set of type
variables to avoid matching), either
(1) Raise a [Match] exception indicating failure, or
(2) Produce the substitution needed to transform the pattern
    type into the object type.

  If (S,_) == match pat ob ([], empty_set),
  then `subst S pat = ob`.
   *)
  val match : Type -> Type ->
                (Type,Type) Subst.t * Type set ->
                (Type,Type) Subst.t * Type set;
end;
```

#### Term Matching

We want to match a pattern term against an object term. This is done
by recursively working on the structure of the terms, building a 
pair `(Term,Term) Subst.t` and `(Type,Type) Subst.t` of substitutions.

##### Recursive Step in Term Matching

Suppose we are matching a pattern term `p` against an object term
`ob`.

**Case 1:** `p` is a free variable.

- **If** there are bound variables in `ob` attached to a binder not
  contained in `ob`, **then** we raise an exception (because free
  variables should not involve bound variables out of scope, it leads
  to horrible complications which are a distraction without use)
- **Else if** there is a term `tm` such that `p |-> tm` appears in the
  term substitutions, **then** check that `ob` is alpha-convertible to `tm`.
  - If it's not alpha-convertible, then raise an exception since we're
    trying to substitute `p` with two distinct terms.
  - Otherwise continue working on the remaining subterms matching each other.
- **Else** there is no term `tm` such that `p |-> tm` appears in the
  term substitution, so add it to the term substitution, and
  recursively work our way through the remaining subterm matchings.

**Case 2:** `p` is a constant term `Const(c1,ty1)` and `ob` is a
constant term `Const(c2,ty2)`.

- **If** `c1` is not the same as `c2`, **then** raise an exception (these
  constants mismatch each other, there's no substitution possible that works).
- **Else** extend the type substitution `tySubs` with `Type.match ty1 ty2 tySubs`
  and recursively work on matching the remaining subterms.

**Case 3:** `p` is an application `App(M,N)` and `ob` is an
application `App(P,Q)`.

Recursively match `M` against `P`, and `N` against `Q`, before working
our way through the remaining subterms.

**Case 4:** `p` is an abstraction `Abs(Var(_,ty1),M)` (where `Var(_,ty2)`
is the bound variable) and `ob` is an abstraction `Abs(Var(_,ty2),N)`.

- Replace the type substitutions `tySubs` with the result of `Type.match ty1 ty2 tySubs`
- Then match `M` against `N` before continuing with the remaining match subterms

**Case 5:** `p` is a bound variable and `ob` is a bound variable.

- **If** `p` and `ob` are both bound to the same binder, **then** continue
  with matching the remaining subterms
- **Else** raise an exception that we're trying to match different
  bound variables.

**Case 6:** For all other `p` and `ob` situations, we should raise an
exception since we're trying to match nonmatchable terms.

##### Normalizing Substitutions

We may have a term substitution replacing a term variable `x` of type `T1`
with a term variable `x` of type `T2`, where `T1` is an instance of
`T2` upon applying the type substitution to it. In this case, we
should discard that particular substitution.

Come to think of it, we should apply type substitutions to the types
of the term variables appearing in the term substitution.

<a name="primitives"></a>

### Step 5: Primitives

There are one or two primitive types in a HOL proof assistant (`bool`
for formulas, and `ind` for nonlogical constants).

There are two different sets of primitive constants for a HOL proof
assistant besides the function type constructor (which I'll write as `a => b`
to distinguish it from a function type in our programming language `a
-> b`):

1. `{= : 'a => 'a => bool, CHOOSE : ('a => bool) => 'a}`,
   just polymorphic equality and Hilbert's choice operator
2. `{imp : bool => bool => bool, all : ('a => bool) => bool}`
   which consists of logical implication and a universal
   quantifier. 
   
We can define all other connectives out of these.

You need to pick one (or both) set of primitive constants to
implement.

Isabelle/Pure uses minimal HOL and adds equality to the second set of primitives.

HOL Light takes the first set of primitives (and defines equality in
the [kernel](https://github.com/jrh13/hol-light/blob/master/fusion.ml),
then introduces the Hilbert choice operator axiomatically).

HOL4 is rather generous, providing a uniform set of functions for
constructing constants in [HOL4/src/1/boolSyntax.sml](https://github.com/HOL-Theorem-Prover/HOL/blob/develop/src/1/boolSyntax.sml),
but according to section 1.6 of its "Description" document, only
equality and implication are really needed as "primitive".

If you're working with _intuitionistic_ higher-order logic, then you
cannot take the Hilbert choice operator (since that gives you
classical logic).

<a name="step-6-kernel"></a>

### Step 6: Kernel

The kernel combines the preceding steps to encode an abstract `thm`
type whose public-facing "constructors" are the inference rules for
the logical system.

The inference rules depend on the choice of primitives from the
previous steps. They will become functions of the kernel module.

HOL Light (and later the [OpenTheory logical kernel](https://gilith.com/papers/stdlib.pdf)) offers the
following rules (which are intuitionistic and assumes equality has
been derived or given as primitive):

$$\frac{}{\vdash t=t}\mathsf{refl}~t$$

$$\frac{}{\phi\vdash\phi}\mathsf{assume}~\phi$$

$$\frac{\Gamma\vdash\phi=\psi,\quad\Delta\vdash\phi}{\Gamma\cup\Delta\vdash\psi}\mathsf{eqMp}$$

$$\frac{\Gamma\vdash t=u}{\Gamma\vdash(\lambda v\ldotp t)=(\lambda v\ldotp u)}\mathsf{absThm}~v$$

$$\frac{\Gamma\vdash f=g,\quad\Delta\vdash x=y}{\Gamma\cup\Delta\vdash f~x=g~y}\mathsf{appThm}$$

$$\frac{\Gamma\vdash\phi,\quad\Delta\vdash\psi}{(\Gamma\setminus\{\psi\})\cup(\Delta\setminus\{\phi\})\vdash\phi=\psi}\mathsf{deductAntisym}$$

$$\frac{\Gamma\vdash\phi}{\Gamma[\sigma]\vdash\phi[\sigma]}\mathsf{subst}~\sigma$$

$$\frac{}{\vdash(\lambda v\ldotp t)u=t[u/v]}\mathsf{betaConv}(\lambda v\ldotp t, u)$$

The last two rules govern how we define constants and terms. Constants
are defined as `c = tm`.

$$\frac{}{\vdash c = t}\mathsf{defineConst}~c~t$$

Then we define a type `T_new` as those terms of type `T_old`
satisfying a predicate. The "data constructor" for the new term is
given by `abs` and the "destructuring function" `rep` gives us the
underlying element of `T_old`.

We also need to give the list of type variables $\vec{\alpha}$
apparing in the type.

(Note: $r$ and $a$ are free variables of appropriate type. In
particular, there are no quantifiers in this inference rule, because
they have not been defined yet.)

$$\frac{\vdash\phi\\;t}{\vdash \textit{abs}(\textit{rep}\\;a)=a,\quad\vdash\phi\\;r=(\textit{rep}(\textit{abs}\\;r)=r)}\mathsf{defineTypeOp}\\;\textit{abs}\\;\textit{rep}\\;\vec{\alpha}$$

**Convention:** Most HOLs use `SCREAMING_SNAKE_CASE` for the inference
rules. You probably want to follow that convention, but it might be _passé_.

**Remark:** _Design decisions catching up with us._<br>
Again, depending on the design decisions made earlier (are you using a
state monad?), our hand is forced on the design of the kernel.

For people making a "purely functional" kernel, you will need to make
the kernel into a state monad. For Standard ML programmers, this means
adding a `type state` to the specification (which will look something
like `{type_constants : (string * int) list, term_constants :
(string * Type.t) list, axioms : thm list, definitions : (string *
thm) list}`). The state will be updated as new definitions and axioms
are added. The initial state will define the primitive types and basic
constants. The inference rules will not return a `thm` but `state -> thm * state`,
and we'll compose everything together using `bind` operators.

Magnus O. Myreen, Scott Owens, and Ramana Kumar's
["Steps Towards Verified Implementations of HOL Light"](https://www.cl.cam.ac.uk/~mom22/itp13.pdf)
sketches out what a monadic purely functional HOL kernel looks like.

**Specification.**

The kernel has the following specification:

```sml
signature KERNEL = sig
  type thm;

  val dest_thm : thm -> Term.t list * Term.t;
  val concl : thm -> Term.t;
  val hyp : thm -> Term.t list; 

  (* inference rules *)
  val refl : Term.t -> thm;
  val assume : Term.t -> thm;
  val eqMp : thm -> thm -> thm;
  val absThm : Term.t -> thm -> thm;
  val appThm : thm -> thm -> thm;
  val deductAntisym : thm -> thm -> thm;
  val termSubst : (Term.t,Term.t) Subst.t -> thm -> thm;
  val typeSubst : (Type.t,Type.t) Subst.t -> thm -> thm;
  val betaConv : Term.t -> Term.t -> thm;
  (* defineConst "c" rhs *)
  val defineConst : string -> Term.t -> thm;

  val defineTypeOp : { abs : string
                     , rep : string
                     , tyvars : string list} ->
                     thm ->
                     thm * thm;

  (* danger! *)
  val new_axiom : Term.t -> thm;
end;
```

**Optional:** 

1. You can include eta conversion ($\vdash(\lambda x\ldotp t\\;x)=t$)
   as an initial axiom at this point.
2. You may want to add some functions like `reset_axioms : unit -> unit`
   to completely forget all axioms, or `pop_axiom : unit -> thm` to
   remove the last axiom registered using `new_axiom` and present it
   to the user.

#### Initial axioms

Once you have defined the quantifiers and impliciation connective, you
will need to include at least the following axiom concerning the type
of individuals:

$$\vdash\exists(f:\mbox{ind}\to\mbox{ind})\ldotp(\forall x_{1},x_{2}\ldotp f(x_{1})=f(x_{2})\implies x_{1}=x_{2})\land(\exists y\ldotp\forall x\ldotp y\neq f(x))$$

This is the axiom of infinity, stating that `ind` is infinite.

The other axiom (consistent with intuitionistic HOL) is eta conversion:

$$\vdash(\lambda x\ldotp t\\;x)=t$$

...which will then give us extensionality.

#### Example Kernels

The [OpenTheory/src/Thm.sig](https://github.com/gilith/opentheory/blob/master/src/Thm.sig)
specification sketches out the type signatures for one HOL kernel.

HOL Light's kernel can be found in [hol-light/fusion.ml](https://github.com/jrh13/hol-light/blob/master/fusion.ml).

HOL4's kernel specification is [FinalThm-sig.sml](https://github.com/HOL-Theorem-Prover/HOL/blob/develop/src/prekernel/FinalThm-sig.sml)
and is implemented in [std-thm.ML](https://github.com/HOL-Theorem-Prover/HOL/blob/develop/src/thm/std-thm.ML).
According to [these slides](https://www.kth.se/social/files/595631e456be5b96c04314d6/itp-course.pdf),
`HOL4/src/0/` contains the "standard kernel".

<a name="step-7-natural-deduction-rules"></a>

### Step 7: Natural Deduction Rules

In the code I wrote, I am working with only one primitive constant
(polymorphic equality).

Besides the primitive inference rules given in the kernel, we need
some "quality of life" inference rules.

> **Confession:** At this point, since I am so close to [HOL Light](https://github.com/jrh13/hol-light)
> presentation, I am following its [hol_lib.ml](https://github.com/jrh13/hol-light/blob/master/hol_lib.ml)
> in guiding the next several steps. This step roughly corresponds to [equal.ml](https://github.com/jrh13/hol-light/blob/master/equal.ml). But I am taking some liberties
> which may be painful to the user-experience, like delaying the
> printer and parser code.
> 
> Again, my goal is to make this project _educational_ and it seems to
> me that developping the HOL logic is more educational than pretty printing or
> writing a parser.

We will be writing a few _derived_ inference rules. These are
functions built using only the inference rules from the Kernel. This
guarantees "correctness-by-construction": if we do not have a bug in
the Kernel, then these rules should be sound.

The inference rules we want include:

$$\frac{\Gamma s=t,\quad\Delta t=u}{\Gamma\cup\Delta\vdash s=u}\mathsf{trans}$$

(Now, after deriving "trans", we will have the same inference rules at
our disposal as HOL Light's primitive 10 inference rules provided by
its kernel.)

$$\frac{\Gamma\vdash x = y}{\Gamma\vdash f(x) = f(y)}\mathsf{apTerm}\\;f$$

$$\frac{\Gamma\vdash f = g}{\Gamma\vdash f(x) = g(x)}\mathsf{apTerm}\\;x$$

$$\frac{\Gamma\vdash t_{1} = t_{2}}{\Gamma\vdash t_{2}=t_{1}}\mathsf{sym}$$

When $t_{1}$ is alpha congruent to $t_{1}'$,

$$\frac{}{\vdash t_{1}=t_{1}'}\mathsf{alpha}\\;t_{1}\\;t_{1}'$$

This is also tied to the axiom schema $\mathsf{alphaConv}(y :
T,\lambda x : T\ldotp t) = \vdash (\lambda x\ldotp t) = (\lambda
y\ldotp t[y/x])$
provided that $y$ does not occur free in $t$. You may wish to
implement this as a function. (Also, implement `Term.is_free_in` to
test if one term is free in another.)

More inference rules to consider:

$$\frac{\vdash \ell_{1}=\ell_{2},\quad\vdash r_{1}=r_{2}}{\vdash b\\;\ell_{1}\\;r_{1}=b\\;\ell_{2}\\;r_{2}}\mathsf{mkBinop}\\;b$$

(provided the types all agree)

**Goal:** implement these inference rules as functions 
- `trans : thm -> thm -> thm`
- `apTerm : Term.t -> thm -> thm`
- `apThm : Term.t -> thm -> thm`
- `sym : thm -> thm`
- `alpha : Term.t -> Term.t -> thm`
- `alphaConv : Term.t -> Term.t -> thm`
- `mkBinop : Term.t -> thm -> thm -> thm`

Construct them only using the primitives from the Kernel.

**Optional:** It is customary now to also use "conversions" to
transform a term $t$ into a theorem of the form $\vdash t=u$.
This can be traced back to Lawrence Paulson's "A Higher-Order
Implementation of Rewriting" (_Sci. Comp. Prog._ **3** (1983) 119-149, [arXiv:cs/9301108](https://arxiv.org/abs/cs/9301108),
[`doi:10.1016/0167-6423(83)90008-4`](https://dx.doi.org/10.1016/0167-6423(83)90008-4)).

Why bring this up? Because term rewriting is a critical aspect to
theorem proving. It will be incredibly useful later on to build
derived inference rules which will include term-rewriting for us, to
simplify the user's life.

But I am focusing on the educational aspect of implementing proof
assistants, so to Hell with user experience.

### Step 8: Derived Connectives and Rules

So far, we only have two or three "primitive" constants. We need to
define the remaining logical connectives and quantifiers in terms of
our primitives.

For example, HOL Light defines:
- $\top := \bigl((\lambda p\ldotp p) = (\lambda p\ldotp p)\bigr)$
- $\land := \lambda p\ldotp\lambda q\ldotp(\lambda f\ldotp f\\;p\\;q)=(\lambda f\ldotp\\;f\\;\top\\;\top)$
- $\Longrightarrow := \lambda p\ldotp\lambda q\ldotp (p\land q \iff p)$
  where we write $\iff$ instead of $=$ to stress we are working with
  Boolean terms
- $\forall := \lambda P\ldotp P = \lambda x\ldotp\top$ where we will
  henceforth write $\forall x\ldotp P[x]$ instead of
  $(\forall)(\lambda x\ldotp P[x])$
- $\exists := \lambda P\ldotp\forall q\ldotp(\forall x\ldotp P(X)\implies q)\implies q$
- $\lor := \lambda p\ldotp\lambda q\ldotp\forall r\ldotp(p\implies r)\implies(q\implies r)\implies r$
- $\bot := \forall p\ldotp p$
- $\neg := \lambda p\ldotp p\implies\bot$
- $\exists! := \lambda P\ldotp\exists P\land\forall x\ldotp\forall y\ldotp(P\\;x\land P\\;y\implies x=y)$

(These are all defined in [hol-light/bool.ml](https://github.com/jrh13/hol-light/blob/master/bool.ml),
and analogous definitions may be found in [HOL4/src/1/boolSyntax.sml](https://github.com/HOL-Theorem-Prover/HOL/blob/develop/src/1/boolSyntax.sml).)

These are all sufficient for us to define the intuitionistic natural
deduction rules for these connectives using the primitive inference
rules from the kernel. We can make them classical by adding a new
axiom.

**Goals:**
1. Define the quantifiers and logical connectives enumerated above
2. Write down the natural deduction inference rules governing their usage

The specification for such a module:

```sml
signature bool = sig
  val is_iff : Term.t -> bool;
(* ------------------------------------------------------------------------- *)
(* Verum --- "true" or "T" *)
(* ------------------------------------------------------------------------- *)
  val T_DEF : thm; (* result of new_basic_def for true *)
  val TRUTH : thm; (* |- true *)
  (*
    A |- tm <=> T
   ---------------  EQT_ELIM
      A |- tm
  *)
  val EQT_ELIM : thm -> thm;
  (*
      A |- tm
   ---------------  EQT_INTRO
    A |- tm <=> T
  *)
  val EQT_INTRO : thm -> thm;
  
(* ------------------------------------------------------------------------- *)
(* Conjunction *)
(* ------------------------------------------------------------------------- *)
  (* AND_DEF is |- (/\) = (\p q. (\f. f p q) = (\f. f true true)) *)
  val AND_DEF : thm;
  val mk_conj : Term.t * Term.t -> Term.t;
  val dest_conj : Term.t -> Term.t * Term.t;
  val is_conj : Term.t -> bool;
  (*
    A1 |- t1      A2 |- t2
   ------------------------  CONJ
     A1 u A2 |- t1 /\ t2
  (introduction rule)
  *)
  val CONJ : thm -> thm -> thm;
  (*
    A |- t1 /\ t2
   ---------------  CONJUNCT1
       A |- t1
  (elimination rule 1)
  *)
  val CONJUNCT1 : thm -> thm;
  (*
    A |- t1 /\ t2
   ---------------  CONJUNCT2
       A |- t2
  (elimination rule 2)
  *)
  val CONJUNCT2 : thm -> thm;
  (*
       A |- t1 /\ t2
   ----------------------  CONJ_PAIR
    A |- t1      A |- t2
  *)
  val CONJ_PAIR : thm -> thm * thm;
  
(* ------------------------------------------------------------------------- *)
(* Implication *)
(* ------------------------------------------------------------------------- *)
  val IMP_DEF : thm; (* (==>) := \p q. p /\ q = p *)
  val mk_imp : Term.t * Term.t -> Term.t;
  val is_imp : Term.t -> bool;
  val dest_imp : Term.t -> Term.t * Term.t;
  (*
    A1 |- t1 ==> t2   A2 |- t1
   -----------------------------  MP
          A1 \/ A2 |- t2
  (elimination rule)
  *)
  val MP : thm -> thm -> thm;
  (*
          A |- t
   --------------------  IMP_INTRO u
    A - {u} |- u ==> t
  *)
  val IMP_INTRO : Term.t -> thm -> thm;
  (*
         A1, ..., An |- t
   ----------------------------  IMP_INTRO_ALL
    |- A1 ==> ... ==> An ==> t
  *)
  val IMP_INTRO_ALL : thm -> thm;
  (*
   A1 |- t1 ==> t2     A2 |- t2 ==> t1
  -------------------------------------  IFF_INTRO
          A1 \/ A2 |- t1 = t2

Note: this may require introducing an "UNDISC" rule

    A |- t1 ==> t2
   ----------------  UNDISCH
     A, t1 |- t2
  *)
  val IFF_INTRO : thm -> thm -> thm;
  (*
       A |- t
   ---------------  WEAKEN `s`
      A, s |- t
  *)
  val WEAKEN : Term.t -> thm -> thm;
  (*
              A |- t1 <=> t2
   -----------------------------------  IFF_ELIM
    A |- t1 ==> t2     A |- t2 ==> t1
  *)
  val IFF_ELIM : thm -> thm * thm;
  (*
    A1 |- t1 ==> t2   A2 |- t2 ==> t3
   ------------------------------------  IMP_TRANS
         A1 \/ A2 |- t1 ==> t3
  *)
  val IMP_TRANS : thm -> thm -> thm;
(* ------------------------------------------------------------------------- *)
(* Universal quantifier *)
(* ------------------------------------------------------------------------- *)
  (* (!) = \P:A->bool. P = \x. true *)
  val FORALL_DEF : thm;
  val mk_forall : Term.t * Term.t -> Term.t;
  val dest_forall : Term.t -> Term.t * Term.t;
  val is_forall : Term.t -> bool;
  (*
     A |- !x. t
   --------------  SPEC u
    A |- t[u/x]
  (i.e., forall elimination)
  *)
  val SPEC : Term.t -> thm -> thm;
  (*
      A |- t
   ------------  GEN x    [where x is not free in A]
    A |- !x. t
  (i.e, forall introduction)
  *)
  val GEN : Term.t -> thm -> thm;
(* ------------------------------------------------------------------------- *)
(* Existential quantifier *)
(* ------------------------------------------------------------------------- *)
  (* (?) = \P:A->bool. !q. (!x. P x ==> q) ==> q *)
  val EXISTS_DEF : thm;
  val mk_exists : Term.t * Term.t -> Term.t;
  val dest_exists : Term.t -> Term.t * Term.t;
  val is_exists : Term.t -> bool;

  (*
    A |- p[u/x]
   -------------  EXISTS (`?x. p`,`u`)
    A |- ?x. p
  (intro rule for existential quantifier)

  The first argument is a quantified pattern, indicating the
    desired shape of the conclusion of the resulting theorem.
  *)
  val EXISTS : Term.t * Term.t -> thm -> thm;
  (*
    A |- p
   -------------  SIMPLE_EXISTS x
    A |- ?x. p
  *)
  val SIMPLE_EXISTS : Term.t -> thm -> thm;
  (*
    A1 |- ?x. s[x]    A2 |- t
   -------------------------------  CHOOSE (v, (A1 |- ?x. s))
     A1 u (A2 - {s[v/x]}) |- t

where v is not free in A2 - {s[v/x]}.
  *)
  val CHOOSE : Term.t * thm -> thm -> thm;

(* ------------------------------------------------------------------------- *)
(* Disjunction *)
(* ------------------------------------------------------------------------- *)
  (* (\/) = \p q. !r. (p ==> r) ==> (q ==> r) ==> r *)
  val OR_DEF : thm;

  val mk_disj : Term.t * Term.t -> Term.t;
  val dest_disj : Term.t -> Term.t * Term.t;
  val is_disj : Term.t -> bool;

  (*
       A |- t1
   ---------------  DISJ1 (A |- t1) `t2`
    A |- t1 \/ t2
  (intro rule for disjunction)
  *)
  val DISJ1 : thm -> Term.t -> thm;
  
  (*
       A |- t2
   ---------------  DISJ2 (A |- t2) `t1`
    A |- t1 \/ t2
  (intro rule for disjunction)
  *)
  val DISJ2 : thm -> Term.t -> thm;

  (*
         A |- t1 \/ t2     A1 |- t      A2 |- t
   --------------------------------------------------  DISJ_CASES
       A \/ (A1 - {t1}) \/ (A2 - {t2}) |- t
  (elimination rule for disjunction)
  *)
  val DISJ_CASES : thm -> thm -> thm -> thm;

(* ------------------------------------------------------------------------- *)
(* Falsum and Negation *)
(* ------------------------------------------------------------------------- *)
  (* F = !p:bool. p *)
  val F_DEF : thm;
  (* (~) = \p. p ==> F *)
  val NOT_DEF : thm;

  val mk_neg : Term.t -> Term.t;
  val dest_neg : Term.t -> Term.t;
  val is_neg : Term.t -> bool;
  (*
      A |- ~t
   --------------  NOT_ELIM
    A |- t ==> F
  *)
  val NOT_ELIM : thm -> thm;
  (*
    A |- t ==> F
   --------------  NOT_INTRO
      A |- ~t
  *)
  val NOT_INTRO : thm -> thm;
  (*
     A |- ~tm
   ---------------  EQF_INTRO
    A |- tm <=> F
  *)
  val EQF_INTRO : thm -> thm;
  (*
    A |- tm <=> F
   ---------------  EQF_ELIM
     A |- ~tm
  *)
  val EQF_ELIM : thm -> thm;
  (*
    A |- F
   --------  F_ELIM t
    A |- t
  *)
  val F_ELIM : Term.t -> thm -> thm;
  
(* ------------------------------------------------------------------------- *)
(* Unique existence quantifier *)
(* ------------------------------------------------------------------------- *)
  (* (?!) = \P:A->bool. ((?) P) /\ (!x y. P x /\ P y ==> x = y) *)
  val EX_UNIQUE_DEF : thm;
  val mk_uexists : Term.t * Term.t -> Term.t;
  val dest_uexists : Term.t -> Term.t * Term.t;
  val is_uexists : Term.t -> bool;

  (*
    A |- ?!x. p
   -------------  EXISTENCE
    A |- ?x. p
  *)
  val EXISTENCE : thm -> thm;
end;
```

### Step 9: Tactics

So now we have introduced all the inference rules for intuitionistic
natural deduction of higher-order logic. What now?

Well, HOL is an LCF-style proof assistant, which works with tactics to
prove theorems.

When we state a theorem, there is a "proof obligation" the user needs
to establish as true using the inference rules we have just derived
(or implemented in the Kernel). 

But natural deduction inference rules are applied "bottom up": we
transform the "proof obligation" until it is an axiom or a previous
result. 

We can call these "proof obligations" (together with some additional
metadata and a verification function [which inference rule is used to
obtain the result]) in LCF-style provers **"goals"**.

At any given time, we will have a stack of goals. We can manipulate
the top of the stack using tactics.

We may now introduce them as something like:

```sml
signature TACTIC = sig
  (* assumptions are labeled theorems *)
  type goal = { assumptions : (string * thm) list
              , target : Term.t };

  type validation; (* thm list -> thm *)
  type goal_state; (* { subgoals : goal list,
                      , metavariables : Term.t list
                      , justify : validation
                      } *)
  type goal_stack; (* list of goal states *)
  type tactic; (* = goal -> goalstate *)
  type thm_tactic; (* thm -> tactic *)

  val prove : Term.t * tactic -> thm;

  val THEN : tactic -> tactic -> tactic;
end;
```

### Step A: Printer and Parser

Usually a HOL proof assistant has a printer and parser which allows
the user to write "pretty printed" formulas and manipulate them with
ease. 

# Make a HOL

Like [MAL](https://github.com/kanaka/mal) but you're making a HOL
proof assistant instead of a Lisp.

If you want to build your own HOL, take a look at the
[guide](./guide.md). This is largely inspired by Michael Norrish's [slides](https://cgi.cse.unsw.edu.au/~kleing/teaching/thprv-04/slides/slides-HOL4.pdf)
about HOL.

I wrote one possible HOL using Moscow ML (so you can use it on your
Raspberry Pi). It is written for clarity and simplicity, to help
readers get started with an impure (but mostly functional) HOL.

## Basic Milestones

The milestones in creating a HOL are roughly as follows:

1. Implement an abstract syntax tree for simply-typed lambda calculus
   with three base types `bool`, `ind`, `('a,'b) fun` for the type of
   functions from `'a` to `'b`
   
   You'll need to pick some way to represent bound variables (for a
   review of the possibilities, see Jesper Cockx's [1001 Representations of Syntax with Binding](https://jesper.sikanda.be/posts/1001-syntax-representations.html)).
2. Implement the kernel (which consists of an abstract data type `thm`
   and the only public facing "constructors" are the primitive
   inference rules for the logic, a few "primitive" constructors; also
   support the basic definition mechanisms for new constants and new
   types).
   
   You need to decide on the proof calculus for the underlying
   logic. Most HOLs use Gentzen-esque natural deduction (sequents with
   exactly one consequent). You could also use a Hilbert calculus, see
   Peter B Andrews's _An Introduction to Mathematical Logic and Type
   Theory: To Truth Through Proof_ (Springer, 2013) for an example of
   such a calculus.
   
   You need to decide if you are working with _intuitionistic_ or _classical_
   higher-order logic, because it will affect your choices made
   here. Most HOLs are classical by axiomatically including a Hilbert
   choice operator.
   
   The minimal choice of primitives is just polymorphic equality
   `= : 'a -> 'a -> bool` (HOL Light does this). Another possible
   choice (following Lambek and Scott): `forall : ('a -> bool) -> bool`,
   `implies : bool -> bool -> bool`, `and : bool -> bool -> bool`,
   and `T : bool` (the canonically valid proposition).

   Technically, at this step, you're done: you've implemented a
   HOL. Congratulations. But to make it actually useful, you should do
   a few more things.
3. Add a simple term rewriting system; for a description of the API,
   see sections 4 et seq of:
   - Lawrence Paulson,
     "A higher-order implementation of rewriting." 
     _Science of computer programming_ **3**, no. 2 (1983) 119-149.
     [Eprint](https://core.ac.uk/download/pdf/81958412.pdf)
   
   This will make it easier to implement the remaining logical
   connectives and quantifiers.
4. Implement the derived connectives and their inference rules. This
   milestone's details depends critically on the choice of primitives
   made in step 2. At the very least, you should support the
   intuitionistic natural deduction inference rules for the usual
   connectives and quantifiers.
5. Grow at least what is discussed in Section 5.2 of [HOL4's "Description"](http://sourceforge.net/projects/hol/files/hol/trindemossen-1/trindemossen-1-description.pdf/download)
   for the "basic theories".

Realistically, you should also support:
- goals, tactics, and tacticals;
- some sort of mechanism for "theories" (a module system for HOL
  identifiers to avoid collision);
- printers and parsers (support for infix operators);
- recursive definitions;
- inductive definitions (and mutually recursive inductive definitions).

You could really go wild with this, adding support for a custom REPL,
type inference, code generators, etc.

That should disabuse the reader of ever thinking about making a HOL
ever again...

## Stuff to think about

Term rewriting is discussed thoroughly in Richard Boulton's [PhD dissertation](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-337.html).

- Term rewriting systems are implemented differently, and obey
  different APIs...
  + Isabelle/HOL uses an extraordinarily sophisticated term rewriting
    system which they call "simplification". It's implemented in the
    Isabelle/Pure metalogic.
  + HOL Light uses "conversions" to transform a term into a theorem,
    and "conversionals" to build new conversions out of old ones (see
    section 7 of the [HOL Light tutorial](https://www.cl.cam.ac.uk/~jrh13/hol-light/tutorial_220.pdf)
    for details)
  + HOL4 uses "conversions" to apply equality theorems (`|- LHS = RHS`
    is applied to replace instances of `LHS` appearing in an
    expression by `RHS`) and "conversionals" to compose conversions
    together
- Inductive definitions should be supported

## References

- Konrad Slind's "An Implementation of Higher Order Logic".
  Master's thesis, U. of Calgary, 1990;
  [PDF](https://ucalgary.scholaris.ca/server/api/core/bitstreams/1e7e0936-2b03-47f4-8b64-628129bcd20c/content)
  + This is a really great read. If you get stuck trying to understand
    something, this is a good place to turn for help.
- Thomas Tuerk's [Interactive Theorem Proving (ITP) Course Web Version](https://hol-theorem-prover.org/hol-course.pdf)

### Term rewriting

For term rewriting systems...

- Lawrence Paulson,
  "A higher-order implementation of rewriting." 
  _Science of computer programming_ **3**, no. 2 (1983) 119-149.
  [Eprint](https://core.ac.uk/download/pdf/81958412.pdf)

For a completely different take on term rewriting subsystems of proof
assistants, see:

- Issam Maamria and Michael Butler,
  "Rewriting and Well-Definedness within a Proof System".
  _EPTCS 43_, 2010, pp. 49-64
  [arXiv:1012.4897](https://arxiv.org/abs/1012.4897), 16 pages

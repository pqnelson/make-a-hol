signature boolSyntax = sig
  (* primitive types, synonyms *)
  val Bool : Type.t;
  val Ind  : Type.t;

  val is_bool : Term.t -> bool;
  val is_ind  : Term.t -> bool;
  
  (* constants *)
  val equality    : Term.t;
  val implication : Term.t;
  val T           : Term.t; (* verum *)
  val F           : Term.t; (* falsum *)
  val universal   : Term.t;
  val existential : Term.t;
  val ex_unique   : Term.t;
  val conjunction : Term.t;
  val disjunction : Term.t;
  val negation    : Term.t;

  (* Public-facing constructors *)
  val mk_eq        : Term.t * Term.t -> Term.t;
  val mk_imp       : Term.t * Term.t -> Term.t;
  val mk_forall    : Term.t * Term.t -> Term.t;
  val mk_exists    : Term.t * Term.t -> Term.t;
  val mk_ex_unique : Term.t * Term.t -> Term.t;
  val mk_conj      : Term.t * Term.t -> Term.t;
  val mk_disj      : Term.t * Term.t -> Term.t;
  val mk_neg       : Term.t -> Term.t;

  (* Destructuring methods *)
  val dest_eq        : Term.t -> Term.t * Term.t;
  val dest_imp       : Term.t -> Term.t * Term.t;
  val dest_forall    : Term.t -> Term.t * Term.t;
  val dest_exists    : Term.t -> Term.t * Term.t;
  val dest_ex_unique : Term.t -> Term.t * Term.t;
  val dest_conj      : Term.t -> Term.t * Term.t;
  val dest_disj      : Term.t -> Term.t * Term.t;
  val dest_neg       : Term.t -> Term.t;
  
  (* Predicates *)
  val is_eq        : Term.t -> bool;
  val is_imp       : Term.t -> bool;
  val is_forall    : Term.t -> bool;
  val is_exists    : Term.t -> bool;
  val is_ex_unique : Term.t -> bool;
  val is_conj      : Term.t -> bool;
  val is_disj      : Term.t -> bool;
  val is_neg       : Term.t -> bool;
end;

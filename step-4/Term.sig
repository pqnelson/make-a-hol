signature Term = sig
  eqtype t;
  type Term = t;

  val compare : Term * Term -> order;
  val eq : Term -> Term -> bool;

  val mk_var : string * Type.t -> Term;
  val mk_const : string * Type.t -> Term;
  val mk_app : Term * Term -> Term;
  val mk_abs : Term * Term -> Term;
  
  val dest_var : Term -> string * Type.t;
  val dest_const : Term -> string * Type.t;
  val dest_app : Term -> Term * Term;
  val dest_abs : Term -> Term * Term;

  (* type instantiation *)
  val inst : (Type.t, Type.t) Subst.t -> Term -> Term;

  val subst : (Term, Term) Subst.t -> Term -> Term;

  val free_vars : Term -> Term list;

  val type_of : Term -> Type.t;

  val pprint : Term -> string;

  val aconv : Term -> Term -> bool;

  (* matching terms against each other *)
  exception Match of string;
  val raw_match : Type.t list -> t list ->
                  t -> t ->
                  ((t,t) Subst.t * (Type.t,Type.t) Subst.t) ->
                  (((t,t) Subst.t * t list) *
                   ((Type.t,Type.t) Subst.t * Type.t list));
  
  val matches : t list -> Type.t list ->
                t -> t ->
                ((t,t) Subst.t * (Type.t,Type.t) Subst.t);
  
  val match_term : t -> t ->
                   ((t,t) Subst.t * (Type.t,Type.t) Subst.t);
end;


signature Term = sig
  eqtype t;
  type Term = t;

  val compare : Term * Term -> order;
  val eq : Term -> Term -> bool;

  val mk_var : string * Type.t -> Term;
  val mk_const : string * Type.t -> Term;
  val mk_app : Term * Term -> Term;
  val mk_abs : Term * Term -> Term;
  
  exception Dest of string;
  val dest_var : Term -> string * Type.t;
  val dest_const : Term -> string * Type.t;
  val dest_app : Term -> Term * Term;
  val dest_abs : Term -> Term * Term;

  val is_var : Term -> bool;
  val is_fvar : Term -> bool;
  val is_bvar : Term -> bool;
  val is_const : Term -> bool;
  val is_app : Term -> bool;
  val is_abs : Term -> bool;

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
  
  (* mutable state :( *)
  val new_const : string * Type.t -> unit;

  (* quality-of-life functions *)
  val is_bool : Term -> bool;
  val curry_eq : Term -> Term;
  val mk_eq : Term * Term -> Term;
  val is_eq : Term -> bool;
  val dest_eq : Term -> Term * Term;
  
end;


signature Kernel = sig
  eqtype thm;

  val dest_thm : thm -> Term.t list * Term.t;
  val concl : thm -> Term.t;
  val hyp : thm -> Term.t list;

  (* If we accidentally introduced an inconsistent axiom,
     and we want a "hard reset" to the initial axioms,
     run `reset_axioms` *)
  val reset_axioms : unit -> unit;

  (* OpenTheory's inference rules *)
  val refl : Term.t -> thm;
  val assume : Term.t -> thm;
  val eqMp : thm -> thm -> thm;
  val absThm : Term.t -> thm -> thm;
  val appThm : thm -> thm -> thm;
  val deductAntisym : thm -> thm -> thm;
  val subst : (Term.t, Term.t) Subst.t -> thm -> thm;
  val betaConv : Term.t -> Term.t -> thm;
  (* defineConstant "c" rhs *)
  val defineConst : string -> Term.t -> thm;
  (* defineTypeOp *)
  val defineTypeOp : { name : string
                     , abs : string
                     , rep : string
                     , tyvars : string list } ->
                     thm ->
                     thm * thm;
  (* danger! *)
  val new_axiom : Term.t -> thm;
end;

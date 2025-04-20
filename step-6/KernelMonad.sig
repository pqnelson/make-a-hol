signature KernelMonad = sig
  eqtype thm;

  val dest_thm : thm -> Term.t list * Term.t;
  val concl : thm -> Term.t;
  val hyp : thm -> Term.t list;

  (* Exception monad transformer applied to State monad *)
  structure State : sig t end;
  type 'a m; (* monad abstract type *)
  val return : 'a -> 'a m;
  val bind : 'a m -> ('a -> 'b m) -> 'b m;
  val runState : 'a m -> (State.t -> 'a * State.t);
  val initial_intuitionistic_state : State.t;
  val initial_classical_state      : State.t;

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
                     , tyvars : string list} ->
                     thm ->
                     thm * thm;
  (* danger! *)
  val new_axiom : Term.t -> thm;
end;

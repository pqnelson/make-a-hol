structure Kernel :> Kernel = struct
  datatype thm = Sequent of Term.t list * Term.t;

  fun dest_thm (Sequent x) = x;
  fun concl (Sequent (_, fm)) = fm;
  fun hyp (Sequent (asms, _)) = asms;

  (*
  local
    open boolSyntax;
  in
  (* infinity_fm is
∃ (f : ind → ind). (∀ x1, x2. (f x1 = f x2) ⟹ (x1 = x2)) ∧
                   (∃ y. ∀ x. y ≠ f x )
   *)
  val infinity_fm =
    let
      val f = Term.mk_var("f", Type.mk_fun(Ind,
                                           Ind));
      fun mk_ind name = Term.mk_var(name, Ind);
      val (x,x1,x2,y) = (mk_ind "x", mk_ind "x1",
                         mk_ind "x2", mk_ind "y");
    in
      mk_exists(f,
                mk_conj(mk_imp(mk_eq(Term.mk_app(f,x1),
                                     Term.mk_app(f,x2)),
                               mk_eq(x1, x2)),
                        mk_exists(y,
                                  mk_forall(x, mk_neg(mk_eq(y,Term.mk_app(f,x)))))))
    end;

  (* eta_fm = ∀t. (λx. t x) = t *)
  val eta_fm =
    let
      val a = Type.mk_var("'a");
      val b = Type.mk_var("'b");
      val t = Term.mk_var("t",Type.mk_fun(a,b));
      val x = Term.mk_var("x",a);
    in
      mk_forall(t,
                mk_eq(Term.mk_abs(x,Term.mk_app(t,x)),
                      t))
    end
  end;

  val initial_axioms = [Sequent([], eta_fm),
                        Sequent([], infinity_fm)];
  *)
  val the_axioms : (thm list) ref = ref [];

  fun reset_axioms () =
    (the_axioms := [];
     ());
  
  (*** Helper functions to treat hypotheses like a set ***)
  fun hyp_contains (t : Term.t) hyps =
    List.exists (fn h => Term.eq t h) hyps;

  (* insert_hyp : Term.t -> Term.t list -> Term.t list
  Adds first argument if it is absent from the second
  argument, then returns the new list; otherwise returns the
  second argument unmodified. *)
  fun insert_hyp (t : Term.t) [] = [t]
    | insert_hyp t (hyps as (h::hs)) =
      if hyp_contains t hyps
      then hyps
      else t::hyps;

  fun rm_hyp t [] = []
    | rm_hyp t (h::hs) =
      if Term.eq t h then hs
      else h::(rm_hyp t hs);

  fun union_hyps [] deltas = deltas
    | union_hyps (g::gs) deltas =
      if List.null deltas
      then (g::gs)
      else insert_hyp g (union_hyps gs deltas);

  fun subst_hyps (s : (Term.t, Term.t) Subst.t) hs =
    map (Term.subst s) hs;

  fun inst_hyps (s : (Type.t, Type.t) Subst.t) hs =
    map (Term.inst s) hs;

  (*** Inference Rules ***)
  (*
   ------------- refl t
     |- t = t
  *)
  fun refl t = Sequent([], (Term.mk_eq(t,t)));
  
  (*
  -------------- assume phi
    phi |- phi
  *)
  fun assume phi =
    if not (Term.is_bool phi)
    then raise Fail "assume: expected formula"
    else Sequent([phi],phi);

  (*
    A1 |- lhs = rhs,   A2 |- lhs
  -------------------------------- EQ_MP
          A1 \/ A2 |- rhs
  *)
  fun eqMp th1 th2 =
    let
      val (lhs, rhs) = Term.dest_eq(concl th1);
    in
      if not (Term.aconv lhs (concl th2))
      then raise Fail "eqMp: expected th2 to prove minor premise"
      else Sequent(union_hyps (hyp th1) (hyp th2), rhs)
    end;

  (*
     A1 |- t = u
  --------------------------- absThm v
    A1 |- (\v. t) = (\v. u)
  HOL Light calls this ABS
  *)
  fun absThm v th =
    if not (Term.is_fvar v)
    then raise Fail "absThm: needs to be given a free variable"
    else
      let
        val (lhs,rhs) = Term.dest_eq(concl th);
      in
        Sequent(hyp th,
                Term.mk_eq(Term.mk_abs(v,lhs),
                           Term.mk_abs(v,rhs)))
      end;

  (*
    A1 |- f = g,  A2 |- x = y  
  ------------------------------ appThm
      A1 \/ A2 |- f x = g y
  HOL Light calls this MK_COMB. It underlies the term
  rewriting system of conversions.
  *)
  fun appThm th1 th2 =
    let
      val (f,g) = Term.dest_eq(concl th1);
      val (x,y) = Term.dest_eq(concl th2);
    in
      Sequent (union_hyps (hyp th1) (hyp th2),
               Term.mk_eq(Term.mk_app(f,x),
                          Term.mk_app(g,y)))
    end;

  (*
          A1 |- phi,    A2 |- psi
  ------------------------------------------ deductAntisym
    (A1 \ phi) \/ (A2 \ psi) |- phi = psi 
  *)
  fun deductAntisym th1 th2 =
    Sequent(union_hyps (rm_hyp (concl th2) (hyp th1))
                       (rm_hyp (concl th1) (hyp th2)),
            Term.mk_eq(concl th1, concl th2));

  (*
           A1 |- phi 
  ---------------------------- termSubst sigma
    A1[sigma] |- phi[sigma]
  *)
  fun termSubst (s : (Term.t, Term.t) Subst.t) th =
    Sequent(subst_hyps s (hyp th),
            Term.subst s (concl th));

  (*
           A1 |- phi 
  ---------------------------- typeSubst sigma
    A1[sigma] |- phi[sigma]
  *)
  fun typeSubst (s : (Type.t, Type.t) Subst.t) th =
    Sequent(inst_hyps s (hyp th),
            Term.inst s (concl th));

  (*
  --------------------------- betaConv (\v.t) u
    |- (\v.t) u = t[u / v]
  *)
  fun betaConv abs_t u =
    let
      val (v,t) = Term.dest_abs abs_t;
      val lhs = Term.mk_app(abs_t,u);
      val rhs = Term.subst [(v,u)] t;
    in
      Sequent([], Term.mk_eq(lhs, rhs))
    end;

  (*
  ------------- defineConst c tm
    |- c = tm

  SIDE EFFECT: adds a new entry to the table of constants (and
  their types)
  *)
  fun defineConst c tm =
    let
      val ty = Term.type_of tm;
    in
      Term.new_const(c, ty); 
      Sequent([], Term.mk_eq(Term.mk_const(c,ty),
                                   tm))
    end;
  
  (*
                    |- P t
  ------------------------------------------------- defineTypeOp
    |- abs(rep a) = a,  |- P r = (rep(abs r) = r) 

  REQUIRES: `name` to be an unused type name
  ENSURES: fails if tyax has hypotheses
           fails if tyax is not either |- exists(v, P v)
                                or |- P t for some term t
  *)
  fun defineTypeOp {name, abs, rep, tyvars} tyax =
    if not (List.null (hyp tyax))
    then raise Fail "defineTypeOp: input theorem must have no assumptions"
    else
      let
        val fm0 = concl tyax;
        val fm = if Term.is_app fm0
                 then fm0
                 else raise Fail ("defineTypeOp: " ^
                                  "input theorem wrong shape");
        val (P, witness) = Term.dest_app fm;
        (* type vars in term *)
        val tyvars = Type.vars_in (Term.type_of P);
        val _ = Type.new_type(name, length tyvars);
        (* uncurry P, if necessary *)
        val (a_ty,r) = Type.dest_fun (Term.type_of P);
        (* REP :: name -> a
           ABS :: a -> name *)
        val ty = Type.mk_type(name, []);
        val ABS = Term.mk_const(abs, Type.mk_fun(a_ty, ty));
        val REP = Term.mk_const(rep, Type.mk_fun(ty, a_ty));
        val mk_eq = Term.mk_eq;
        val a = Term.mk_var("a", a_ty);
        val r = Term.mk_var("r", ty);
      in
        (* XXX: register the new type ty,
                and the new constants ABS and REP *)
        (Sequent([], mk_eq(Term.mk_app(ABS, Term.mk_app(REP, a)),
                           a)),
         Sequent([], mk_eq(Term.mk_app(P, r),
                           mk_eq(Term.mk_app(REP,
                                             Term.mk_app(ABS,r)),
                                 r))))
      end;

  fun new_axiom fm =
    if Term.is_bool fm
    then (let
           val th = Sequent ([], fm)
         in
           the_axioms := th::(!the_axioms);
           th
         end)
    else raise Fail "new_axiom: must be given a formula!";
end;

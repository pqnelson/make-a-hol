structure Kernel :> Kernel = struct
  datatype thm = Sequent of Term.t list * Term.t;

  fun dest_thm (Sequent x) = x;
  fun concl (Sequent (_, fm)) = fm;
  fun hyp (Sequent (asms, _)) = asms;

  
  (*** state monad ***)
  structure State = struct
    datatype def = ConstDef of { name : string
                               , rhs  : Term.t }
                 | TypeDef of { name : string
                              , prop : Term.t
                              , abs  : string
                              , rep  : string }
                 | AxiomDef of Term.t;
    structure Table : Table = MkTable(String);
    type t = { the_type_constants : int Table.t
             , the_term_constants : Type.t Table.t
             , the_axioms : thm list
             , the_definitions : def list
             };
  end;

  (* This should really be the exception monad transformer *)
  datatype ('e,'a) Either = Err of 'e | Ok of 'a;
  datatype 'a m = State of State.t -> (exn,'a) Either * State.t;

  fun runState (State s : 'a m) = s;

  fun return x = State (fn s => (Ok x, s));
  
  fun bind (act1 : 'a m) k =
  State (fn s1 =>
          (case runState m s1 of
             (Ok a, s2) => runState (k a) s2
           | (r as (Err e, s2)) => r)
          handle e => (Err e, s1));
  (*
  (* If we *just* want a state monad, exceptions be damned,
     then the following code works: *)
  datatype 'a m = State of (State.t -> 'a * State.t);

  fun runState (State s : 'a m) = s;
  
  fun return x = State (fn s => (x, s));

  (* bind : 'a m -> ('a -> 'b m) -> 'b m *)
  fun bind (act1 : 'a m) k =
    State (fn s1 =>
              let
                val (a, s2) = runState m s1;
              in
                runState (k a) s2
              end);
  *)
  
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
  (*** Helper functions to treat hypotheses like a set ***)
  fun insert_hyp (t : Term.t) hs =
      if List.exists (fn hy => Term.eq t hy) hs
      then hs
      else t::hs;

  fun rm_hyp t [] = []
    | rm_hyp t (h::hs) =
      if Term.eq t h then hs
      else h::(rm_hyp t hs);

  fun union_hyps [] deltas = deltas
    | union_hyps gammas [] = gammas
    | union_hyps (g::gs) deltas =
      union_hyps gs (insert_hyp g deltas);

  fun subst_hyps (s : (Term.t, Term.t) Subst.t) hs =
    map (Term.subst s) hs;

  (*** Inference Rules ***)
  (*
   ------------- refl t
     |- t = t
  *)
  fun refl t = Sequent([], (boolSyntax.mk_eq(t,t)));
  fun refl t = State (fn s =>
                       (Sequent([], (boolSyntax.mk_eq(t,t))),
                        s));

  (*
  -------------- assume phi
    phi |- phi
  *)
  fun assume phi =
    if boolSyntax.Bool <> Term.type_of phi
    then raise Fail "assume: expected formula"
    else Sequent([phi],phi);

  (*
    A1 |- lhs = rhs,   A2 |- lhs
  -------------------------------- EQ_MP
          A1 \/ A2 |- rhs
  *)
  fun eqMp th1 th2 =
    let
      val (lhs, rhs) = boolSyntax.dest_eq(concl th1);
    in
      if not (Term.aconv lhs (concl th2))
      then raise Fail "eqMp: expected th2 to prove minor premise"
      else Sequent(union_hyps (hyp th1) (hyp th2), rhs)
    end;

  (*
     A1 |- t = u
  --------------------------- absThm v
    A1 |- (\v. t) = (\v. u)
  *)
  fun absThm v th =
    if not (Term.is_fvar v)
    then raise Fail "absThm: needs to be given a free variable"
    else
      let
        val (lhs,rhs) = boolSyntax.dest_eq(concl th);
      in
        Sequent(hyp th,
                boolSyntax.mk_eq(Term.mk_abs(v,lhs),
                                 Term.mk_abs(v,rhs)))
      end;

  (*
    A1 |- f = g,  A2 |- x = y  
  ------------------------------ appThm
      A1 \/ A2 |- f x = g y
  *)
  fun appThm th1 th2 =
    let
      val (f,g) = boolSyntax.dest_eq(concl th1);
      val (x,y) = boolSyntax.dest_eq(concl th2);
    in
      Sequent (union_hyps (hyp th1) (hyp th2),
               boolSyntax.mk_eq(Term.mk_app(f,x),
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
            boolSyntax.mk_eq(concl th1, concl th2));

  (*
           A1 |- phi 
  ---------------------------- subst sigma
    A1[sigma] |- phi[sigma]
  *)
  fun subst (s : (Term.t, Term.t) Subst.t) th =
    Sequent(subst_hyps s (hyp th),
            Term.subst s (concl th));

  (*
  --------------------------- betaConv (\v.t) u
    |- (\v.t) u = t[u / v]
  *)
  fun betaConv abs_t u =
    let
      val (v,t) = Term.dest_abs abs_t;
    in
      Sequent([], boolSyntax.mk_eq(Term.mk_app(abs_t,u),
                                   Term.subst [(v,u)] t))
    end;

  (*
  ------------- defineConst c tm
    |- c = tm
  *)
  fun defineConst c tm =
    let
      val ty = Term.type_of tm;
    in
      Sequent([], boolSyntax.mk_eq(Term.mk_const(c,ty),
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
        val fm = if boolSyntax.is_exists fm0
                  then #2 (boolSyntax.dest_exists fm0)
                  else if Term.is_app fm0
                  then fm0
                  else raise Fail ("defineTypeOp: " ^
                                   "input theorem wrong shape");
        val (P, witness) = Term.dest_app fm;
        (* uncurry P, if necessary *)
        val (a_ty,r) = Type.dest_fun (Term.type_of P);
        (* REP :: name -> a
           ABS :: a -> name *)
        val ty = Type.mk_type(name, []);
        val ABS = Term.mk_const(abs, Type.mk_fun(a_ty, ty));
        val REP = Term.mk_const(rep, Type.mk_fun(ty, a_ty));
        val mk_eq = boolSyntax.mk_eq;
        val mk_forall = boolSyntax.mk_forall;
        val a = Term.mk_var("a", a_ty);
        val r = Term.mk_var("r", ty);
      in
        (* XXX: register the new type ty,
                and the new constants ABS and REP *)
        (Sequent([], mk_forall(a,
                               mk_eq(Term.mk_app(ABS, Term.mk_app(REP, a)),
                                     a))),
         Sequent([], mk_forall(r,
                               mk_eq(Term.mk_app(P, r),
                                     mk_eq(Term.mk_app(REP,
                                                       Term.mk_app(ABS,r)),
                                           r)))))
      end;

  fun new_axiom fm =
    if boolSyntax.is_bool fm
    then Sequent ([], fm)
    else raise Fail "new_axiom: must be given a formula!";
end;

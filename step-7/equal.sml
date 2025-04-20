local
  open Kernel;
  fun rator (tm : Term.t) =
    case Term.dest_app tm of
        (l,r) => l
      | _ => raise Fail "rator: not an app";
in
structure equal = struct
  
  (*
    A1 |- x = y
  -------------------- apTerm f
    A1 |- f x = f y
  *)
  fun apTerm tm =
    let
      val rth = refl tm
    in
      fn th => (appThm rth th
                handle e => raise Fail "apTerm")
    end;
  (*
   A1 |- s = t,  A2 |- t = u
  --------------------------- trans_fast
      A2 \/ A1 |- s = u
  Faster than `trans`, but swaps the group of hypotheses
  before concatenation.
  *)
  fun trans_fast th1 th2 =
    let
      val (s,_) = Term.dest_eq (concl th1);
      val f = Term.curry_eq s;
      (* th3: A2 |- (= s) t  =  (= s) u *)
      val th3 = apTerm f th2;
    in
      eqMp th3 th1
    end;

  (*
    A |- t1 = t2
   --------------  SYM
    A |- t2 = t1
  NOTE: sym works on "simple equations", not equations within
  quantifiers.
  *)
  fun sym th =
    let
      val tm = concl th;
      val (l,r) = Term.dest_eq tm;
      val lth = refl l;
    in
      eqMp (appThm (apTerm (rator (rator tm)) th) lth) lth
    end;
  (*
    A1 |- x = y
  -------------------- apThm f
    A1 |- f x = f y
  *)
  fun apThm tm th =
    appThm th (refl tm)
    handle _ => raise Fail "AP_THM";

  (*
   A1 |- s = t,  A2 |- t = u
  --------------------------- trans_preserve_hyps
      A1 \/ A2 |- s = u
  *)
  fun trans_preserve_hyps th1 th2 =
    let
      val (_,u) = Term.dest_eq (concl th2);
      val f = Term.curry_eq u;
      (* th3': A1 |- (= u) s  =  (= u) t *)
      (* th3: A1 |- (= u) t  =  (= u) s *)
      val th3 = sym (apTerm f th1);
      (* th2': A2 |- u = t *)
      val th2' = sym th2;
      (* th4: A1 \/ A2 |- (= u) s *)
      val th4 = eqMp th3 th2';
    in
      sym th4
    end;
  val trans = trans_preserve_hyps;
  (*

   -------------  ALPHA t1 t2
    |- t1 = t1'
  *)
  fun alpha t1 t2 =
    trans (refl t1) (refl t2)
    handle _ => raise Fail "alpha";

  (*
    |- l = l'       |- r = r'
  ----------------------------- mkBinop op
      |- op l r = op l' r'
  *)
  fun mkBinop binop =
    let
      val afn = apTerm binop
    in
      fn lth => fn rth => appThm (afn lth) rth
    end;
end;
end;

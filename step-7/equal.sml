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
  --------------------------- trans
      A1 \/ A2 |- s = u
  *)
  fun trans th1 th2 =
    let
      val (s,_) = Term.dest_eq (concl th1);
      val f = Term.curry_eq s;
      val th3 = apTerm f th2;
    in
      eqMp th3 th1
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
    A |- t1 = t2
   --------------  SYM
    A |- t2 = t1
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

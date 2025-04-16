structure boolSyntax :> boolSyntax = struct

  val Bool = Type.Bool;
  val Ind  = Type.Ind;

  fun is_bool tm = (Bool = Term.type_of tm);

  fun is_ind tm = (Ind = Term.type_of tm);
  
  (* type variable used in type of constants *)
  val a = Type.mk_var("'a");
  (* constants *)
  local
    val bin_pred_ty = Type.mk_fun(a, Type.mk_fun(a, Bool));
    val connective_ty = Type.mk_fun(Bool,
                                    Type.mk_fun(Bool, Bool));
    val quant_ty = Type.mk_fun(Type.mk_fun(a,Bool),Bool);
  in
  val equality = Term.mk_const("=", bin_pred_ty);
  val implication = Term.mk_const("==>", connective_ty);
  val T = Term.mk_const("T", Bool);
  val F = Term.mk_const("F", Bool);
  val universal   = Term.mk_const("forall", quant_ty);
  val existential = Term.mk_const("exist", quant_ty);
  val ex_unique   = Term.mk_const("ex_unique", quant_ty);
  val conjunction = Term.mk_const("/\\", connective_ty);
  val disjunction = Term.mk_const("\\/", connective_ty);
  val negation    = Term.mk_const("~", Type.mk_fun(Bool,Bool));
  end;
  
  (* Public-facing constructors *)
  fun mk_eq (lhs,rhs) =
    let
      val T1 = Term.type_of lhs;
      val t1 = Term.mk_app (Term.inst [(a,T1)] equality,
                           lhs);
    in
      if T1 = Term.type_of rhs
      then Term.mk_app (t1, rhs)
      else raise Fail ("mk_eq: lhs and rhs have different types")
    end;

  fun mk_imp(ant,conseq) =
    if Bool <> Term.type_of ant
    then raise Fail "mk_imp: antecedent is not a Bool"
    else if Bool <> Term.type_of conseq
    then raise Fail "mk_imp: consequent is not a Bool"
    else Term.mk_app(Term.mk_app(implication, ant), conseq);

  fun mk_binder ctor (p as (bv, _)) =
    Term.mk_app (Term.inst [(a, Term.type_of bv)] ctor
                , Term.mk_abs p);
  
  (* mk_forall (FVar ..., body) *)
  val mk_forall = mk_binder universal;
  val mk_exists = mk_binder existential;
  val mk_ex_unique = mk_binder ex_unique;

  fun mk_conj(con1, con2) =
    if Bool <> Term.type_of con1
    then raise Fail "mk_conj: first arg is not a Bool"
    else if Bool <> Term.type_of con2
    then raise Fail "mk_conj: second arg is not a Bool"
    else Term.mk_app(Term.mk_app(conjunction, con1),con2);

  fun mk_disj(disj1, disj2) =
    if Bool <> Term.type_of disj1
    then raise Fail "mk_disj: first arg is not a Bool"
    else if Bool <> Term.type_of disj2
    then raise Fail "mk_disj: sedisjd arg is not a Bool"
    else Term.mk_app(Term.mk_app(disjunction, disj1),disj2);

  fun mk_neg fm =
    if Bool <> Term.type_of fm
    then raise Fail "mk_neg: called on non-Bool term"
    else Term.mk_app(negation, fm);

  (* Destructuring methods *)
  fun dest_binop msg name binop_const fm =
    let
      val (fm',rhs) = Term.dest_app fm;
      val (lhs,bin) = Term.dest_app fm;
    in
      if Term.eq binop_const bin
      then (lhs,rhs)
      else raise Fail (msg ^": expected "^
                       name ^
                       " but received " ^
                       (Term.pprint bin))
    end;
  val dest_eq = dest_binop "dest_eq" "eq" equality;

  val dest_imp = dest_binop "dest_imp" "imp" implication;

  val dest_forall = dest_binop "dest_forall" "forall" universal;

  val dest_exists = dest_binop "dest_exists" "exists" existential;

  val dest_ex_unique = dest_binop "dest_ex_unique"
                                  "ex_unique"
                                  ex_unique;

  val dest_conj = dest_binop "dest_conj" "/\\" conjunction;

  val dest_disj = dest_binop "dest_disj" "\\/" disjunction;
  
  fun dest_neg fm =
    let
      val (bin, fm') = Term.dest_app fm;
    in
      if Term.eq negation bin
      then fm
      else raise Fail ("dest_neg: expected ~ found " ^
                       (Term.pprint bin))
    end;
  
  (* Predicates *)
  local
    fun is_app_match const tm =
      Term.is_app tm andalso
      (let
        val (c, _) = Term.dest_app tm
      in
        Term.eq c const
      end);
    fun is_binop bin_tm fm =
      Term.is_app fm andalso 
      (let
        val (c, _) = Term.dest_app fm;
      in
        is_app_match bin_tm c
      end)
  in
  val is_eq = is_binop equality;
  val is_imp = is_binop implication;
  val is_forall = is_binop universal;
  val is_exists = is_binop existential;
  val is_ex_unique = is_binop ex_unique;
  val is_conj = is_binop conjunction;
  val is_disj = is_binop disjunction;
  val is_neg = is_app_match negation;
  end;
end;

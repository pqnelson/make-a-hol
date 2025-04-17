(* Terms, using locally nameless variables *)

structure Term :> Term = struct
  (* BVar must be purely "internal", i.e., the user must never
     know we're using locally nameless variables as opposed
     to...anything else *)
  datatype t = FVar of string * Type.t
             | BVar of int
             | Const of string * Type.t
             | App of t * t
             | Abs of t * t;

  type Term = t;

  fun serialize (FVar (x,T)) = "FVar(" ^
                               x ^
                               ", " ^
                               (Type.serialize T) ^
                               ")"
    | serialize (BVar n) = "BVar(" ^
                           (Int.toString n) ^
                           ")"
    | serialize (Const(c,T)) = "Const(" ^
                               c ^
                               ", " ^
                               (Type.serialize T) ^
                               ")"
    | serialize (App(M,N)) = "App("^
                             (serialize M) ^
                             ", " ^
                             (serialize N) ^
                             ")"
    | serialize (Abs(x,M)) = "Abs(" ^
                             (serialize x) ^
                             ", " ^
                             (serialize M) ^
                             ")";
  
  (* Groups free variables and constants by identifier then
  type, though I don't know if this is the best way to go... *)
  fun compare(FVar(x,ty1),FVar(y,ty2)) =
    (case String.compare(x,y) of
        EQUAL => Type.compare(ty1,ty2)
      | unequal => unequal)
    | compare(FVar _, _) = LESS
    | compare(BVar _, FVar _) = GREATER
    | compare(BVar i, BVar j) = Int.compare(i,j)
    | compare(BVar _, _) = LESS
    | compare(Const _, FVar _) = GREATER
    | compare(Const _, BVar _) = GREATER
    | compare(Const(c1,ty1),Const(c2,ty2)) =
      (case String.compare(c1,c2) of
           EQUAL => Type.compare(ty1, ty2)
        | unequal => unequal)
    | compare(Const _, _) = LESS
    | compare(App(M,N), App(P,Q)) =
      (case compare(M,P) of
           EQUAL => compare(N,Q)
        | unequal => unequal)
    | compare(App _, Abs _) = LESS
    | compare(App _, _) = GREATER
    | compare(Abs(FVar(_,ty1),M), Abs(FVar(_,ty2),N)) =
      (case Type.compare(ty1,ty2) of
           EQUAL => compare(M, N)
        | unequal => unequal)
    | compare(Abs _, _) = GREATER;  

  fun eq lhs rhs = (EQUAL = compare(lhs,rhs));


  fun mk_var(name, ty) = FVar(name, ty);

  (* TODO: this should check that the constant has been
  already constructed in a table of constant definitions *)
  fun mk_const(name, ty) = Const(name, ty);

  fun mk_app(M,N) = App(M, N);

  (* shift : int -> t -> t *)
  fun shift amt M =
    if amt = 0 then M
    else case M of
             FVar _ => M
           | BVar i => BVar (i + amt)
           | Const _ => M
           | App(P,Q) => App(shift amt P, shift amt Q)
           | Abs(x,N) => Abs(x, shift (amt + 1) N);
  local
    fun iter idx x M =
      case M of
          (y as FVar _) => if x = y then (BVar idx) else y
        | (BVar _) => M
        | (Const _) => M
        | (App(P,Q)) => App(iter idx x P, iter idx x Q)
        | (Abs(y,N)) => if x = y then M
                        else Abs(y, iter (idx + 1) x N);
  in
  fun mk_abs(x as FVar _, M) = Abs(x, iter 0 x M)
    | mk_abs(x, _) = raise Fail "mk_abs: first arg must be a free variable"
  end;

  exception Dest of string;
  
  fun dest_var(FVar x) = x
    | dest_var t = raise Dest ("dest_var: expected a free "^
                               "variable, given: " ^
                               (serialize t));

  fun dest_const(Const x) = x
    | dest_const t = raise Dest ("dest_const: expected a constant"^
                                 ", given: "^
                                 (serialize t));
                                 
  fun dest_app(App x) = x
    | dest_app t = raise Dest ("dest_app: expected an app" ^
                               ", given: " ^
                               (serialize t));

  (* all_vars : t -> t list
   Produce list of all `FVar` objects appearing in argument,
   even those bound to a lambda abstraction. *)
  local
    fun iter acc M = case M of
                         FVar _ => M::acc
                       | BVar _ => acc
                       | Const _ => acc
                       | App(P,Q) => iter (iter acc P) Q
                       | Abs(x,N) => iter (x::acc) N;
  in
  fun all_vars M = iter [] M
  end;

  (* type_of : t -> Type.t *)
  local
    fun lookup 0 (ty::_) = ty
      | lookup n (_::tys) = lookup (n - 1) tys
      | lookup _ [] = raise Fail "type_of: lookup";
    fun ty_of (FVar (_, ty)) _ = ty
      | ty_of (Const (_, ty)) _ = ty
      | ty_of (BVar i) table = lookup i table
      | ty_of (App(rator, _)) table = Type.range(ty_of rator table)
      | ty_of (Abs(FVar(_,ty),M)) table = Type.mk_fun(ty, ty_of M (ty::table))
      | ty_of _ _ = raise Fail "type_of: term construction";
  in
  fun type_of tm = ty_of tm []
  end;

  fun fresh_for (v as (FVar(x,ty))) M =
    let
      val vars = all_vars M;
      fun is_fresh_for (s : string) xs =
        List.all (fn (FVar (x',_)) => s <> x' | _ => true) xs;
      fun iter s = if is_fresh_for s vars then s
                   else iter (s ^ "'");
      val x' = iter x;
    in
      if x' = x then v else FVar(x', ty)
    end
    | fresh_for t _ =
      raise Fail ("Term.fresh_for: expected FVar as first argument" ^
                  ", given: " ^
                  (serialize t));

  (* dest_abs : t -> t * t
   ENSURES: result contains *)
  local
    (* replace_with : t -> t -> t
     ASSUME: new is fresh in M *)
    fun replace_with (old as FVar _) new idx M =
     (case M of
          FVar _ => if old = M then new else M
        | BVar i => if i = idx then new else M
        | Const _ => M
        | App(P,Q) => App(replace_with old new idx P,
                          replace_with old new idx Q)
        | Abs(y,N) => Abs(y, replace_with old new (idx + 1) N))
      | replace_with t _ _ _ = raise Dest ("dest_abs: expected FVar as first argument of Abs, found: "^(serialize t));
  in
  fun dest_abs(Abs(x,M)) =
    let val x' = fresh_for x M;
    in (x', replace_with x x' 0 M)
    end
    | dest_abs t = raise Dest ("dest_abs: expected Abs, "^
                               "given: "^
                               (serialize t))
  end;

  (* type-variable instantiation *)
  fun inst [] tm = tm
    | inst sigma tm =
      (case tm of
        FVar(x,ty) => FVar(x, Type.subst sigma ty)
      | BVar _ => tm
      | Const(c,ty) => Const(c, Type.subst sigma ty)
      | App(P,Q) => App(inst sigma P,
                        inst sigma Q)
      | Abs(x,N) => Abs(inst sigma x, inst sigma N));

  fun subst [] tm = tm
    | subst (s as (old,new)::sigma) tm =
      case (old, tm) of
         (FVar(x,ty1), FVar(y,ty2)) => if Type.eq ty1 ty2
                                       then new
                                       else subst sigma tm
       | (BVar i, BVar j) => (* this shouldn't happen? *)
                             if i = j then new else tm
       | (_, Const _) => tm
       | (_, App(P,Q)) => App(subst s P, subst s Q)
       | (_, Abs(x,M)) => Abs(x, subst s M)
       | (_, M) => subst sigma M; (* case: bad subst pair, try next *)

  fun free_vars M =
    let
      fun iter acc (x as FVar _) = x::acc
        | iter acc (BVar _) = acc
        | iter acc (Const _) = acc
        | iter acc (App(P,Q)) = iter (iter acc P) Q
        | iter acc (Abs(_,N)) = iter acc N;
    in
      iter [] M
    end;

  local
    fun lookup 0 (x::_) = x
      | lookup i (x::xs) = lookup (i - 1) xs
      | lookup _ [] = raise Fail "pprint: cannot find name for bvar";
    fun iter (FVar (x, _)) _ = x
      | iter (BVar i) table = lookup i table
      | iter (Const (c,_)) _ = c
      | iter (App(M,N)) table = (iter M table) ^
                                " " ^
                                (iter N table)
      | iter (Abs(FVar(x,_), M)) table = ("(\\" ^
                                          x ^
                                          ". " ^
                                          (iter M (x::table)) ^
                                          ")")
      (* next case is impossible, but it appeases the  
         exhaustive case checker *)
      | iter (Abs(x, M)) table = ("(\\" ^
                                  (iter x table) ^
                                  ". " ^
                                  (iter M table) ^
                                  ")");
  in
  fun pprint M = (iter M []) ^
                 " :: " ^
                 (Type.pprint (type_of M))
  end;

  fun aconv lhs rhs =
    case (lhs, rhs) of
      (FVar _, FVar _) => eq lhs rhs
    | (Const _, Const _) => eq lhs rhs
    | (BVar _, BVar _) => eq lhs rhs
    | (App(M,N), App(P,Q)) => (aconv M P) andalso
                              (aconv N Q)
    | (Abs(FVar(_,ty1),M),
       Abs(FVar(_,ty2),N)) => Type.eq ty1 ty2 andalso
                              aconv M N
    | _ => false;
end;

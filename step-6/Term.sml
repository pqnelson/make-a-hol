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

  (*** mutable state, primitive constants ***)
  structure ConstTable = MkTable(String);

  val initial_table =
    let
      val a = Type.mk_var "'a";
      infixr -->;
      fun (T1 --> T2) = Type.mk_fun(T1,T2);
    in
      foldl (fn ((k,v), tbl) => ConstTable.insert tbl k v)
            ConstTable.empty
            [("=", a --> a --> Type.Bool)
            ]
    end;
  
  val const_table = ref initial_table;
  val checkpoint_table = ref initial_table;

  fun reset_table () =
    const_table := (!checkpoint_table);

  fun checkpoint_defs () =
    checkpoint_table := (!const_table);

  fun new_const(name, ty) =
    if ConstTable.member (!const_table) name
    then raise Fail ("new_const: already used: " ^
                     name)
    else (const_table := (ConstTable.insert (!const_table)
                                           name
                                           ty);
          ());

  fun mk_var(name, ty) = FVar(name, ty);

  (*
  Fails if `name` is not registered as a constant.

  Fails if `ty` is not an instance of the type for the
  constant.
  *)
  fun mk_const(name, ty) =
    case ConstTable.lookup (!const_table) name of
        NONE => raise Fail ("mk_const: unregistered constant "^
                            name)
      | SOME ty' => (case Type.match ty' ty ([], []) of
                       (tyS,_) => Const(name, ty));

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

  fun is_fvar (FVar _) = true
    | is_fvar _ = false;

  fun is_bvar (BVar _) = true
    | is_bvar _ = false;

  fun is_var x = is_fvar x orelse is_bvar x;

  fun is_const (Const _) = true
    | is_const _ = false;

  fun is_app (App _) = true
    | is_app _ = false;
  
  fun is_abs (Abs _) = true
    | is_abs _ = false;
  
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
      val vars = Lib.mergesort true compare (all_vars M);
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
      Lib.mergesort true compare (iter [] M)
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

  (*** Term matching ***)
  exception Match of string;
  
  (* This is kinda ugly, a lot of it is "inspired" (borrowed)
  from HOL4/src/0/Term.sml and hol-light/drule.ml ---- I wish
  there were a more motivated derivation, but it makes sense
  after you rework your way through it...

  I think its lineage can be traced back to LCF77 with its
  TERMMATCH function (which behaves remarkably similar).

  I AM SO SORRY; I AM SO, SO SORRY. *)
local
  (* lookup : Term.t -> Term.t list ->
                (Term.t,Term.t) Subst.t ->
                Term.t option
  
  Lookup a (free variable) term in the substitution; if it's
  there, return its replacement.

  Otherwise, lookup the term in the list of `ids`. If it's
  there, then return itself.

  Otherwise, return NONE.
   *)
  fun lookup x ids =
    let fun look [] = if List.exists (fn y => x = y) ids
                      then SOME x
                      else NONE
          | look ((old,new)::t) = if x = old
                                  then SOME new
                                  else look t
    in look end;

  (* insert : ''a -> ''a list -> ''a list

   Inserts an element into a list if it is absent. *)
  fun insert x [] = [x]
    | insert x ys = if List.exists (fn y => x = y) ys
                    then ys
                    else x::ys;

  (* free : Term.t -> int -> bool

   Tests the Term.t instance is built up out of free
  variables, constants, and bound variables whose binders are
  contained in the term. *)
  fun free (BVar i) n = i < n
    | free (App(M,N)) n = free M n andalso free N n
    | free (Abs(_,M)) n = free M (n + 1)
    | free _ _ = true;   

  (* bound_by_scoped : bool -> Term.t -> bool

  Tests if M contains any bound variables whose binders are
  **not** contained in M, provided `scoped` is true.

  The result is logically equivalent to the negation of
  "scoped implies (free M 0)". *)
  fun bound_by_scoped scoped M = scoped andalso not (free M 0);

  (* Failure to match two different constants should raise an
  exception, this is *that* exception (refactored to avoid
  distracting the complicated logic of RM from accidental
  complexity ). *)
  fun const_mismatch c1 c2 =
    raise Match ("Different constants: " ^
                 c1 ^
                 " and " ^
                 c2);
  
(* RM : (t * t * bool) list ->
          (((t,t) Subst.t * t list) *
           ((Type.t,Type.t) Subst.t * Type.t list)) ->
          ((t,t) Subst.t * (Type.t,Type.t) Subst.t)
The "raw matcher"'s inner engine.

This works its way through a list of "equations", trying to
match an "object term" (right-hand side) to a "pattern"
(left-hand side) until all equations have been satisfied or an
exception has been raised.

Basically:
- Free variables match anything EXCEPT bound variables (or
  subexpressions involving bound variables)
- Constants match constants, possibly fixing type variables.
- Bound variables only match other bound variables of
  identical de Bruijn indices.
- Applications match other applications "componentwise".
- Abstractions match abstractions, possibly fixing type
  variables of the bound variable, by matching against the
  bodies of the abstraction.
- Anything else should be an error. *)
  fun RM [] theta = theta
    | RM (((v as FVar(x,ty)),tm,scoped)::rst) ((tmS,tmVars),tyS) =
      if bound_by_scoped scoped tm
      then raise Match "Attempt to capture bound variable"
      else RM rst
           ((case lookup v tmVars tmS of
              NONE => if eq v tm
                      then (tmS, insert v tmVars)
                      else ((v,tm)::tmS,tmVars)
            | SOME tm' => if aconv tm' tm
                          then (tmS,tmVars)
                          else raise Match ("double bind on variable "^x)),
            Type.match ty (type_of tm) tyS)
    | RM ((Const(c1,ty1),Const(c2,ty2),_)::rst) (tmS,tyS) =
        RM rst
           (if c1 <> c2
            then const_mismatch c1 c2 (* raise Match... *)
            else (tmS, Type.match ty1 ty2 tyS))
    | RM ((Abs(FVar(_,ty1),M),Abs(FVar(_,ty2),N),_)::rst) (tmS,tyS) = 
        RM ((M,N,true)::rst) (tmS, Type.match ty1 ty2 tyS)
    | RM ((App(M,N),App(P,Q),s)::rst) S =
        RM ((M,P,s)::(N,Q,s)::rst) S
    | RM ((BVar i,BVar j,_)::rst) S =
        if i = j then RM rst S
        else raise Match "Bound var does not match"
    | RM all others = raise Match "different constructors";

  (* Normalize the term substitutions by removing those which
  just "fix" the type variables (which can be handled by a
  call to `Term.inst` with the given `tyS`). *)
  fun normalize_subst ((tmS,_), (tyS,_)) =
    let
      val Theta = inst tyS;
      fun del A [] = A
        | del A ((old,new)::rst) =
          del (let val old' = Theta old
               in if new = old'
                  then A
                  else (old',new)::A
              end)
              rst
    in
      (del [] tmS, tyS)
    end;
in
  (* Made public for debugging purposes... *)
  fun raw_match tyfixed tmfixed pat ob (tmS,tyS) =
    RM [(pat,ob,false)] ((tmS,tmfixed), (tyS,tyfixed));

  (* matches : t list -> Type.t list ->
                t -> t ->
                ((t,t) Subst.t * (Type.t,Type.t) Subst.t) *)
  fun matches vars tyvars pat ob =
    normalize_subst (raw_match tyvars vars pat ob ([],[]))
end;

  (* match_term : t -> t -> ((t,t) Subst.t * (Type.t,Type.t) Subst.t)

  ENSURES: (S,T) == result implies
           Term.aconv (Term.subst S (Term.inst T pat)) ob
   *)
  fun match_term pat ob = matches [] [] pat ob;

  fun is_bool tm = (Type.Bool = type_of tm);

  local
    val a = Type.mk_var("a")
  in
  val equality =
    Const("=", Type.mk_fun(a, Type.mk_fun(a, Type.Bool)));
  
  fun mk_eq (lhs,rhs) =
    let
      val T1 = type_of lhs;
      val t1 = mk_app (inst [(a,T1)] equality,
                       lhs);
    in
      if Type.eq T1 (type_of rhs)
      then mk_app (t1, rhs)
      else raise Fail ("mk_eq: lhs and rhs have different types")
    end;
  end;

  fun is_eq (App(App(Const(c, _),lhs),rhs)) = ("=" = c)
    | is_eq _ = false;

  fun dest_eq (App(App(c,lhs),rhs)) =
    ((case (Type.match (type_of equality)
                       (type_of c)
                       ([],[])) of
          (tyS,igS) => (lhs,rhs))
     handle _ => raise Fail ("dest_eq: binary operator is " ^
                             (serialize c)))
    | dest_eq tm = raise Fail ("dest_eq: given " ^
                               (serialize tm));
  
end;

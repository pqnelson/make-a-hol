structure Type :> Type = struct
  datatype Type = Var of string
                | App of string * Type list;

  type t = Type;
  
  fun list_compare compare [] [] = EQUAL
    | list_compare compare [] ys = LESS
    | list_compare compare xs [] = GREATER
    | list_compare compare (x::xs) (y::ys) =
      case compare(x,y) of
          EQUAL => list_compare compare xs ys
        | unequal => unequal;

  fun compare(Var x, Var y) = String.compare(x,y)
    | compare(Var _, _) = LESS
    | compare(App _, Var _) = GREATER
    | compare(App(c1,args1),App(c2,args2)) =
      case String.compare(c1,c2) of
          EQUAL => list_compare compare args1 args2
        | unequal => unequal;

  fun eq lhs rhs = (EQUAL = compare(lhs,rhs));

  (* serialize : Type -> string

   Return a string which resembles the code needed to make this thing. *)
  fun serialize (Var x) = "Var("^x^")"
    | serialize (App(name,args)) = "App(" ^
                                   name ^
                                   ", [" ^
                                   (String.concatWith
                                      ", "
                                      (map serialize args)) ^
                                   "]) ";

  (*** mutable state, primitive constants ***)
  structure TypeTable = MkTable(String);

  val initial_table =
    foldl (fn ((k,v), tbl) => TypeTable.insert tbl k v)
          TypeTable.empty
          [("bool", 0),
           ("ind", 0)];

  val checkpoint_table = ref initial_table;
  val type_table = ref initial_table;

  fun new_type(name,arity) =
    type_table := TypeTable.insert (!type_table) name arity;

  fun reset_table () =
    type_table := (!checkpoint_table);

  fun checkpoint_defs () =
    checkpoint_table := (!type_table);
  
  (** Constructors *)
  fun mk_type(name,args) =
    case (TypeTable.lookup (!type_table) name) of
        SOME arity => if arity = length args
                      then App(name,args)
                      else raise Fail ("mk_type: "^
                                       name ^
                                       " has arity "^
                                       (Int.toString arity) ^
                                       " received "^
                                      (Int.toString (length args)))
      | NONE => raise Fail ("mk_type: unregistered type "^ 
                            name);
  
  fun mk_var(name) = Var(name);

  (** Destructors **)
  exception Dest of string;
  fun dest_type(App x) = x
    | dest_type T = raise Dest ("dest_type: given " ^
                                (serialize T));

  fun dest_var(Var x) = x
    | dest_var T = raise Dest ("dest_var: given " ^
                               (serialize T));

  fun is_type (App _) = true
    | is_type _ = false;
  
  fun is_var (Var _) = true
    | is_var _ = false;

  val Bool = App("bool", []);
  val Ind = App("ind", []);

  fun subst [] T = T
    | subst (s as ((Var x, T')::sigma)) T =
      (case T of
           (Var y) => if x = y then T'
                      else subst sigma T
         | (App(c,args)) => App(c, map (subst s) args))
  (* oddball case which should never happen, but just to cover
     all our bases *)
    | subst (_::sigma) T = subst sigma T;

  fun mk_fun (T1, T2) = App("fun",[T1,T2]);

  fun is_fun (App("fun",[T1,T2])) = true
    | is_fun _ = false;
  
  fun range(App("fun",[_,T])) = T
    | range T = raise Dest ("range: given " ^
                            (serialize T));
  
  fun domain(App("fun",[T,_])) = T
    | domain T = raise Dest ("domain: given " ^
                             (serialize T));

  fun dest_fun(App("fun",[T1,T2])) = (T1,T2)
    | dest_fun T = raise Dest ("dest_fun: given " ^
                               (serialize T));

  fun pprint (Var x) = x
    | pprint (App("fun",[T1,T2])) = (pprint T1) ^
                                    " --> " ^
                                    (pprint T2)
    | pprint (App(name, [])) = name
    | pprint (App(name, [T])) = (pprint T) ^ " " ^ name 
    | pprint (App(name,args)) = "(" ^
                                (String.concatWith
                                   ", "
                                   (map pprint args)) ^
                                ") " ^
                                name;

  exception Match of string;

local
  fun lookup x ids =
    let fun look [] = if List.exists (fn y => x = y) ids
                      then SOME x
                      else NONE
          | look ((old,new)::rst) =
            if x = old then SOME new else look rst
    in look
    end;
  fun tymatch [] [] Sids = Sids
    | tymatch ((v as Var name)::ps) (ty::obs) (Sids as (S,ids)) = 
      tymatch ps obs
              (case lookup v ids S of
                   NONE => if v = ty
                           then (S,v::ids)
                           else ((v,ty)::S,ids)
                 | SOME ty1 => if ty1 = ty
                               then Sids
                               else raise Match ("double bind on type variable " ^ name))
    | tymatch (App(c1,A1)::ps) (App(c2,A2)::obs) Sids =
      if c1 = c2 then tymatch (A1@ps) (A2 @ obs) Sids
      else raise Match ("attempt to match different tyops: " ^
                        c1 ^
                        " against " ^
                        c2)
    | tymatch any other thing = raise Match "different constructors"
in
  fun match pat ob Sids = tymatch [pat] [ob] Sids;
end;

  fun vars_in (T as Var x) = [T]
    | vars_in (App(_,args)) = Lib.mergesort true
                                            compare
                                            (List.concat
                                               (map vars_in args)); 
end;

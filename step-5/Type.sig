signature Type = sig
  eqtype Type;

  type t = Type;

  val compare : Type * Type -> order;
  val eq : Type -> Type -> bool;

  (* Constructors for HOL Types *)
  val mk_type : string * Type list -> Type;
  val mk_var : string -> Type;

  (* Destructuring functions for HOL Types *)
  (* (Destructorators?) *)
  exception Dest of string;
  val dest_type : Type -> string * Type list;
  val dest_var : Type -> string;

  val is_type : Type -> bool;
  val is_var : Type -> bool;

  (* constants used by HOL *)
  val Bool : Type;
  val Ind : Type;

  (* Operations on types *)
  (* [subst] will substitute on type variables only *)
  val subst : (Type,Type) Subst.t -> Type -> Type;

  val pprint : Type -> string;
  val serialize : Type -> string;

  (* "arrow type", function types *)
  val mk_fun : Type * Type -> Type;
  val dest_fun : Type -> Type * Type;
  val domain : Type -> Type;
  val range : Type -> Type;
  val is_fun : Type -> bool;

  (* matching logic *)
  exception Match of string;
  
  val match : Type -> Type ->
                (Type,Type) Subst.t * Type list ->
                (Type,Type) Subst.t * Type list;
end;

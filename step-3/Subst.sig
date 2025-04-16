(* If we wanted to make this a "bona fide abstract data type",
 we should include methods like:
 - peek : ('a, 'b) t -> ('a * 'b) option
 - pop : ('a, 'b) t -> ('a * 'b) * ('a, 'b) t
 - null : ('a, 'b) t -> bool
 - merge : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
 - compose : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t

 ...among others.
*)
signature Subst = sig
  type ('a, 'b) t = ('a * 'b) list;

  (* the empty substitution *)
  val empty : ('a, 'b) t;

  (* test if the given object appears in the domain of the
     substitution *)
  val contains : (''a, 'b) t -> ''a -> bool;

  (* Extend a substitution with a new entry *)
  val add : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t;
end;

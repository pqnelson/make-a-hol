structure Subst :> Subst = struct
  type ('a, 'b) t = ('a * 'b) list;

  (* the empty substitution *)
  val empty = [];

  (* test if the given object appears in the domain of the
     substitution *)
  fun contains [] _ = false
    | contains ((redex,residue)::rst) x =
      (x = redex) orelse contains rst x;

  (* Extend a substitution with a new entry *)
  fun add S redex residue =
    (redex,residue)::S;
end;

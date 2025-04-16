signature Table = sig
  structure Key : ORD;
  type 'a t;
  type 'a Table = 'a t;

  val empty : 'a Table;
  
  (* [insert] Either insert a new key-value entry into a
     table, or if the key is already present overwrite the
     entry. *)
  val insert : 'a Table -> Key.t -> 'a -> 'a Table;
  
  (* [null] tests if a table is empty or not *)
  val null : 'a Table -> bool;

  (* [size] The number of entries in the table *)
  val size : 'a Table -> int;

  (* [member] Tests if the Table contains the key. *)
  val member : 'a Table -> Key.t -> bool;

  val lookup : 'a Table -> Key.t -> 'a option;
  
  (* val union : 'a Table -> 'a Table -> 'a Table; *)
end;

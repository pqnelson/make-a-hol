signature ORD = sig
  type t;
  val compare : t * t -> order;
  val eq : t -> t -> bool;
end;

(* patch existing primitive types to be Ord *)
structure Bool = struct
  open Bool;
  type t = bool;
  fun compare(false,false) = EQUAL
    | compare(false,_) = LESS
    | compare(true,true) = EQUAL
    | compare(true,_) = GREATER;
  fun eq x y = (x = y);
end;

structure Char = struct
  open Char;
  type t = char;
  fun eq x y = (x = y);
end;

structure Int = struct
  open Int;
  type t = int;
  fun eq x y = (x = y);
end;

structure String = struct
  open String;
  type t = string;
  fun eq x y = (x = y);
end;

structure Substring = struct
  open Substring;
  type t = substring;
  fun eq x y = (EQUAL = compare(x,y));
end;

structure Word = struct
  open Word;
  type t = word;
  fun eq x y = (x = y);
end;

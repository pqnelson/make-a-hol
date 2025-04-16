(* Red-Black tree, following Okasaki *)
functor MkTable(K : ORD) :> Table where type Key.t = K.t =
struct
  structure Key : ORD = K;

  datatype Color = Red | Black;

  datatype 'a t = E
                | N of Color * 'a t * (Key.t * 'a) * 'a t;
  
  type 'a Table = 'a t;
  
  val empty = E;

  (* insert *)
  fun R t1 a t2 = N(Red,t1,a,t2);
  fun B t1 a t2 = N(Black,t1,a,t2);
  
  fun paint _ E = E
    | paint c (tree as N(c',l,x,r)) =
      if c = c' then tree else N(c,l,x,r); 
  
  (* balanceL : 'a Tree -> 'a -> 'a Tree -> 'a Tree
     
     Perform right rotations to rebalance the tree. *)
  fun balanceL (N(Red,N(Red,t1,a,t2),b,t3)) c t4 = R (B t1 a t2)
                                                     b
                                                     (B t3 c t4)
    | balanceL (N(Red,t1,a,N(Red,t2,b,t3))) c t4 = R (B t1 a t2)
                                                     b
                                                     (B t3 c t4)
    | balanceL t1 a t2 = B t1 a t2;
  
  (* balanceR : 'a Tree -> 'a -> 'a Tree -> 'a Tree
     
     Perform left tree rotations to rebalance the right
     subtree. *)
  fun balanceR t1 a (N(Red,t2,b,N(Red,t3,c,t4))) = R (B t1 a t2)
                                                     b
                                                     (B t3 c t4)
    | balanceR t1 a (N(Red,N(Red,t2,b,t3),c,t4)) = R (B t1 a t2)
                                                     b
                                                     (B t3 c t4)
    | balanceR t1 a t2 = B t1 a t2;
  
  fun insert (tree : 'a t) (k : Key.t) (v : 'a) =
    let
      fun ins E = R E (k,v) E
        | ins (N(Black,l,x as (k_a,a),r)) =
            (case Key.compare(k,k_a) of
              LESS => balanceL (ins l) x r
            | EQUAL => B l (k,v) r
            | GREATER => balanceR l x (ins r))
        | ins (N(Red,l,x as (k_a,a),r)) =
            (case Key.compare(k,k_a) of
              LESS => R (ins l) x r
            | EQUAL => R l (k,v) r
            | GREATER => R l x (ins r));
    in
      paint Black (ins tree)
    end;

  fun null E = true
    | null _ = false;

  fun size E = 0
    | size (N(_,l,_,r)) = 1 + size l + size r;

  fun member E _ = false
    | member (N(_,l,(k,_),r)) needle =
      (case Key.compare(needle,k) of
        LESS => member l needle
      | EQUAL => true
      | GREATER => member r needle);

  fun lookup E _ = NONE
    | lookup (N(_,l,(k,v),r)) needle =
      (case Key.compare(needle,k) of
        LESS => lookup l needle
      | EQUAL => SOME v
      | GREATER => lookup r needle);

  fun union t E = t
    | union t (N(_,l,(k,v),r)) =
      union (union (insert t k v) l) r;

  end;
                                                       

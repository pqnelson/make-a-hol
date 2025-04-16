structure Lib = struct
(* mergesort : bool -> ('a * 'a -> order) -> 'a list -> 'a list 
REQUIRES: compare is a linear order
ENSURES: result is sorted wrt compare 
ENSURES: unique implies result has no duplicates *)
  fun mergesort unique compare =
    let
  (* merge : 'a list -> 'a list -> 'a list
  ENSURES: xs is sorted wrt compare andalso ys is sorted wrt compare 
           implies result is sorted wrt compare
  ENSURES: xs is subset of result
  ENSURES: ys is subset of result *)
  fun merge [] ys = ys
    | merge xs [] = xs
    | merge (xs as x :: tx) (ys as y :: ty) =
        (case compare (x, y) of
          LESS => x :: (merge tx ys)
        | EQUAL => if unique
                   then merge xs ty
                   else x :: (merge xs ty)
        | GREATER => y :: merge xs ty);
      
  (* merge_all : 'a list list -> 'a list 
  REQUIRES: argument is nonempty list
  REQUIRES: for arg in xss holds arg is sorted wrt compare
  ENSURES: result is sorted wrt compare *)
  fun merge_all [xs] = xs
    | merge_all xss = merge_all (merge_pairs xss)
  
  (* merge_pairs : 'a list list -> 'a list list
  REQUIRES: for arg in xss holds arg is sorted wrt compare
  ENSURES: unique implies hd result has no duplicates
  ENSURES: argument = xs :: ys :: lss
           implies xs is subset of hd result 
                   andalso ys is subset of hd result 
  ENSURES: for coll in result
           holds col is sorted wrt compare *)
  and merge_pairs (xs :: ys :: lss) =
          (merge xs ys) :: (merge_pairs lss)
    | merge_pairs xss = xss;

  (* runs : 'a list -> 'a list list 
  ENSURES: for coll in result holds coll is sorted wrt compare
  ENSURES: result is nonempty list *)
  fun runs (x :: y :: zs) =
      (case compare(x,y) of
        LESS => ascending y [x] zs
      | EQUAL => if unique
                 then runs (x :: zs)
                 else ascending y [x] zs
      | GREATER => descending y [x] zs)
    | runs xs = [xs]

  (* ascending : 'a -> 'a list -> 'a list -> 'a list list 
  REQUIRES: xs is sorted descending wrt compare
  ENSURES: for coll in result holds coll is sorted wrt compare
  ENSURES: result is nonempty list *)
  and ascending x xs (ys as y :: ty) =
      (case compare(x,y) of
        LESS => ascending y (x :: xs) ty
      | EQUAL => if unique
                 then ascending x xs ty
                 else ascending y (x :: xs) ty
      | GREATER => (rev (x :: xs)) :: runs ys)
    | ascending x xs [] = [rev (x :: xs)]
  (* descending : 'a -> 'a list -> 'a list -> 'a list list 
  REQUIRES: xs is sorted wrt compare
  ENSURES: for coll in result holds coll is sorted wrt compare
  ENSURES: result is nonempty list *)
  and descending x xs (ys as y :: ty) =
      (case compare(x,y) of 
        LESS => (x :: xs) :: (runs ys)
      | EQUAL => if unique
                 then descending x xs ty
                 else (x :: xs) :: (runs ys)
      | GREATER => descending y (x :: xs) ty)
    | descending x xs [] = [x :: xs];
in
    merge_all o runs
end; (* mergesort *)
end;                  

(* just run "main ()" in the repl *)
(* If you are using a "real" Standard ML implementation, you
need to comment out the following two lines of code: *)
app load ["ORD-sig", "MkTable", "Subst", "Type", "Lib", "Term"];
use "xunit.sml";

val Type_suite = suite "Type"
[
  test "var_is_var" (fn () =>
    let
      val a = Type.mk_var("a");
    in
      if Type.is_var a
      then TestSuccess
      else TestFailure "EXPECTED: 'a is var\nACTUAL: 'a is not var"
    end),
  test "app_is_not_var" (fn () =>
    let
      val ty = Type.mk_type("bool", []);
    in
      if Type.is_var ty
      then TestFailure "EXPECTED: bool is not var\nACTUAL: bool is var"
      else TestSuccess
    end),
  test "a_is_not_bool" (fn () =>
    let
      val ty = Type.mk_type("bool", []);
      val a = Type.mk_var("a");
    in
      if Type.eq ty a
      then TestFailure "EXPECTED: bool <> 'a \nACTUAL: bool = 'a"
      else TestSuccess
    end),
  test "var_eq_var" (fn () =>
    let
      val a1 = Type.mk_var("a");
      val a2 = Type.mk_var("a");
    in
      if Type.eq a1 a2
      then TestSuccess
      else TestFailure "EXPECTED: 'a = 'a \nACTUAL: 'a <> 'a"
    end),
  test "bool_eq_bool" (fn () =>
    let
      val b1 = Type.mk_type("bool", []);
      val b2 = Type.mk_type("bool", []);
    in
      if Type.eq b1 b2
      then TestSuccess
      else TestFailure "EXPECTED: bool = bool \nACTUAL: bool <> bool"
    end),
  test "subst" (fn () =>
    let
      val a = Type.mk_var("a");
      val b = Type.mk_var("b");
      val ty1 = Type.mk_type("list", [a]);
      val expected = Type.mk_type("list", [b]);
      val actual = Type.subst [(a,b)] ty1;
    in
      if Type.eq expected actual
      then TestSuccess
      else TestFailure ("EXPECTED: " ^
                        (Type.serialize expected) ^
                        "\nACTUAL: " ^
                        (Type.serialize actual))
    end),
  test "dest_var" (fn () =>
    let
      val a = Type.mk_var("a");
      val expected = "a";
      val actual = Type.dest_var a;
    in
      if expected = actual
      then TestSuccess
      else TestFailure ("EXPECTED: " ^
                        expected ^
                        "\nACTUAL: " ^
                        actual)
    end),
  test "dest_var_fail" (fn () =>
    let
      val a = Type.mk_type("bool", []);
      val expected = "a";
      val actual = (Type.dest_var a; "bool is not a var")
                   handle Type.Dest _ => "a";
    in
      if expected = actual
      then TestSuccess
      else TestFailure ("EXPECTED: " ^
                        expected ^
                        "\nACTUAL: " ^
                        actual)
    end),
  test "dest_type" (fn () =>
    let
      val a = Type.mk_var("a");
      val ty = Type.mk_type("list", [a]);
      val (expected, exp_args) = ("list", [a]);
      val (actual, act_args) = Type.dest_type ty;
      fun serialize_args args = "[" ^
                           (String.concatWith ","
                                              (map
                                                 Type.serialize
                                                 args)) ^
                           "]";
    in
      if expected = actual andalso exp_args = act_args
      then TestSuccess
      else TestFailure ("EXPECTED: " ^
                        expected ^
                        (serialize_args exp_args) ^
                        "\nACTUAL: " ^
                        actual ^
                        (serialize_args act_args))
    end),
  test "match_var" (fn () =>
    let
      val a = Type.mk_var("a");
      val ty = Type.mk_type("list", [a]);
      val expected = ([(a, ty)], []);
      val actual = Type.match a ty ([],[]);
      fun subst_to_s [] = ""
        | subst_to_s ((x,y)::xs) = (Type.pprint x) ^ " |-> " ^
                                   (Type.pprint y) ^
                                   (if List.null xs then ""
                                    else ", " ^ (subst_to_s xs));
      fun list_to_s [] = ""
        | list_to_s (x::xs) = (Type.pprint x) ^
                              (if List.null xs then ""
                               else ", " ^ (list_to_s xs));
      fun serialize (tyS, varS) = "([" ^
                                  (subst_to_s tyS) ^
                                  "], {" ^
                                  (list_to_s varS) ^
                                  "})";
    in
      if expected = actual
      then TestSuccess
      else TestFailure ("EXPECTED: " ^
                        (serialize expected) ^
                        "\nACTUAL: " ^
                        (serialize actual))
    end),
  test "match_var" (fn () =>
    let
      val a = Type.mk_var("a");
      val ty = Type.mk_type("list", [a]);
      val expected = ([(a, ty)], []);
      val actual = Type.match a ty ([],[]);
      fun subst_to_s [] = ""
        | subst_to_s ((x,y)::xs) = (Type.pprint x) ^ " |-> " ^
                                   (Type.pprint y) ^
                                   (if List.null xs then ""
                                    else ", " ^ (subst_to_s xs));
      fun list_to_s [] = ""
        | list_to_s (x::xs) = (Type.pprint x) ^
                              (if List.null xs then ""
                               else ", " ^ (list_to_s xs));
      fun serialize (tyS, varS) = "([" ^
                                  (subst_to_s tyS) ^
                                  "], {" ^
                                  (list_to_s varS) ^
                                  "})";
    in
      if expected = actual
      then TestSuccess
      else TestFailure ("EXPECTED: " ^
                        (serialize expected) ^
                        "\nACTUAL: " ^
                        (serialize actual))
    end)
];

val Term_suite = suite "Term"
[
  test "var_of_tyvar_is_var" (fn () =>
    let
      val a = Type.mk_var("a");
      val x = Term.mk_var("x", a);
    in
      if Term.is_var x
      then TestSuccess
      else TestFailure "EXPECTED: x::'a is var\nACTUAL: x::'a is not var"
    end),
  test "const_is_not_var" (fn () =>
    let
      val a = Type.mk_var("a");
      val c = Term.mk_const("c", a);
    in
      if not (Term.is_var c)
      then TestSuccess
      else TestFailure "EXPECTED: const c is not var\nACTUAL: c is var"
    end),
  test "abs_is_not_var" (fn () =>
    let
      val a = Type.mk_var("a");
      val x = Term.mk_var("x", a);
      val tm = Term.mk_abs(x, x);
    in
      if not (Term.is_var tm)
      then TestSuccess
      else TestFailure "EXPECTED: (\\x. x) is not var\nACTUAL: (\\x. x) is var"
    end),
  test "app_is_not_var" (fn () =>
    let
      val a = Type.mk_var("a");
      val x = Term.mk_var("x", a);
      val tm = Term.mk_app(Term.mk_abs(x, x), x);
    in
      if not (Term.is_var tm)
      then TestSuccess
      else TestFailure "EXPECTED: (\\x. x) x is not var\nACTUAL: (\\x. x) x is var"
    end),
  test "var_of_tyvar_is_not_const" (fn () =>
    let
      val a = Type.mk_var("a");
      val x = Term.mk_var("x", a);
    in
      if not (Term.is_const x)
      then TestSuccess
      else TestFailure "EXPECTED: x::'a is not const\nACTUAL: x::'a is const"
    end),
  test "const_is_const" (fn () =>
    let
      val a = Type.mk_var("a");
      val c = Term.mk_const("c", a);
    in
      if Term.is_const c
      then TestSuccess
      else TestFailure "EXPECTED: const c is const\nACTUAL: c is not const"
    end),
  test "abs_is_not_const" (fn () =>
    let
      val a = Type.mk_var("a");
      val x = Term.mk_var("x", a);
      val tm = Term.mk_abs(x, x);
    in
      if not (Term.is_const tm)
      then TestSuccess
      else TestFailure "EXPECTED: (\\x. x) is not const\nACTUAL: (\\x. x) is const"
    end),
  test "app_is_not_const" (fn () =>
    let
      val a = Type.mk_var("a");
      val x = Term.mk_var("x", a);
      val tm = Term.mk_app(Term.mk_abs(x, x), x);
    in
      if not (Term.is_const tm)
      then TestSuccess
      else TestFailure "EXPECTED: (\\x. x) x is not const\nACTUAL: (\\x. x) x is const"
    end),
  test "dest_var_test" (fn () =>
    let
      val a = Type.mk_var("a");
      val expected = ("x", a);
      val x = Term.mk_var expected;
      val actual = Term.dest_var x;
    in
      if expected = actual
      then TestSuccess
      else TestFailure "EXPECTED: dest_var(x :: 'a) == (x, 'a)"
    end),
  test "dest_abs_test" (fn () =>
    let
      val a = Type.mk_var("a");
      val x = Term.mk_var ("x", a);
      val expected = (x, x);
      val tm = Term.mk_abs expected;
      val actual = Term.dest_abs tm;
    in
      if expected = actual
      then TestSuccess
      else TestFailure "EXPECTED: dest_abs (\\x. x) == (x, x)"
    end),
  test "dest_app_test" (fn () =>
    let
      val a = Type.mk_var("a");
      val x = Term.mk_var ("x", a);
      val y = Term.mk_var ("y", a);
      val tm = Term.mk_abs (x, x);
      val expected = (tm, y);
      val tm = Term.mk_app expected;
      val actual = Term.dest_app tm;
    in
      if expected = actual
      then TestSuccess
      else TestFailure "EXPECTED: dest_abs ((\\x. x) y) == ((\\x. x), y)"
    end),
  test "free_vars_test1" (fn () =>
    let
      val a = Type.mk_var("a");
      val b = Type.mk_var("b");
      val c = Type.mk_var("c");
      val f = Term.mk_var ("f", Type.mk_fun(a, Type.mk_fun(b, c)));
      val x = Term.mk_var ("x", b);
      val y = Term.mk_var ("y", a);
      val tm = Term.mk_app (Term.mk_app(f, y), x);
      val expected = [f, x, y];
      val actual = Term.free_vars tm;
    in
      if expected = actual
      then TestSuccess
      else TestFailure ("EXPECTED: free_vars (f y x) == [f,x,y]" ^
                        "\nACTUAL: [" ^
                        (String.concatWith ", "
                                           (map Term.pprint
                                                actual)) ^
                        "]")
    end),
  test "free_vars_test2" (fn () =>
    let
      val a = Type.mk_var("a");
      val b = Type.mk_var("b");
      val c = Type.mk_var("c");
      val f = Term.mk_var ("f", Type.mk_fun(a, Type.mk_fun(b, c)));
      val x = Term.mk_var ("x", b);
      val y = Term.mk_var ("y", a);
      val tm = Term.mk_abs(f, Term.mk_app (Term.mk_app(f, y), x));
      val expected = [x, y];
      val actual = Term.free_vars tm;
    in
      if expected = actual
      then TestSuccess
      else TestFailure ("EXPECTED: free_vars (\\f.f y x) == [x,y]" ^
                        "\nACTUAL: [" ^
                        (String.concatWith ", "
                                           (map Term.pprint
                                                actual)) ^
                        "]")
    end),
  test "free_vars_in_const_test" (fn () =>
    let
      val a = Type.mk_var("a");
      val c = Term.mk_const("c", a);
      val expected = [];
      val actual = Term.free_vars c;
    in
      if expected = actual
      then TestSuccess
      else TestFailure ("EXPECTED: free_vars c == []" ^
                        "\nACTUAL: [" ^
                        (String.concatWith ", "
                                           (map Term.pprint
                                                actual)) ^
                        "]")
    end),
  test "aconv_fvar_test1" (fn () =>
    let
      val a = Type.mk_var("a");
      val x = Term.mk_var("x", a);
      val y = Term.mk_var("y", a);
    in
      if not (Term.aconv x y)
      then TestSuccess
      else TestFailure ("EXPECTED: not (aconv `x :: 'a` `y :: a'`)" ^
                        "\nACTUAL: aconv `x :: 'a` `y :: 'a`")
    end),
  test "aconv_fvar_test2" (fn () =>
    let
      val a = Type.mk_var("a");
      val x = Term.mk_var("x", a);
    in
      if Term.aconv x x
      then TestSuccess
      else TestFailure ("EXPECTED: aconv `x :: 'a` `x :: a'`" ^
                        "\nACTUAL: not (aconv `x :: 'a` `x :: 'a`)")
    end),
  test "aconv_fvar_test3" (fn () =>
    let
      val a = Type.mk_var("a");
      val b = Type.mk_var("b");
      val x1 = Term.mk_var("x", a);
      val x2 = Term.mk_var("x", b);
    in
      if not (Term.aconv x1 x2)
      then TestSuccess
      else TestFailure ("EXPECTED: not (aconv `x :: 'a` `x :: b'`)" ^
                        "\nACTUAL: aconv `x :: 'a` `x :: 'b`)")
    end),
  test "aconv_fvar_test4" (fn () =>
    let
      val a = Type.mk_var("a");
      val b = Type.mk_var("b");
      val x1 = Term.mk_var("x", a);
      val x2 = Term.mk_var("x", a);
    in
      if (Term.aconv x1 x2) andalso
         (Term.aconv x1 x2) = (Term.aconv x2 x1)
      then TestSuccess
      else TestFailure ("EXPECTED: aconv `x :: 'a` `x :: a'`" ^
                        "\nACTUAL: not (aconv `x :: 'a` `x :: 'a`)")
    end),
  test "aconv_const_test1" (fn () =>
    let
      val a = Type.mk_var("a");
      val b = Type.mk_var("b");
      val c1 = Term.mk_const("c", a);
      val c2 = Term.mk_const("c", b);
    in
      if not (Term.aconv c1 c2)
      then TestSuccess
      else TestFailure ("EXPECTED: not (aconv `c :: 'a` `c :: b'`)" ^
                        "\nACTUAL: aconv `c :: 'a` `c :: 'b`")
    end),
  test "aconv_const_test2" (fn () =>
    let
      val a = Type.mk_var("a");
      val b = Type.mk_var("b");
      val c1 = Term.mk_const("c", a);
      val c2 = Term.mk_const("c", a);
    in
      if Term.aconv c1 c2
      then TestSuccess
      else TestFailure ("EXPECTED: aconv `c :: 'a` `c :: b'`" ^
                        "\nACTUAL: not (aconv `c :: 'a` `c :: 'b`)")
    end),
  test "aconv_const_test3" (fn () =>
    let
      val a = Type.mk_var("a");
      val c1 = Term.mk_const("c1", a);
      val c2 = Term.mk_const("c2", a);
    in
      if not (Term.aconv c1 c2)
      then TestSuccess
      else TestFailure ("EXPECTED: not (aconv `c1 :: 'a` `c2 :: a'`)" ^
                        "\nACTUAL: aconv `c1 :: 'a` `c2 :: 'a`")
    end),
  test "aconv_const_test4" (fn () =>
    let
      val a = Type.mk_var("a");
      val b = Type.mk_var("b");
      val c1 = Term.mk_const("c1", a);
      val c2 = Term.mk_const("c2", b);
    in
      if not (Term.aconv c1 c2)
      then TestSuccess
      else TestFailure ("EXPECTED: not (aconv `c1 :: 'a` `c2 :: b'`)" ^
                        "\nACTUAL: aconv `c1 :: 'a` `c2 :: 'b`")
    end),
  test "typeof_var_test" (fn () =>
    let
      val a = Type.mk_var("a");
      val x = Term.mk_var("x", a);
      val expected = a;
      val actual = Term.type_of x;
    in
      if Type.eq actual expected
      then TestSuccess
      else TestFailure ("EXPECTED: type_of (x :: a) = a" ^
                        "\nACTUAL: " ^ (Type.pprint actual))
    end),
  test "typeof_const_test" (fn () =>
    let
      val Bool = Type.mk_type("bool", []);
      val T = Term.mk_const("T", Bool);
      val expected = Bool;
      val actual = Term.type_of T;
    in
      if Type.eq actual expected
      then TestSuccess
      else TestFailure ("EXPECTED: type_of (T :: :bool) = :bool" ^
                        "\nACTUAL: " ^ (Type.pprint actual))
    end),
  test "typeof_eq_test" (fn () =>
    let
      val Bool = Type.mk_type("bool", []);
      val a = Type.mk_var("a");
      val ty = Type.mk_fun(a, Type.mk_fun(a, Bool));
      val eq = Term.mk_const("=", ty);
      val expected = ty;
      val actual = Term.type_of eq;
    in
      if Type.eq actual expected
      then TestSuccess
      else TestFailure ("EXPECTED: type_of (= :: 'a --> 'a --> :bool) = 'a --> 'a --> :bool" ^
                        "\nACTUAL: " ^ (Type.pprint actual))
    end),
  test "typeof_abs_test" (fn () =>
    let
      val Bool = Type.mk_type("bool", []);
      val a = Type.mk_var("a");
      val ty = Type.mk_fun(a, Type.mk_fun(a, Bool));
      val eq = Term.mk_const("=", ty);
      val T = Term.mk_const("T", Bool);
      val x = Term.mk_var("x", Bool);
      val tm = Term.mk_abs(x, Term.mk_app(Term.mk_app(eq,x),T));
      val expected = Type.mk_fun(Bool, Bool);
      val actual = Term.type_of tm;
    in
      if Type.eq actual expected
      then TestSuccess
      else TestFailure ("EXPECTED: type_of (\\x. x = T :: :bool --> :bool) = :bool --> :bool" ^
                        "\nACTUAL: " ^ (Type.pprint actual))
    end),
  test "typeof_app_test" (fn () =>
    let
      val Bool = Type.mk_type("bool", []);
      val a = Type.mk_var("a");
      val ty = Type.mk_fun(a, Type.mk_fun(a, Bool));
      val eq = Term.mk_const("=", ty);
      val x = Term.mk_var("x", a);
      val y = Term.mk_var("y", a);
      val tm = Term.mk_app(Term.mk_app(eq,x),y);
      val expected = Bool;
      val actual = Term.type_of tm;
    in
      if Type.eq actual expected
      then TestSuccess
      else TestFailure ("EXPECTED: type_of (x = y) = :bool" ^
                        "\nACTUAL: " ^ (Type.pprint actual))
    end),
  test "typeof_app_test" (fn () =>
    let
      val Bool = Type.mk_type("bool", []);
      val a = Type.mk_var("a");
      val ty = Type.mk_fun(a, Type.mk_fun(a, Bool));
      val eq = Term.mk_const("=", ty);
      val x = Term.mk_var("x", a);
      val y = Term.mk_var("y", a);
      val tm = Term.mk_app(Term.mk_app(eq,x),y);
      val expected = Bool;
      val actual = Term.type_of tm;
    in
      if Type.eq actual expected
      then TestSuccess
      else TestFailure ("EXPECTED: type_of (x = y) = :bool" ^
                        "\nACTUAL: " ^ (Type.pprint actual))
    end),
  test "match_term_test1" (fn () =>
    let
      val Bool = Type.mk_type("bool", []);
      val a = Type.mk_var("a");
      val b = Type.mk_var("b");

      val pat = Term.mk_var("x", a);
      val lhs = Term.mk_var("x", Bool);

      val ty = Type.mk_fun(b, Type.mk_fun(b, Bool));
      val eq = Term.mk_const("=", ty);
      val x = Term.mk_var("x", b);
      val y = Term.mk_var("y", b);
      val ob = Term.mk_app(Term.mk_app(eq,x),y);
      val expected = ([(lhs,ob)], [(a,Bool)]);
      val actual = Term.match_term pat ob;
      fun ty_subst_to_s [] = ""
        | ty_subst_to_s ((x,y)::xs) = (Type.pprint x) ^ " |-> " ^
                                   (Type.pprint y) ^
                                   (if List.null xs then ""
                                    else ", " ^ (ty_subst_to_s xs));
      fun tm_subst_to_s [] = ""
        | tm_subst_to_s ((x,y)::xs) = (Term.pprint x) ^ " |-> " ^
                                   (Term.pprint y) ^
                                   (if List.null xs then ""
                                    else ", " ^ (tm_subst_to_s xs));
      fun serialize (tmS, tyS) = "([" ^
                                  (tm_subst_to_s tmS) ^
                                  "], [" ^
                                  (ty_subst_to_s tyS) ^
                                  "])";
    in
      if actual = expected
      then TestSuccess
      else TestFailure ("EXPECTED: " ^ (serialize expected) ^
                        "\nACTUAL: " ^ (serialize actual))
    end),
  test "match_term_test2" (fn () =>
    let
      val Bool = Type.mk_type("bool", []);
      val a = Type.mk_var("a");
      val b = Type.mk_var("b");

      val pat = Term.mk_var("x", a);
      val lhs = Term.mk_var("x", Bool);

      val ty = Type.mk_fun(b, Type.mk_fun(b, Bool));
      val eq = Term.mk_const("=", ty);
      val x = Term.mk_var("x", b);
      val y = Term.mk_var("y", b);
      val ob = Term.mk_app(Term.mk_app(eq,x),y);
      val expected = ([(lhs,ob)], [(a,Bool)]);
      val actual = Term.match_term pat ob;
      fun ty_subst_to_s [] = ""
        | ty_subst_to_s ((x,y)::xs) = (Type.pprint x) ^ " |-> " ^
                                   (Type.pprint y) ^
                                   (if List.null xs then ""
                                    else ", " ^ (ty_subst_to_s xs));
      fun tm_subst_to_s [] = ""
        | tm_subst_to_s ((x,y)::xs) = (Term.pprint x) ^ " |-> " ^
                                   (Term.pprint y) ^
                                   (if List.null xs then ""
                                    else ", " ^ (tm_subst_to_s xs));
      fun serialize (tmS, tyS) = "([" ^
                                  (tm_subst_to_s tmS) ^
                                  "], [" ^
                                  (ty_subst_to_s tyS) ^
                                  "])";
      val (S, T) = actual;
    in
      if Term.aconv (Term.subst S (Term.inst T pat)) ob
      then TestSuccess
      else TestFailure ("EXPECTED: aconv (subst " ^
                        (tm_subst_to_s S) ^
                        " (inst " ^
                        (ty_subst_to_s T) ^
                        " "
                        ^ (Term.pprint pat) ^")) " ^
                        (Term.pprint ob) ^
                        "\nACTUAL: false")
    end),
  test "match_term_test3" (fn () =>
    (* example from HOL4 manula 
https://hol-theorem-prover.org/kananaskis-11-helpdocs/help/Docfiles/HTML/Term.match_term.html
     *)
    let
      val Bool = Type.mk_type("bool", []);
      val b = Type.mk_var("b");
      val ty = Type.mk_fun(b, Type.mk_fun(b, Bool));
      val eq = Term.mk_const("=", ty);
      val x = Term.mk_var("x", b);
      val pat = Term.mk_app(Term.mk_app(eq,x),x);

      val num = Type.mk_type(":num", []);
      val one = Term.mk_const("1", num);
      val ty1 = Type.mk_fun(num, Type.mk_fun(num, Bool));
      val eq1 = Term.mk_const("=", ty1);
      val ob = Term.mk_app(Term.mk_app(eq1,one),one);
      
      val expected = ([(x,one)], [(b,num)]);
      val actual = Term.match_term pat ob;
      fun ty_subst_to_s [] = ""
        | ty_subst_to_s ((x,y)::xs) = (Type.pprint x) ^ " |-> " ^
                                   (Type.pprint y) ^
                                   (if List.null xs then ""
                                    else ", " ^ (ty_subst_to_s xs));
      fun tm_subst_to_s [] = ""
        | tm_subst_to_s ((x,y)::xs) = (Term.pprint x) ^ " |-> " ^
                                   (Term.pprint y) ^
                                   (if List.null xs then ""
                                    else ", " ^ (tm_subst_to_s xs));
      fun serialize (tmS, tyS) = "([" ^
                                  (tm_subst_to_s tmS) ^
                                  "], [" ^
                                  (ty_subst_to_s tyS) ^
                                  "])";
      val (S, T) = actual;
    in
      if Term.aconv (Term.subst S (Term.inst T pat)) ob
      then TestSuccess
      else TestFailure ("EXPECTED: aconv (subst " ^
                        (tm_subst_to_s S) ^
                        " (inst " ^
                        (ty_subst_to_s T) ^
                        " "
                        ^ (Term.pprint pat) ^")) " ^
                        (Term.pprint ob) ^
                        "\nACTUAL: false")
    end)
]

val all_tests = suite "test" [Type_suite, Term_suite];

fun main () =
  run all_tests;

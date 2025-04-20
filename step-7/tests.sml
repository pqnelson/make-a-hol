(* just run "main ()" in the repl *)
(* If you are using a "real" Standard ML implementation, you
need to comment out the following two lines of code: *)
app load ["ORD-sig", "MkTable", "Subst", "Type", "Lib",
          "Term", "Kernel", "equal"];
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
      (* setup *)
      val _ = Type.reset_table ();
      val _ = Type.new_type("list",1);
      (* execute *)
      val a = Type.mk_var("a");
      val b = Type.mk_var("b");
      val ty1 = Type.mk_type("list", [a]);
      val expected = Type.mk_type("list", [b]);
      val actual = Type.subst [(a,b)] ty1;
      (* cleanup *)
      val _ = Type.reset_table ();
      val _ = Type.new_type("list",1);
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
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val a = Type.mk_var("a");
      val _ = Term.new_const("c", a);
      val c = Term.mk_const("c", a);
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
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
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val a = Type.mk_var("a");
      val _ = Term.new_const("c", a);
      val c = Term.mk_const("c", a);
      (* execute *)
      val result = Term.is_const c;
      (* cleanupup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
    in
      if result
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
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val a = Type.mk_var("a");
      val b = Type.mk_var("b");
      val c = Type.mk_var("c");
      val f = Term.mk_var ("f", Type.mk_fun(a, Type.mk_fun(b, c)));
      val x = Term.mk_var ("x", b);
      val y = Term.mk_var ("y", a);
      val tm = Term.mk_app (Term.mk_app(f, y), x);
      (* execute *)
      val expected = [f, x, y];
      val actual = Term.free_vars tm;
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
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
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val a = Type.mk_var("a");
      val _ = Term.new_const("c", a);
      val c = Term.mk_const("c", a);
      (* execute *)
      val expected = [];
      val actual = Term.free_vars c;
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
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
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val a = Type.mk_var("a");
      val b = Type.mk_var("b");
      val _ = Term.new_const("c", a);
      (* execute *)
      val c1 = Term.mk_const("c", a);
      val c2 = Term.mk_const("c", b);
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
    in
      if not (Term.aconv c1 c2)
      then TestSuccess
      else TestFailure ("EXPECTED: not (aconv `c :: 'a` `c :: b'`)" ^
                        "\nACTUAL: aconv `c :: 'a` `c :: 'b`")
    end),
  test "aconv_const_test2" (fn () =>
    let
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val a = Type.mk_var("a");
      val b = Type.mk_var("b");
      val _ = Term.new_const("c", a);
      (* execute *)
      val c1 = Term.mk_const("c", a);
      val c2 = Term.mk_const("c", a);
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
    in
      if Term.aconv c1 c2
      then TestSuccess
      else TestFailure ("EXPECTED: aconv `c :: 'a` `c :: b'`" ^
                        "\nACTUAL: not (aconv `c :: 'a` `c :: 'b`)")
    end),
  test "aconv_const_test3" (fn () =>
    let
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val a = Type.mk_var("a");
      val _ = Term.new_const("c1", a);
      val _ = Term.new_const("c2", a);
      (* execute *)
      val c1 = Term.mk_const("c1", a);
      val c2 = Term.mk_const("c2", a);
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
    in
      if not (Term.aconv c1 c2)
      then TestSuccess
      else TestFailure ("EXPECTED: not (aconv `c1 :: 'a` `c2 :: a'`)" ^
                        "\nACTUAL: aconv `c1 :: 'a` `c2 :: 'a`")
    end),
  test "aconv_const_test4" (fn () =>
    let
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val a = Type.mk_var("a");
      val b = Type.mk_var("b");
      val _ = Term.new_const("c1", a);
      val _ = Term.new_const("c2", b);
      (* execute *)
      val c1 = Term.mk_const("c1", a);
      val c2 = Term.mk_const("c2", b);
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
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
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val Bool = Type.Bool;
      val _ = Term.new_const("T", Bool);
      val T = Term.mk_const("T", Bool);
      (* execute *)
      val expected = Bool;
      val actual = Term.type_of T;
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
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
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val Bool = Type.Bool;
      val a = Type.mk_var("a");
      val ty = Type.mk_fun(a, Type.mk_fun(a, Bool));
      val _ = Term.new_const("T", Bool);
      val eq = Term.mk_const("=", ty);
      val T = Term.mk_const("T", Bool);
      val x = Term.mk_var("x", Bool);
      val tm = Term.mk_abs(x, Term.mk_eq(x,T));
      (* execute *)
      val expected = Type.mk_fun(Bool, Bool);
      val actual = Term.type_of tm;
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
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
    (* example from HOL4 manual 
https://hol-theorem-prover.org/kananaskis-11-helpdocs/help/Docfiles/HTML/Term.match_term.html
     *)
    let
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val _ = Type.new_type(":num",0);
      val Bool = Type.Bool;
      val b = Type.mk_var("b");
      val ty = Type.mk_fun(b, Type.mk_fun(b, Bool));
      val eq = Term.mk_const("=", ty);
      val x = Term.mk_var("x", b);
      val pat = Term.mk_eq(x,x);

      val num = Type.mk_type(":num", []);
      val _ = Term.new_const("1",num);
      val one = Term.mk_const("1", num);
      val ty1 = Type.mk_fun(num, Type.mk_fun(num, Bool));
      val ob = Term.mk_eq(one,one);
      
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
      (* execute *)
      val result = Term.aconv (Term.subst S (Term.inst T pat)) ob;
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
    in
      if result
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
  test "eq for test.Kernel.deductAntisym_test1" (fn () =>
    (* example from HOL4 manual 
https://hol-theorem-prover.org/kananaskis-11-helpdocs/help/Docfiles/HTML/Term.match_term.html
     *)
    let
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val _ = Type.new_type(":num", 0);
      val num = Type.mk_type(":num", []);
      val x = Term.mk_var("x", num);
      val y = Term.mk_var("y", num);
      val lhs = Term.mk_eq(x,x);
      val rhs = Term.mk_eq(y,y);
      (* execute *)
      val expected = Term.eq lhs rhs;
      val actual = false;
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
    in
      if (expected = actual)
      then TestSuccess
      else TestFailure ("EXPECTED: not (Term.eq " ^
                        (Term.serialize lhs) ^ " " ^
                        (Term.serialize rhs) ^ ")" ^
                        "\nACTUAL: false")
    end)
]

val Kernel_suite = suite "Kernel"
[
  test "refl_test1" (fn () =>
    (* example from HOL Light manual 
https://www.cl.cam.ac.uk/~jrh13/hol-light/HTML/REFL.html
     *)
    let
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val _ = Type.new_type(":num", 0);
      val num = Type.mk_type(":num",[]);
      val _ = Term.new_const("2", num);
      val t2 = Term.mk_var("2", num);
      (* execute *)
      val expected = Term.mk_eq(t2,t2);
      val actual = Kernel.concl(Kernel.refl t2);
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
    in
      if Term.eq expected actual
      then TestSuccess
      else TestFailure ("EXPECTED: " ^
                        (Term.serialize expected) ^
                        "\nACTUAL: " ^
                        (Term.serialize actual) ^
                        "\n")
    end),
  test "refl_test2" (fn () =>
    (* example from HOL Light manual 
https://www.cl.cam.ac.uk/~jrh13/hol-light/HTML/REFL.html
     *)
    let
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val p = Term.mk_var("p", Type.Bool);
      (* execute *)
      val expected = Term.mk_eq(p,p);
      val actual = Kernel.concl(Kernel.refl p);
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
    in
      if Term.eq expected actual
      then TestSuccess
      else TestFailure ("EXPECTED: " ^
                        (Term.serialize expected) ^
                        "\nACTUAL: " ^
                        (Term.serialize actual) ^
                        "\n")
    end),
  test "assume_test" (fn () =>
    (* example from HOL Light manual 
https://www.cl.cam.ac.uk/~jrh13/hol-light/HTML/ASSUME.html
     *)
    let
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val _ = Term.new_const("/\\", Type.mk_fun(Type.Bool,
                                                   Type.mk_fun(Type.Bool, Type.Bool)));
      val conj = Term.mk_const("/\\", Type.mk_fun(Type.Bool,
                                                   Type.mk_fun(Type.Bool, Type.Bool)));
      val p = Term.mk_var("p", Type.Bool);
      val q = Term.mk_var("q", Type.Bool);
      val tm = Term.mk_app(Term.mk_app(conj,p),q);
      fun serialize (xs,y) = ((String.concatWith ", " (map Term.serialize xs)) ^
                              " |- " ^
                              (Term.serialize y));
      (* execute *)
      val expected = ([tm],tm);
      val actual = Kernel.dest_thm(Kernel.assume tm);
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
    in
      if expected = actual
      then TestSuccess
      else TestFailure ("EXPECTED: " ^
                        (serialize expected) ^
                        "\nACTUAL: " ^
                        (serialize actual) ^
                        "\n")
    end),
  test "eq_mp_test1" (fn () =>
    (* example from HOL Light manual 
https://www.cl.cam.ac.uk/~jrh13/hol-light/HTML/EQ_MP.html
     *)
    let
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val _ = Term.new_const("/\\", Type.mk_fun(Type.Bool,
                                                   Type.mk_fun(Type.Bool, Type.Bool)));
      val conj = Term.mk_const("/\\", Type.mk_fun(Type.Bool,
                                                   Type.mk_fun(Type.Bool, Type.Bool)));
      val p1 = Term.mk_var("p1", Type.Bool);
      val p2 = Term.mk_var("p2", Type.Bool);
      val q = Term.mk_var("q", Type.Bool);
      (* \p. p /\ q *)
      val tm1 = Term.mk_abs(p1,Term.mk_app(Term.mk_app(conj,p1),q));
      val tm2 = Term.mk_abs(p2,Term.mk_app(Term.mk_app(conj,p2),q));
      fun serialize (xs,y) = ((String.concatWith ", " (map Term.serialize xs)) ^
                              " |- " ^
                              (Term.serialize y));
      (* execute *)
      val th0 = Kernel.refl tm1; (*  |- (\p. p /\ q) = (\p. p /\ q) *)
      (* |- ((\p. p /\ q) = (\p. p /\ q)) = ((\p. p /\ q) = (\p. p /\ q)) *)
      val th1 = Kernel.refl(Kernel.concl th0);
      (*  |- (\p. p /\ q) = (\p. p /\ q) *)
      val th2 = Kernel.refl(tm2);
      val expected = ([],
                      Term.mk_eq(tm2,tm2));
      val actual = Kernel.dest_thm th2;
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
    in
      if expected = actual
      then TestSuccess
      else TestFailure ("EXPECTED: " ^
                        (serialize expected) ^
                        "\nACTUAL: " ^
                        (serialize actual) ^
                        "\n")
    end),
  test "absThm_test1" (fn () =>
    (* example from HOL Light manual 
https://www.cl.cam.ac.uk/~jrh13/hol-light/HTML/ABS.html
     *)
    let
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val _ = Type.new_type(":num", 0);
      val num = Type.mk_type(":num", []);
      val m = Term.mk_var("m", num);
      fun serialize (xs,y) = ((String.concatWith ", " (map Term.serialize xs)) ^
                              " |- " ^
                              (Term.serialize y));
      (* execute *)
      val th = Kernel.absThm m (Kernel.refl m);
      val id_m = Term.mk_abs(m,m);
      val tm = Term.mk_eq(id_m,id_m);
      val expected = ([], tm);
      val actual = Kernel.dest_thm th;
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
    in
      if expected = actual
      then TestSuccess
      else TestFailure ("EXPECTED: " ^
                        (serialize expected) ^
                        "\nACTUAL: " ^
                        (serialize actual) ^
                        "\n")
    end),
  test "appThm_test1" (fn () =>
    (* example from HOL Light manual 
https://www.cl.cam.ac.uk/~jrh13/hol-light/HTML/MK_COMB_UPPERCASE.html
     *)
    let
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      (* T = ((\p:bool. p) = (\p:bool. p)) *)
      val T_DEF =
        let
          val p = Term.mk_var("p", Type.Bool);
          val t = Term.mk_abs(p,p);
          val definiendum = Term.mk_eq(t,t);
        in
          Kernel.defineConst "true" definiendum
        end;
      (* (!) = \P:A->bool. P = \x. true *)
      val FORALL_DEF =
        let
          val a = Type.mk_var("a");
          val P = Term.mk_var("P", Type.mk_fun(a, Type.Bool));
          val T = Term.mk_const("true",Type.Bool);
          val x = Term.mk_var("x", a);
          val body = Term.mk_eq(P, Term.mk_abs(x,T));
          val definiendum = Term.mk_abs(P, body);
        in
          Kernel.defineConst "!" definiendum
        end;
      val forall =
        let
          val a = Type.mk_var("a");
          val ty = Type.mk_fun(Type.mk_fun(a, Type.Bool),
                               Type.Bool);
        in
          Term.mk_const("!", ty)
        end;
      fun mk_forall(bv,body) =
        let
          val a = Type.mk_var("a");
          val q = Term.inst [(a, Term.type_of bv)] forall;
        in
          Term.mk_app(q,Term.mk_abs(bv,body))
        end;

      (* F = !p:bool. p *)
      val F_DEF =
        let
          val p = Term.mk_var("p", Type.Bool);
        in
          Kernel.defineConst "F" (mk_forall(p,p))
        end;
      val F = Term.mk_const("F", Type.Bool);
      val connective_ty = Type.mk_fun(Type.Bool,
                                      Type.mk_fun(Type.Bool,
                                                  Type.Bool));
      (* (/\) = (\p q. (\f. f p q) = (\f. f true true)) *)
      val AND_DEF =
        let
          val p = Term.mk_var("p",Type.Bool);
          val q = Term.mk_var("q",Type.Bool);
          val T = Term.mk_const("true",Type.Bool);
          val f = Term.mk_var("f",connective_ty);
          val lhs = Term.mk_abs(f,
                                Term.mk_app(Term.mk_app(f,p),q));
          val rhs = Term.mk_abs(f,
                                Term.mk_app(Term.mk_app(f,T),T));
          val body = Term.mk_eq(lhs,rhs);
          val definiendum = Term.mk_abs(p,Term.mk_abs(q,body));
        in
          Kernel.defineConst "/\\" definiendum
        end;
      val conj = Term.mk_const("/\\", connective_ty);
      fun mk_conj(p,q) =
        Term.mk_app(Term.mk_app(conj, p), q);
      val IMP_DEF = (* (==>) := \p q. p /\ q = p *)
        let
          val p = Term.mk_var("p",Type.Bool);
          val q = Term.mk_var("q",Type.Bool);
          val lhs = mk_conj(p,q);
          val body = Term.mk_eq(lhs,p);
          val definiendum = Term.mk_abs(p, Term.mk_abs(q, body));
        in
          Kernel.defineConst "==>" definiendum
        end;
      val imp = Term.mk_const("==>", connective_ty);
      fun mk_imp (p,q) =
        Term.mk_app(Term.mk_app(imp,p),q);
      (* (~) = \p. p ==> F *)
      val NOT_DEF : Kernel.thm =
        let
          val p = Term.mk_var("p", Type.Bool);
          val body = mk_imp(p, F);
          val definiendum = Term.mk_abs(p, body);
        in
          Kernel.defineConst "~" definiendum
        end;
      fun mk_not tm = Term.mk_app(Term.mk_const("~",Type.mk_fun(Type.Bool,Type.Bool)),
                                  tm);
      val p = Term.mk_var("p", Type.Bool);
      fun serialize (xs,y) = ((String.concatWith ", " (map Term.serialize xs)) ^
                              " |- " ^
                              (Term.serialize y));
      (* execute *)
      val th1 = NOT_DEF;
      val th2 = Kernel.refl p;
      val th = Kernel.appThm th1 th2;
      val tm = Term.mk_eq(mk_not(p),
                          Term.mk_app(Term.mk_abs(p,mk_imp(p,F)),p));
      val expected = ([], tm);
      val actual = Kernel.dest_thm th;
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
    in
      if expected = actual
      then TestSuccess
      else TestFailure ("EXPECTED: " ^
                        (serialize expected) ^
                        "\nACTUAL: " ^
                        (serialize actual) ^
                        "\n")
    end),
  test "deductAntisym_test1" (fn () =>
    (* example from HOL Light manual 
https://www.cl.cam.ac.uk/~jrh13/hol-light/HTML/DEDUCT_ANTISYM_RULE.html
     *)
    let
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val _ = Type.new_type(":num", 0);
      val num = Type.mk_type(":num", []);
      val x = Term.mk_var("x", num);
      val y = Term.mk_var("y", num);
      fun serialize (xs,y) = ((String.concatWith ", " (map Term.pprint xs)) ^
                              " |- " ^
                              (Term.pprint y));
      (* execute *)
      val x_eq_x = Term.mk_eq(x,x);
      val y_eq_y = Term.mk_eq(y,y);
      val th1 = Kernel.assume x_eq_x;
      val th2 = Kernel.assume y_eq_y;
      val th = Kernel.deductAntisym th1 th2;
      val expected = ([x_eq_x,y_eq_y], Term.mk_eq(x_eq_x, y_eq_y));
      val actual = Kernel.dest_thm th;
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
    in
      if expected = actual
      then TestSuccess
      else TestFailure ("EXPECTED: " ^
                        (serialize expected) ^
                        "\nACTUAL: " ^
                        (serialize actual) ^
                        "\n")
    end),
  test "betaConv_test1" (fn () =>
    (* example from HOL Light manual 
https://www.cl.cam.ac.uk/~jrh13/hol-light/HTML/BETA_CONV.html
     *)
    let
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val _ = Type.new_type(":num", 0);
      val num = Type.mk_type(":num", []);
      val _ = Term.new_const("1",num);
      val _ = Term.new_const("+", Type.mk_fun(num, Type.mk_fun(num,num)));
      val plus = Term.mk_const("+", Type.mk_fun(num, Type.mk_fun(num,num)));
      val one = Term.mk_const("1", num);
      val x = Term.mk_var("x", num);
      val y = Term.mk_var("y", num);
      fun serialize (xs,y) = ((String.concatWith ", " (map Term.pprint xs)) ^
                              " |- " ^
                              (Term.pprint y));
      (* execute *)
      val x_plus_1 = Term.mk_app(Term.mk_app(plus,x),one);
      val y_plus_1 = Term.mk_app(Term.mk_app(plus,y),one);
      val lhs_abs = Term.mk_abs(x,x_plus_1);
      val lhs = Term.mk_app(lhs_abs,y);
      val rhs = y_plus_1;
      val th1 = Kernel.betaConv lhs_abs y;
      val expected = ([], Term.mk_eq(lhs,rhs));
      val actual = Kernel.dest_thm th1;
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
    in
      if expected = actual
      then TestSuccess
      else TestFailure ("EXPECTED: " ^
                        (serialize expected) ^
                        "\nACTUAL: " ^
                        (serialize actual) ^
                        "\n")
    end)
];

val equal_suite = suite "equal"
[
  test "apTerm_test1" (fn () =>
    (* example from HOL Light manual 
https://www.cl.cam.ac.uk/~jrh13/hol-light/HTML/BETA_CONV.html
     *)
    let
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
      val _ = Type.new_type(":num", 0);
      val num = Type.mk_type(":num", []);
      val _ = Term.new_const("1",num);
      val _ = Term.new_const("+", Type.mk_fun(num, Type.mk_fun(num,num)));
      val plus = Term.mk_const("+", Type.mk_fun(num, Type.mk_fun(num,num)));
      val one = Term.mk_const("1", num);
      val x = Term.mk_var("x", num);
      val y = Term.mk_var("y", num);
      val inc = Term.mk_abs(x, Term.mk_app(Term.mk_app(plus, x), one));
      fun serialize (xs,y) = ((String.concatWith ", " (map Term.pprint xs)) ^
                              " |- " ^
                              (Term.pprint y));
      (* execute *)
      val y_plus_1 = Term.mk_app(Term.mk_app(plus,y),one);
      val th1 = Kernel.refl(y_plus_1);
      val th2 = equal.apTerm inc th1;
      val lhs = Term.mk_app(inc,y_plus_1);
      val rhs = lhs;
      val expected = ([], Term.mk_eq(lhs,rhs));
      val actual = Kernel.dest_thm th2;
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
    in
      if expected = actual
      then TestSuccess
      else TestFailure ("EXPECTED: " ^
                        (serialize expected) ^
                        "\nACTUAL: " ^
                        (serialize actual) ^
                        "\n")
    end),
  test "trans_test1" (fn () =>
    (* example from HOL Light manual 
https://www.cl.cam.ac.uk/~jrh13/hol-light/HTML/TRANS.html
     *)
    let
      (* set up *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();

      val a = Term.mk_var("a", Type.Bool);
      val b = Term.mk_var("b", Type.Bool);
      val c = Term.mk_var("c", Type.Bool);

      fun serialize (xs,y) = ((String.concatWith ", " (map Term.pprint xs)) ^
                              " |- " ^
                              (Term.pprint y));
      (* execute *)
      val a_eq_b = Term.mk_eq(a,b);
      val b_eq_c = Term.mk_eq(b,c);
      val a_eq_c = Term.mk_eq(a,c);
      val th1 = Kernel.assume (a_eq_b);
      val th2 = Kernel.assume (b_eq_c);

      val th = equal.trans th1 th2;
      val expected = ([a_eq_b, b_eq_c], a_eq_c);
      val actual = Kernel.dest_thm th;
      (* cleanup *)
      val _ = Term.reset_table ();
      val _ = Type.reset_table ();
    in
      if expected = actual
      then TestSuccess
      else TestFailure ("EXPECTED: " ^
                        (serialize expected) ^
                        "\nACTUAL: " ^
                        (serialize actual) ^
                        "\n")
    end)
];

val all_tests = suite "test" [ Type_suite
                             , Term_suite
                             , Kernel_suite
                             , equal_suite
                             ];

fun main () =
  run all_tests;

(*
App(App(Const(=, App(fun, [App(fun, [App(bool, []) , App(bool,
                                                         [])
                              ]) , App(fun, [App(fun,
                                                 [App(bool,
                                                      []) ,
                                                  App(bool,
                                                      []) ]) ,
                                             App(bool, []) ])
             ]) ),
        Abs(FVar(p2, App(bool, []) ),
            App(App(Const(/\, App(fun, [App(bool, []) , App(fun,
                                                         [App(bool,
                                                         []) ,
                                                         App(bool,
                                                         [])
                                                         ]) ])
                         ),
                    BVar(0)),
                FVar(q, App(bool, []) )))),
    Abs(FVar(p2, App(bool, []) ),
        App(App(Const(/\, App(fun, [App(bool, []) , App(fun, [App(bool, []) , App(bool, []) ]) ]) ), BVar(0)), FVar(q, App(bool, []) ))))
*)

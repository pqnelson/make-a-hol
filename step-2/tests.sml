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
    end)
];

val all_tests = suite "test" [Type_suite];

fun main () =
  run all_tests;

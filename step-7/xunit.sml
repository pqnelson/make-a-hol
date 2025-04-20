(* Unit test infrastructure, built specifically for Moscow ML *)
app load ["Time", "Timer"];

datatype TestResult = TestSuccess
                    | TestFailure of string
                    | TestException of exn;

datatype TestReport = ReportSuccess of { name : string
                                       , dt : Time.time }
                    | ReportFailure of { name : string
                                       , reason : string
                                       , dt : Time.time }
                    | ReportException of { name : string
                                         , error : exn
                                         , dt : Time.time }
                    | ReportSuite of { name : string
                                     , reports : TestReport list
                                     , dt : Time.time };

fun is_success (TestSuccess) = true
  | is_success _ = false;

fun is_fail (TestFailure _) = true
  | is_fail _ = false;

fun is_except (TestException _) = true
  | is_except _ = false;

datatype Test = Case of string * (unit -> TestResult)
              | Suite of string * (Test list);

fun test name f = Case (name,
                           fn () =>
                              f()
                              handle e => TestException e);

fun suite name tests = Suite (name, tests);

fun total_time start =
  let
    val {usr,sys} = Timer.checkCPUTimer start
  in
    Time.+(usr,sys)
  end;

fun generate_report (Case (name, f)) : TestReport =
  let
    val start = Timer.startCPUTimer ();
    val result = f();
    val dt = total_time start;
  in
    (case result of
      TestSuccess => ReportSuccess { name = name
                                     , dt = dt }
    | TestFailure msg => ReportFailure { name = name
                                        , reason = msg
                                        , dt = dt }
    | TestException e => ReportException { name = name
                                         , error = e
                                         , dt = dt })
  end
  | generate_report (Suite (name, tests)) = 
  let
    val start = Timer.startCPUTimer ();
    val results = map generate_report tests;
    val dt = total_time start;
  in
    ReportSuite { name = name
                , reports = results
                , dt = dt }
  end;

fun count_successes (ReportSuccess _) = 1
  | count_successes (ReportSuite {reports,...}) =
    foldr (op +) 0 (map count_successes reports)
  | count_successes _ = 0;

fun count_fails (ReportFailure _) = 1
  | count_fails (ReportSuite {reports,...}) =
    foldr (op +) 0 (map count_fails reports)
  | count_fails _ = 0;

fun count_errs (ReportException _) = 1
  | count_errs (ReportSuite {reports,...}) =
    foldr (op +) 0 (map count_errs reports)
  | count_errs _ = 0;

(* change this to LargeInt.toString to make it work for all
other Standard ML implementations... *)
fun interval_to_string (dt : Time.time) =
  (Int.toString (Time.toMicroseconds dt))^"ms";

fun report (parents : string list) (ReportSuccess _) = ""
  | report parents (ReportFailure {name,reason,dt}) =
    ((String.concatWith "." (rev (name::parents))) ^
     " FAIL: " ^
     reason ^
     "\n")
  | report parents (ReportException {name,error,dt}) =
    ((String.concatWith "." (rev (name::parents))) ^
     " ERROR: " ^
     (General.exnName error) ^ " " ^
     (General.exnMessage error) ^
     "\n")
  | report parents (r as ReportSuite {name,reports,dt}) =
    let
      val successes = count_successes r;
      val fails = count_fails r;
      val errs = count_errs r;
      val total = successes + fails + errs;
    in
      concat
      ["Running ",
       (String.concatWith "." (rev (name::parents))),
       "\n",
       (concat (map (report (name::parents)) reports)),
       "Tests run: ",
       Int.toString(total),
       ", Failures: ",
       Int.toString(fails),
       ", Errors: ",
       Int.toString(errs),
       ", Time elapsed: ",
       (interval_to_string dt),
       " - in ", name, "\n"]
    end;

fun run (t : Test) =
  (print o (report []) o generate_report) t;



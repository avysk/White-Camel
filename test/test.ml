open OUnit

;;

let suites_to_run =
  [Test_utils.suite] in
  List.map run_test_tt_main suites_to_run

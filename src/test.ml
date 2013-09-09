(* Testing with OUnit *)
open OUnit
open OUnit_XML
open Test_chien
open Test_set_reconciliation
open Test_find_roots
open Test_roots

let suite = "Set Reconciliation" >::: [ "test_chien_search" >:: test_chien_search ;
                                        "test_set_reconciliation" >:: test_set_reconc ;
                                        "test_set_reconciliation_bigger" >:: test_set_reconc_bigger ;
                                        "test_bta" >:: test_roots ;
                                        "test_ch_vs_bta" >:: test_compare]

let run_all_tests () =
  let _ = OUnit.run_test_tt suite
  in
  0

let run_all_tests_xml filename =
  let result = { result_list = [];
                 total_time = 0.0;
                 total_success = 0;
                 total_failures = 0;
                 total_errors = 0;
                 total_disabled = 0;
                 tmp_start = 0.0;
                 tmp_result = RSuccess []
               } in
  let _ = perform_timed_tests result suite in
  let () = print_xml result filename in
  0

let do_path p = Printf.printf "%s\n" (OUnit.string_of_path p)

let list_tests() =
 let () =  List.iter do_path (OUnit.test_case_paths suite) in
 0


type action=
| Usage
| AllTests
| AllTestsXML
| ListTests


let () =
  let _usage = "unit test driver.\n\n If you're clueless, try --help\n" in
  let usage () =
    print_endline _usage; 0
  in

  let action = ref Usage in
  let filename = ref "result.xml" in
  let set_action x ()= action := x in
  let interface = [
  ("--run-all-tests",Arg.Unit (set_action AllTests), "runs all tests");
  ("--run-all-tests-xml",
   Arg.Tuple [
     Arg.Set_string filename;
     Arg.Unit (set_action AllTestsXML)],
   "<filename> run tests producing jenkins compatible xml");
  ("--list-tests", Arg.Unit (set_action ListTests), "list tests")
  ]
  in
  let () =
    Arg.parse
      interface
      (fun x -> raise (Arg.Bad ("Bad argument " ^ x)))
      _usage
  in
  let rc =
    match !action with
    | Usage       -> usage ()
    | AllTests    -> run_all_tests ()
    | AllTestsXML -> run_all_tests_xml !filename
    | ListTests   -> list_tests ()
  in
  exit rc

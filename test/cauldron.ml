(* TODO: Figure out why VSCode thinks this is a type error*)
let lex str = Cauldron.Lexer.to_string (Cauldron.Lexer.tokenize str)

(* The tests *)
let test_function_application () =
  Alcotest.(check string)
    "lex 3 identifiers" "Identifier(add),Identifier(a),Identifier(b)"
    (lex "add a b")

let test_function_application_nested () =
  Alcotest.(check string)
    "lex 5 identifiers and 2 delimiters"
    "Identifier(sub),Delimiter((),Identifier(add),Identifier(a),Identifier(b),Delimiter()),Identifier(c)"
    (lex "sub (add a b) c")

let test_assignment_simple () =
  Alcotest.(check string)
    "lex simple variable assignment"
    "Keyword(let),Identifier(one),Operator(=),Integer(1)" (lex "let one = 1")

(* Run it *)
let () =
  let open Alcotest in
  run "Utils"
    [
      ( "lex-function-application",
        [
          test_case "lex application" `Quick test_function_application;
          test_case "lex nested application" `Quick
            test_function_application_nested;
          test_case "lex assignment" `Quick test_assignment_simple;
        ] );
    ]
(* TODO: Figure out why VSCode thinks this is a type error*)
let lex str = Cauldron.Lexer.to_string (Cauldron.Lexer.tokenize str)

let parse str =
  Cauldron.Parser.to_string
    (Cauldron.Parser.parse (Cauldron.Lexer.tokenize str))

(* The tests *)
let test_lex_function_application () =
  Alcotest.(check string)
    "lex 3 identifiers" "Identifier(add),Identifier(a),Identifier(b)"
    (lex "add a b")

let test_lex_function_application_nested () =
  Alcotest.(check string)
    "lex 5 identifiers and 2 delimiters"
    "Identifier(sub),Delimiter((),Identifier(add),Identifier(a),Identifier(b),Delimiter()),Identifier(c)"
    (lex "sub (add a b) c")

let test_lex_assignment_nested () =
  Alcotest.(check string)
    "lex 5 identifiers and 2 delimiters"
    "Identifier(sub),Delimiter((),Identifier(add),Identifier(a),Identifier(b),Delimiter()),Identifier(c)"
    (lex "let one = let two = 2 in sub two 1")

let test_lex_assignment_simple () =
  Alcotest.(check string)
    "lex simple variable assignment"
    "Keyword(let),Identifier(one),Operator(=),Integer(1)" (lex "let one = 1")

let test_parse_expression_simple () =
  Alcotest.(check string) "parse simple expression" "" (parse "let one = 1")

(* Run it *)
let () =
  let open Alcotest in
  run "Utils"
    [
      ( "parse",
        [
          test_case "parse expression simple" `Quick
            test_parse_expression_simple;
        ] );
      ( "lex",
        [
          test_case "lex application" `Quick test_lex_function_application;
          test_case "lex application nested" `Quick
            test_lex_function_application_nested;
          test_case "lex assignment" `Quick test_lex_assignment_simple;
          test_case "lex assignment nested" `Quick test_lex_assignment_nested;
        ] );
    ]
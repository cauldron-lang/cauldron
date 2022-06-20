module Program = struct
  type token = Integer of string

  type statement =
    | Assignment of string * statement
    | Expression of token
    | InvalidExpression

  type t = { errors : string list; statements : statement list; pointer : int }

  let statements program = program.statements
  let pointer program = program.pointer
  let init = { errors = []; statements = []; pointer = 1 }

  let with_statement program statement =
    {
      errors = program.errors;
      statements = statement :: program.statements;
      pointer = program.pointer;
    }

  let with_error program error =
    {
      errors = error :: program.errors;
      statements = program.statements;
      pointer = program.pointer;
    }

  let with_pointer program pointer =
    { errors = program.errors; statements = program.statements; pointer }

  let incr_pointer program =
    {
      errors = program.errors;
      statements = program.statements;
      pointer = program.pointer + 1;
    }
end

let parse_expression program tokens =
  let open Lexer in
  let open Lexer.Token in
  let current_token = Tokens.get_token_opt tokens (Program.pointer program) in
  match current_token with
  | Some (Integer integer) ->
      (program, Program.Expression (Program.Integer integer))
  | _ ->
      let new_program =
        Program.with_error program "Invalid token in prefix position"
      in
      let newer_program =
        Program.with_pointer new_program
          (Tokens.seek_delimiter tokens (Program.pointer new_program))
      in
      (newer_program, InvalidExpression)

let parse_assignment_body program tokens assignment_name =
  let open Lexer in
  let open Lexer.Token in
  let next_token = Tokens.get_token_opt tokens (Program.pointer program) in
  match next_token with
  | None | Some (Delimiter _) ->
      let new_program =
        Program.with_error program
          "Expected expression after assignment operator in assignment \
           statement"
      in
      Program.with_pointer new_program
        (Tokens.seek_delimiter tokens (Program.pointer new_program))
  | _ ->
      let new_program = Program.incr_pointer program in
      let newer_program, expression = parse_expression new_program tokens in
      let newest_program = Program.incr_pointer newer_program in
      Program.with_statement newest_program
        (Program.Assignment (assignment_name, expression))

let parse_assignment_operator program tokens assignment_name =
  let open Lexer in
  let open Lexer.Token in
  let next_token = Tokens.get_token_opt tokens (Program.pointer program) in
  match next_token with
  | Some (Operator "=") ->
      let new_program = Program.incr_pointer program in
      parse_assignment_body new_program tokens assignment_name
  | _ ->
      let new_program =
        Program.with_error program
          "Expected assignment operator after identifier in assignment \
           statement"
      in
      Program.with_pointer new_program
        (Tokens.seek_delimiter tokens (Program.pointer new_program))

let parse_assignment program tokens =
  let open Lexer in
  let next_poiner = Program.pointer program + 1 in
  let next_token = Tokens.get_token_opt tokens next_poiner in
  let open Lexer.Token in
  match next_token with
  | Some (Identifier assignment_name) ->
      let new_program = Program.incr_pointer program in
      parse_assignment_operator new_program tokens assignment_name
  | _ ->
      let new_program =
        Program.with_error program
          "Expected identifier after 'let' keyword in assignment statement"
      in
      Program.with_pointer new_program
        (Tokens.seek_delimiter tokens (Program.pointer new_program))

let parse_statement program tokens current_token =
  let open Lexer in
  let open Token in
  match current_token with
  | Keyword keyword_let when keyword_let == "let" ->
      parse_assignment program tokens
  | _ ->
      let new_program, expression = parse_expression program tokens in
      Program.with_statement new_program expression

let parse tokens =
  let rec inner_parse program tokens =
    match Lexer.Tokens.get_token_opt tokens (Program.pointer program) with
    | Some current_token ->
        let new_program = parse_statement program tokens current_token in
        inner_parse new_program tokens
    | None -> program
  in
  let program = Program.init in
  inner_parse program tokens

(* TODO: make tail recursive*)
let to_string program =
  let open Program in
  let rec statement_to_string statement =
    match statement with
    | Assignment (name, statement) ->
        "Assignment(" ^ name ^ statement_to_string statement ^ ")"
    | Expression token -> "Expression(" ^ token_to_string token ^ ")"
    | InvalidExpression -> "InvalidExpression"
  and token_to_string token =
    match token with Integer integer -> "Integer(" ^ integer ^ ")"
  in
  "Program("
  ^ Util.StringList.join
      (List.map statement_to_string (Program.statements program))
      ","
  ^ ")"

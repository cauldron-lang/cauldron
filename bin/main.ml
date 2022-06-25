let usage_msg = "cauldron"
let args = ref []
let anon_fun filename = args := filename :: !args
let speclist = []
let () = Arg.parse speclist anon_fun usage_msg
let subcommand = List.hd !args

let rec loop fn =
  let () =
    match In_channel.input_line stdin with Some line -> fn line | None -> ()
  in
  loop fn

let lex code =
  let tokens = Cauldron.Lexer.tokenize code in
  print_string (Cauldron.Lexer.to_string tokens);
  print_newline ();
  flush stdout

let parse code =
  let tokens = Cauldron.Lexer.tokenize code in
  let parsed = Cauldron.Parser.parse tokens in
  print_string (Cauldron.Parser.to_string parsed);
  Cauldron.Parser.print_errors parsed;
  print_newline ();
  flush stdout

let () =
  match subcommand with
  | "rlpl" -> loop lex
  | "rppl" -> loop parse
  | _ -> exit 1

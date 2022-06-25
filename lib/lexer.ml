module Token = struct
  type t =
    | Operator of string
    | Delimiter of string
    | Integer of string
    | Identifier of string
    | Keyword of string
    | Illegal

  let to_string token =
    match token with
    | Operator operator -> "Operator(" ^ operator ^ ")"
    | Delimiter delimiter -> "Delimiter(" ^ delimiter ^ ")"
    | Keyword keyword -> "Keyword(" ^ keyword ^ ")"
    | Integer integer -> "Integer(" ^ integer ^ ")"
    | Identifier identifier -> "Identifier(" ^ identifier ^ ")"
    | Illegal -> "Illegal"
end

module Tokens = struct
  type t = Token.t array

  let length = Array.length
  let get_token tokens pointer = Array.get tokens pointer

  let get_token_opt tokens pointer =
    if pointer < Array.length tokens && pointer >= 0 then
      Some (Array.get tokens pointer)
    else None

  let rec seek_delimiter tokens current_pointer =
    let next_pointer = current_pointer + 1 in
    let token = get_token_opt tokens next_pointer in
    let open Token in
    match token with
    | None | Some (Delimiter _) -> next_pointer
    | _ -> seek_delimiter tokens next_pointer
end

let rec range i j = if j < i then [] else j :: range i (j - 1)

let valid_chars =
  let lowercase = range 97 122 in
  let uppercase = range 65 90 in
  let digits = List.map Int.to_string (range 0 9) in
  let mapper c = c |> Char.chr |> String.make 1 in
  let chars = List.map mapper (List.concat [ lowercase; uppercase ]) in
  List.concat [ chars; digits; [ "_" ] ]

let valid_keywords = [ "let"; "in" ]

let get_next_char_opt pointer str =
  if !pointer + 1 < String.length str then
    Some (String.make 1 str.[!pointer + 1])
  else None

let tokenize str =
  let tokens = ref [] in
  let pointer = ref 0 in
  while !pointer < String.length str do
    let current = String.make 1 str.[!pointer] in
    let next = get_next_char_opt pointer str in
    let open Token in
    let () =
      match current with
      | " " -> ()
      | "+" | "-" | "*" | "/" -> tokens := Operator current :: !tokens
      | "=" | "!" | ">" | "<" -> (
          match next with
          | Some "=" ->
              tokens := Operator (current ^ "=") :: !tokens;
              pointer := !pointer + 1
          | _ -> tokens := Operator current :: !tokens)
      | ";" | "(" | ")" -> tokens := Delimiter current :: !tokens
      | _current when List.mem _current valid_chars -> (
          let buffer = ref _current in
          let keep_looping =
            ref
              (!pointer + 1 < String.length str
              && List.mem (String.make 1 str.[!pointer + 1]) valid_chars)
          in
          while !keep_looping do
            buffer := !buffer ^ String.make 1 str.[!pointer + 1];
            pointer := !pointer + 1;
            keep_looping :=
              !pointer + 1 < String.length str
              && List.mem (String.make 1 str.[!pointer + 1]) valid_chars
          done;
          match !buffer with
          | integer when int_of_string_opt integer != None ->
              tokens := Integer integer :: !tokens
          | keyword when List.mem keyword valid_keywords ->
              tokens := Keyword keyword :: !tokens
          | identifier -> tokens := Identifier identifier :: !tokens)
      | _ -> tokens := Illegal :: !tokens
    in

    pointer := !pointer + 1
  done;
  Array.of_list (List.rev !tokens)

let to_string tokens =
  String.concat "," (List.map Token.to_string (Array.to_list tokens))

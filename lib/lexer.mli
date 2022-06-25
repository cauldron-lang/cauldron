module Token : sig
  type t =
    | Operator of string
    | Delimiter of string
    | Integer of string
    | Identifier of string
    | Keyword of string
    | Illegal

  val to_string: t -> string
end

module Tokens : sig
  type t
  val length : t -> int
  val get_token : t -> int -> Token.t
  val get_token_opt : t -> int -> Token.t option
  
  (* This function given tokens and a pointer to one of them
   * will increment the given pointer until it is on the next delimiter
   * or it has exceeded the length of tokens *)
  val seek_delimiter : t -> int -> int
end

val tokenize: string -> Tokens.t

val to_string: Tokens.t -> string

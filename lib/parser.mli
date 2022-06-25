module Program : sig
    type t
end

val parse: Lexer.Tokens.t -> Program.t

val to_string: Program.t -> string

val print_errors: Program.t -> unit
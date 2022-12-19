use regex::Regex;

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Minus,
    Plus,
    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    Assignment,
    Bang,
    Divide,
    Multiply,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Boolean(bool),
    Delimiter(char),
    Identifier(String),
    Illegal,
    Integer(String),
    Keyword(String),
    MapInitializer(char),
    MapKeySuffix(char),
    Operator(Operator),
    String(String),
}

pub type Tokens = Vec<Token>;

pub fn tokenize(str: &str) -> Tokens {
    let mut tokens = Tokens::new();
    let mut characters = str.chars().peekable();
    let valid_integer = Regex::new("^[0-9]+$").unwrap();
    let valid_alphanum = Regex::new("^[a-zA-Z0-9_]+$").unwrap();
    let valid_keyword = Regex::new("^(if|fn|while)$").unwrap();
    let valid_delimiter = Regex::new("^(,|;|\\(|\\)|\\{|\\}|\\[|\\]|\\|)$").unwrap();
    let valid_boolean = Regex::new("^(true|false)$").unwrap();

    loop {
        let current = characters.next();
        let peek = characters.peek();

        match current {
            Some('"') => {
                let mut buffer = String::new();

                loop {
                    let current = characters.next();

                    match current {
                        Some('"') => {
                            tokens.push(Token::String(buffer));
                            break;
                        }
                        Some(char) => buffer.push(char.clone()),
                        None => {
                            // Unexpected end of string without closing paren
                            tokens.push(Token::Illegal);
                            break;
                        }
                    }
                }
            }
            Some(' ') => continue,
            Some('=') => {
                if peek == Some(&'=') {
                    tokens.push(Token::Operator(Operator::Equals));
                    characters.next();
                } else {
                    tokens.push(Token::Operator(Operator::Assignment));
                }
            }
            Some('!') => {
                if peek == Some(&'=') {
                    tokens.push(Token::Operator(Operator::NotEquals));
                    characters.next();
                } else {
                    tokens.push(Token::Operator(Operator::Bang))
                }
            }
            Some('-') => tokens.push(Token::Operator(Operator::Minus)),
            Some('+') => tokens.push(Token::Operator(Operator::Plus)),
            Some('/') => tokens.push(Token::Operator(Operator::Divide)),
            Some('*') => tokens.push(Token::Operator(Operator::Multiply)),
            Some('>') => tokens.push(Token::Operator(Operator::GreaterThan)),
            Some('<') => tokens.push(Token::Operator(Operator::LessThan)),
            Some('%') => tokens.push(Token::MapInitializer('%')),
            Some(':') => tokens.push(Token::MapKeySuffix(':')),
            Some(delimiter) if valid_delimiter.is_match(String::from(delimiter).as_str()) => {
                tokens.push(Token::Delimiter(delimiter))
            }
            Some(alphanum) if valid_alphanum.is_match(String::from(alphanum).as_str()) => {
                let mut buffer = String::from(alphanum);

                loop {
                    match characters.peek() {
                        Some(next_char)
                            if valid_alphanum
                                .is_match(String::from(next_char.clone()).as_str()) =>
                        {
                            buffer.push(next_char.clone())
                        }
                        _ => break,
                    }

                    characters.next();
                }

                match buffer.as_str() {
                    integer if valid_integer.is_match(integer) => {
                        tokens.push(Token::Integer(String::from(integer)))
                    }
                    keyword if valid_keyword.is_match(keyword) => {
                        tokens.push(Token::Keyword(String::from(keyword)))
                    }
                    boolean if valid_boolean.is_match(boolean) => {
                        tokens.push(Token::Boolean(boolean == "true"))
                    }
                    variable => tokens.push(Token::Identifier(String::from(variable))),
                }
            }
            None => break,
            _ => tokens.push(Token::Illegal),
        }
    }

    tokens
}

#[cfg(test)]
mod tests {
    use super::Operator;
    use crate::lexer::tokenize;
    use crate::lexer::Token;
    use crate::lexer::Tokens;

    fn assert_tokens_eq(code: &str, tokens: Vec<Token>) {
        assert_eq!(tokenize(code), Tokens::from(tokens));
    }

    #[test]
    fn it_lexes_calling() {
        let actual = tokenize("add(a, b)");
        let expected = Tokens::from(vec![
            Token::Identifier(String::from("add")),
            Token::Delimiter('('),
            Token::Identifier(String::from("a")),
            Token::Delimiter(','),
            Token::Identifier(String::from("b")),
            Token::Delimiter(')'),
        ]);
        assert_eq!(actual, expected);
    }

    #[test]
    fn it_lexes_nested_calling() {
        let actual = tokenize("sub(add(a, b), c)");
        let expected = Tokens::from(vec![
            Token::Identifier(String::from("sub")),
            Token::Delimiter('('),
            Token::Identifier(String::from("add")),
            Token::Delimiter('('),
            Token::Identifier(String::from("a")),
            Token::Delimiter(','),
            Token::Identifier(String::from("b")),
            Token::Delimiter(')'),
            Token::Delimiter(','),
            Token::Identifier(String::from("c")),
            Token::Delimiter(')'),
        ]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn it_lexes_multiple_calls() {
        let actual = tokenize("add(a, b); sub(a, b)");
        let expected = Tokens::from(vec![
            Token::Identifier(String::from("add")),
            Token::Delimiter('('),
            Token::Identifier(String::from("a")),
            Token::Delimiter(','),
            Token::Identifier(String::from("b")),
            Token::Delimiter(')'),
            Token::Delimiter(';'),
            Token::Identifier(String::from("sub")),
            Token::Delimiter('('),
            Token::Identifier(String::from("a")),
            Token::Delimiter(','),
            Token::Identifier(String::from("b")),
            Token::Delimiter(')'),
        ]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn it_lexes_assignment() {
        let actual = tokenize("a = 123");
        let expected = Tokens::from(vec![
            Token::Identifier(String::from("a")),
            Token::Operator(Operator::Assignment),
            Token::Integer(String::from("123")),
        ]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn it_lexes_callable_assignment() {
        let actual = tokenize("add = fn(a, b) { a + b }");
        let expected = Tokens::from(vec![
            Token::Identifier(String::from("add")),
            Token::Operator(Operator::Assignment),
            Token::Keyword(String::from("fn")),
            Token::Delimiter('('),
            Token::Identifier(String::from("a")),
            Token::Delimiter(','),
            Token::Identifier(String::from("b")),
            Token::Delimiter(')'),
            Token::Delimiter('{'),
            Token::Identifier(String::from("a")),
            Token::Operator(Operator::Plus),
            Token::Identifier(String::from("b")),
            Token::Delimiter('}'),
        ]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn it_lexes_conditional_statement() {
        let actual = tokenize("if (1) { print(1) }");
        let expected = Tokens::from(vec![
            Token::Keyword(String::from("if")),
            Token::Delimiter('('),
            Token::Integer(String::from("1")),
            Token::Delimiter(')'),
            Token::Delimiter('{'),
            Token::Identifier(String::from("print")),
            Token::Delimiter('('),
            Token::Integer(String::from("1")),
            Token::Delimiter(')'),
            Token::Delimiter('}'),
        ]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn it_lexes_greater_than_expressions() {
        assert_tokens_eq(
            "1 > 2",
            vec![
                Token::Integer(String::from("1")),
                Token::Operator(Operator::GreaterThan),
                Token::Integer(String::from("2")),
            ],
        )
    }

    #[test]
    fn it_lexes_less_than_expressions() {
        assert_tokens_eq(
            "1 < 2",
            vec![
                Token::Integer(String::from("1")),
                Token::Operator(Operator::LessThan),
                Token::Integer(String::from("2")),
            ],
        )
    }

    #[test]
    fn it_lexes_negative_number_expression() {
        assert_tokens_eq(
            "-100",
            vec![
                Token::Operator(Operator::Minus),
                Token::Integer(String::from("100")),
            ],
        );
    }

    #[test]
    fn it_lexes_bang_expression_1() {
        assert_tokens_eq(
            "!false",
            vec![Token::Operator(Operator::Bang), Token::Boolean(false)],
        );
    }

    #[test]
    fn it_lexes_equality_expressions_1() {
        assert_tokens_eq(
            "a == b",
            vec![
                Token::Identifier(String::from("a")),
                Token::Operator(Operator::Equals),
                Token::Identifier(String::from("b")),
            ],
        );
    }

    #[test]
    fn it_lexes_equality_expressions_2() {
        assert_tokens_eq(
            "a != b",
            vec![
                Token::Identifier(String::from("a")),
                Token::Operator(Operator::NotEquals),
                Token::Identifier(String::from("b")),
            ],
        );
    }

    #[test]
    fn it_lexes_group_expressions() {
        assert_tokens_eq(
            "(100 + 50) * 5",
            vec![
                Token::Delimiter('('),
                Token::Integer(String::from("100")),
                Token::Operator(Operator::Plus),
                Token::Integer(String::from("50")),
                Token::Delimiter(')'),
                Token::Operator(Operator::Multiply),
                Token::Integer(String::from("5")),
            ],
        );
    }

    #[test]
    fn it_lexes_strings() {
        assert_tokens_eq("\"foobar\"", vec![Token::String(String::from("foobar"))])
    }

    #[test]
    fn it_lexes_illegal_strings() {
        assert_tokens_eq("\"foobar", vec![Token::Illegal])
    }

    #[test]
    fn it_lexes_vectors() {
        assert_tokens_eq(
            "[1]",
            vec![
                Token::Delimiter('['),
                Token::Integer(String::from("1")),
                Token::Delimiter(']'),
            ],
        )
    }

    #[test]
    fn it_lexes_maps() {
        assert_tokens_eq(
            "%[foo: \"bar\"]",
            vec![
                Token::MapInitializer('%'),
                Token::Delimiter('['),
                Token::Identifier(String::from("foo")),
                Token::MapKeySuffix(':'),
                Token::String(String::from("bar")),
                Token::Delimiter(']'),
            ],
        )
    }

    #[test]
    fn it_lexes_looping_statement() {
        let actual = tokenize("while (true) { print(1) }");
        let expected = Tokens::from(vec![
            Token::Keyword(String::from("while")),
            Token::Delimiter('('),
            Token::Boolean(true),
            Token::Delimiter(')'),
            Token::Delimiter('{'),
            Token::Identifier(String::from("print")),
            Token::Delimiter('('),
            Token::Integer(String::from("1")),
            Token::Delimiter(')'),
            Token::Delimiter('}'),
        ]);

        assert_eq!(actual, expected);
    }
}

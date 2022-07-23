use regex::Regex;

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Minus,
    Plus,
    Equals,
    NotEquals,
    Assignment,
    Bang,
    Divide,
    Multiply,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Operator(Operator),
    Delimiter(char),
    Integer(String),
    Boolean(bool),
    Identifier(String),
    Keyword(String),
    Illegal,
}

pub type Tokens = Vec<Token>;

pub fn tokenize(str: &str) -> Tokens {
    let mut tokens = Tokens::new();
    let mut characters = str.chars().peekable();
    let valid_integer = Regex::new("^[0-9]+$").unwrap();
    let valid_alphanum = Regex::new("^[a-zA-Z0-9_]+$").unwrap();
    let valid_keyword = Regex::new("^(if)$").unwrap();
    let valid_delimiter = Regex::new("^(,|;|\\(|\\)|\\{|\\}|\\|)$").unwrap();
    let valid_boolean = Regex::new("^(true|false)$").unwrap();

    loop {
        let current = characters.next();
        let peek = characters.peek();

        match current {
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
        let actual = tokenize("add = {|a, b| a + b }");
        let expected = Tokens::from(vec![
            Token::Identifier(String::from("add")),
            Token::Operator(Operator::Assignment),
            Token::Delimiter('{'),
            Token::Delimiter('|'),
            Token::Identifier(String::from("a")),
            Token::Delimiter(','),
            Token::Identifier(String::from("b")),
            Token::Delimiter('|'),
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
    fn it_lexes_simple_expressions() {
        let expectations = [
            (
                "-100",
                vec![
                    Token::Operator(Operator::Minus),
                    Token::Integer(String::from("100")),
                ],
            ),
            (
                "!false",
                vec![Token::Operator(Operator::Bang), Token::Boolean(false)],
            ),
            (
                "a == b",
                vec![
                    Token::Identifier(String::from("a")),
                    Token::Operator(Operator::Equals),
                    Token::Identifier(String::from("b")),
                ],
            ),
            (
                "a != b",
                vec![
                    Token::Identifier(String::from("a")),
                    Token::Operator(Operator::NotEquals),
                    Token::Identifier(String::from("b")),
                ],
            ),
            (
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
            ),
        ];

        for (code, tokens) in expectations {
            let actual = tokenize(code);
            let expected = Tokens::from(tokens);

            assert_eq!(actual, expected);
        }
    }
}

use regex::Regex;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Operator(char),
    Delimiter(char),
    Integer(String),
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
    let valid_keyword = Regex::new("^(let|in)$").unwrap();
    let valid_delimiter = Regex::new("^(;|\\(|\\))$").unwrap();

    loop {
        let current = characters.next();

        match current {
            Some(' ') => continue,
            Some(equals) if equals == '=' => tokens.push(Token::Operator('=')),
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
    use crate::lexer::tokenize;
    use crate::lexer::Token;
    use crate::lexer::Tokens;

    #[test]
    fn it_lexes_function_application() {
        let actual = tokenize("add a b");
        let expected = Tokens::from(vec![
            Token::Identifier(String::from("add")),
            Token::Identifier(String::from("a")),
            Token::Identifier(String::from("b")),
        ]);
        assert_eq!(actual, expected);
    }

    #[test]
    fn it_lexes_function_application_nested() {
        let actual = tokenize("sub (add a b) c");
        let expected = Tokens::from(vec![
            Token::Identifier(String::from("sub")),
            Token::Delimiter('('),
            Token::Identifier(String::from("add")),
            Token::Identifier(String::from("a")),
            Token::Identifier(String::from("b")),
            Token::Delimiter(')'),
            Token::Identifier(String::from("c")),
        ]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn it_lexes_assignment_nested() {
        let actual = tokenize("let a = let b = 123 in let c = 456 in add b c");
        let expected = Tokens::from(vec![
            Token::Keyword(String::from("let")),
            Token::Identifier(String::from("a")),
            Token::Operator('='),
            Token::Keyword(String::from("let")),
            Token::Identifier(String::from("b")),
            Token::Operator('='),
            Token::Integer(String::from("123")),
            Token::Keyword(String::from("in")),
            Token::Keyword(String::from("let")),
            Token::Identifier(String::from("c")),
            Token::Operator('='),
            Token::Integer(String::from("456")),
            Token::Keyword(String::from("in")),
            Token::Identifier(String::from("add")),
            Token::Identifier(String::from("b")),
            Token::Identifier(String::from("c")),
        ]);

        assert_eq!(actual, expected);
    }
}

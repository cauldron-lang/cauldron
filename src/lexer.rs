use regex::Regex;

#[derive(Debug)]
enum Token {
    Operator(char),
    Delimiter(char),
    Integer(String),
    Identifier(String),
    Keyword(String),
    Illegal
}

#[derive(Debug)]
pub struct Tokens {
    contents: Vec<Token>
}

impl Tokens {
    fn new() -> Self {
        Self { contents: Vec::new() }
    }

    fn push(&mut self, token: Token) {
        self.contents.push(token);
    }
}

pub fn tokenize(str: &str) -> Tokens {
    let mut tokens = Tokens::new();
    let mut characters = str.chars().peekable();
    let valid_integer = Regex::new("^[0-9]$").unwrap();
    let valid_alphanum = Regex::new("^[a-zA-Z0-9_]+$").unwrap();
    let valid_keyword = Regex::new("^(let)$").unwrap();
    let valid_delimiter = Regex::new("^(;|\\(|\\))$").unwrap();

    loop {
        let current = characters.next();

        match current {
            Some(' ') => continue,
            Some(equals) if equals == '=' => tokens.push(Token::Operator('=')),
            Some(delimiter) if valid_delimiter.is_match(String::from(delimiter).as_str())  => tokens.push(Token::Delimiter(delimiter)),
            Some(alphanum) if valid_alphanum.is_match(String::from(alphanum).as_str()) =>  {
                let mut buffer = String::from(alphanum);

                loop {
                    match characters.peek() {
                        Some(next_char) if valid_alphanum.is_match(String::from(next_char.clone()).as_str()) => buffer.push(next_char.clone()),
                        _ => break
                    }

                    characters.next();
                }

                match buffer.as_str() {
                    integer if valid_integer.is_match(integer) => tokens.push(Token::Integer(String::from(integer))),
                    keyword if valid_keyword.is_match(keyword) => tokens.push(Token::Keyword(String::from(keyword))),
                    variable => tokens.push(Token::Identifier(String::from(variable))),
                }
            },
            None => break,
            _ => tokens.push(Token::Illegal)
        }
    }

    tokens
}
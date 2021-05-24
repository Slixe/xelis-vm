#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier,
    ValNumber,
    ValString,

    CommentDoubleSlashes,
    CommentMultiline,

    BraceOpen,
    BraceClose,
    BracketOpen,
    BracketClose,

    Const,
    Let,
    Entry,
    Function,
    Dot,
    Comma,
    Colon,
    Return,
    If,
    Else,
    For,
    While,
    Break,
    Continue,

    ParenthesisOpen,
    ParenthesisClose,
    Struct,

    OperatorAssign,
    OperatorEquals,
    OperatorNotEquals,
    OperatorAnd,
    OperatorOr,
    OperatorGreaterThan,
    OperatorLessThan,
    OperatorGreaterOrEqual,
    OperatorLessOrEqual,

    OperatorPlus,
    OperatorMinus,
    OperatorMultiply,
    OperatorDivide,
    OperatorModulo,

    OperatorPlusAssign,
    OperatorMinusAssign,
    OperatorMultiplyAssign,
    OperatorDivideAssign,
    OperatorBitwiseLeft,
    OperatorBitwiseRight,

    Null,
    True,
    False,
}

impl Token {
    fn value_of_characters(characters: &Vec<char>) -> Option<Token> {
        let value: String = characters.into_iter().collect();
        return Token::value_of(value);
    }

    fn value_of(characters: String) -> Option<Token> {
        use Token::*;

        Some(match characters.as_str() {
            "'" | "\"" => ValString,
            "//" => CommentDoubleSlashes,
            "/*" => CommentMultiline,

            "{" => BraceOpen,
            "}" => BraceClose,
            "[" => BracketOpen,
            "]" => BracketClose,

            "const" => Const,
            "let" => Let,
            "entry" => Entry,
            "func" => Function,
            "." => Dot,
            "," => Comma,
            ":" => Colon,
            "return" => Return,
            "if" => If,
            "else" => Else,
            "for" => For,
            "while" => While,
            "break" => Break,
            "continue" => Continue,

            "(" => ParenthesisOpen,
            ")" => ParenthesisClose,
            "struct" => Struct,

            "=" => OperatorAssign,
            "==" => OperatorEquals,
            "!=" => OperatorNotEquals,
            "&&" => OperatorAnd,
            "||" => OperatorOr,
            ">" => OperatorGreaterThan,
            "<" => OperatorLessThan,
            ">=" => OperatorGreaterOrEqual,
            "<=" => OperatorLessOrEqual,

            "+" => OperatorPlus,
            "-" => OperatorMinus,
            "*" => OperatorMultiply,
            "/" => OperatorDivide,
            "%" => OperatorModulo,

            "+=" => OperatorPlusAssign,
            "-=" => OperatorMinusAssign,
            "*=" => OperatorMultiplyAssign,
            "/=" => OperatorDivideAssign,

            "<<" => OperatorBitwiseLeft,
            ">>" => OperatorBitwiseRight,

            "null" => Null,
            "true" => True,
            "false" => False,

            _ => return None,
        })
    }

    pub fn is_operator(&self) -> bool {
        use Token::*;
        match self {
            | OperatorEquals
            | OperatorNotEquals
            | OperatorAnd
            | OperatorOr
            | OperatorGreaterThan
            | OperatorLessThan
            | OperatorGreaterOrEqual
            | OperatorLessOrEqual
            | OperatorPlus
            | OperatorMinus
            | OperatorMultiply
            | OperatorDivide
            | OperatorModulo
            /*| OperatorPlusAssign
            | OperatorMinusAssign
            | OperatorMultiplyAssign
            | OperatorDivideAssign*/
            | OperatorBitwiseLeft
            | OperatorBitwiseRight
            | Dot => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TokenValue {
    pub line: usize,
    pub column: usize,
    pub token: Token,
    pub value: String,
}

pub struct Lexer {
    //line & column are used for syntax error
    content: Vec<char>,
    line: usize,
    column: usize,
    cursor: usize,
    tokens: Vec<TokenValue>,
}

impl Lexer {
    pub fn new(content: String) -> Lexer {
        return Lexer {
            content: content.chars().collect(),
            cursor: 0,
            line: 1,
            column: 0,
            tokens: vec![],
        };
    }

    pub fn get(mut self) -> Vec<TokenValue> {
        while self.has_current() {
            if let Some(value) = self.next_token() {
                self.tokens.push(value)
            }
        }

        return self.tokens;
    }

    fn current(&mut self) -> Option<char> {
        let value = self._current();
        self.cursor_plus();
        return value;
    }

    fn _current(&self) -> Option<char> {
        return self.content.get(self.cursor).copied();
    }

    fn next(&self) -> Option<char> {
        return self.content.get(self.cursor + 1).copied();
    }

    fn has_current(&self) -> bool {
        return self.cursor < self.content.len();
    }

    fn has_next(&self) -> bool {
        return self.cursor + 1 < self.content.len()
            && match self.next() {
                Some(value) => value != ' ',
                None => false,
            };
    }

    fn _cursor_plus(&mut self, n: usize) {
        self.cursor += n;
        self.column += n;
    }

    fn _cursor_minus(&mut self, n: usize) {
        self.cursor -= n;
        self.column -= n;
    }

    fn cursor_plus(&mut self) {
        self._cursor_plus(1);
    }

    fn next_token(&mut self) -> Option<TokenValue> {
        let first = self._current()?;

        if first == ' ' || first == '\n' {
            if first == '\n' {
                self.line += 1;
                self.cursor += 1;
                self.column = 0;
            } else {
                self.cursor_plus();
            }

            return None;
        } else if first.is_numeric() {
            let value = self
                .read_while(|c: char| -> bool {
                    return c.is_numeric() || c == '_';
                })
                .iter()
                .collect();
            self._cursor_minus(1);

            return Some(TokenValue {
                line: self.line,
                column: self.column,
                token: Token::ValNumber,
                value: value,
            });
        } else if first == '"' || first == '\'' {
            self.cursor_plus();
            let value = self.read_until(first);
            return Some(TokenValue {
                line: self.line,
                column: self.column,
                token: Token::ValString,
                value: value.iter().collect(),
            });
        } else if first.is_alphabetic() {
            let value = self.read_word();
            return Some(TokenValue {
                line: self.line,
                column: self.column,
                token: match Token::value_of_characters(&value) {
                    Some(value) => value,
                    None => Token::Identifier,
                },
                value: value.iter().collect(),
            });
        } else if self.has_next() {
            let chars: Vec<char> = vec![first, self.next()?]; //should not return
            if let Some(value) = Token::value_of_characters(&chars) {
                self._cursor_plus(2);
                if value == Token::CommentDoubleSlashes {
                    /*chars = */
                    self.read_while(|character: char| -> bool { character != '\n' });
                    return None; //we don't save comments yet
                }
                /*else if value == Token::CommentMultiline { // TODO
                    self.read_while(|character: char | -> bool {
                        character != '*' && self.next()? != '/'
                    });
                }*/
                return Some(TokenValue {
                    line: self.line,
                    column: self.column,
                    token: value,
                    value: chars.iter().collect(),
                });
            }
        }

        let mut value: Vec<char> = vec![first];
        let token = match Token::value_of_characters(&value) {
            Some(value) => {
                self.cursor_plus();
                value
            }
            None => {
                value = self.read_until(' ');
                Token::value_of_characters(&value)?
            }
        };

        return Some(TokenValue {
            line: self.line,
            column: self.column,
            token: token,
            value: value.iter().collect(),
        });
    }

    fn read_while<F>(&mut self, condition: F) -> Vec<char>
    where
        F: Fn(char) -> bool,
    {
        let mut value: Vec<char> = vec![];

        let mut current = match self.current() {
            Some(value) => value,
            None => return value,
        };
        while condition(current) && self.has_current() {
            if current != '\n' {
                value.push(current);
            }

            current = match self.current() {
                Some(value) => value,
                None => return value,
            };
        }

        return value;
    }

    fn read_until(&mut self, delimiter: char) -> Vec<char> {
        let value = self.read_while(|character: char| -> bool {
            return character != delimiter;
        });

        return value;
    }

    fn read_word(&mut self) -> Vec<char> {
        let val = self.read_while(|character: char| -> bool {
            return character.is_alphanumeric() || character == '_';
        });
        self._cursor_minus(1);
        val
    }
}

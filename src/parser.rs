use crate::ast::{NodeType, Program, Statement, Expression, LetStatement, Identifier, Node};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::{ mem, fmt::Write };

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Parser {
            lexer,
            cur_token,
            peek_token,
            errors: vec![]
        }
    }

    fn next_token(&mut self) {
        self.cur_token = mem::take(&mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.cur_token.token_type != TokenType::Eof {
            let statement = self.parse_statement();

            program.statements.push(statement);
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            _ => None
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let let_token = mem::take(&mut self.cur_token);
        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let identifier_value = self.cur_token.literal.clone();
        let identifier_token = mem::take(&mut self.cur_token);

        let identifier = Identifier { token : identifier_token, value: identifier_value};

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }

        //TODO: parse the expression part, for now skip it
        while !self.cur_token_is(TokenType::SemiColon) {
            self.next_token();
        }

        Some(Statement::Let(LetStatement {
            token: let_token,
            name: identifier,
        }))
    }

    // helper functions
    fn cur_token_is(&self, token_type : TokenType) -> bool {
        self.cur_token.token_type == token_type
    }

    fn peek_token_is(&self, token_type : TokenType) -> bool {
        self.peek_token.token_type == token_type
    }

    fn expect_peek(&mut self, token_type : TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            true
        } else {
            self.peek_error(token_type);
            false
        }
    }

    fn peek_error(&mut self, token_type: TokenType) {
        let message = format!("expected next token to be {}, got {} instead", token_type, self.peek_token.token_type);
        self.errors.push(message);
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_let_statements() {
        let input = r#"
            let x = 5;
            let y = 10;
            let foobar = 838383;
        "#;

        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        let statements_count = program.statements.len();
        assert_eq!(
            statements_count, 3,
            "program.Statements does not contain 3 statements. got={statements_count}"
        );

        let tests = ["x", "y", "foobar"];
        for (index, expected_identifier) in tests.iter().enumerate() {
            let stmt = program.statements[index].as_ref().unwrap();
            let token_literal = stmt.token_literal();
            let node_type = stmt.node_type();
            assert_eq!(
                token_literal, "let",
                "stmt.token_literal is not 'let'. got = {token_literal}"
            );

            assert_eq!(
                node_type,
                NodeType::Let,
                "stmt is not ast::LetStatement. got = {node_type}"
            );

            let Statement::Let(let_statement) = stmt;
                let ident_val = &let_statement.name.value;
                let ident_literal = &let_statement.name.token_literal();
                assert_eq!(ident_val, expected_identifier, "let_statment.name.value not {expected_identifier}. got={ident_val}");

                assert_eq!(ident_literal, expected_identifier, "let_statement.name.token_literal() not {expected_identifier}. got={ident_literal}");
        }
    }

    fn check_parser_errors(parser: &Parser) {
        let error_len = parser.errors.len();

        if error_len == 0 {
            return 
        }

        let mut buffer = String::new();

        write!(&mut buffer, "parser has {} errors \n", error_len).unwrap();
        for msg in &parser.errors {
            write!(&mut buffer, "parser error: {} \n", msg).unwrap();
        }

        assert_eq!(error_len, 0, "{}" ,buffer);
    }
}

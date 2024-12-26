use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::{collections::HashMap, fmt::Write, mem};

type PrefixParseFn = fn(&mut Parser) -> Option<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Option<Expression>;

#[derive(PartialEq, Eq)]
enum Precedence {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Parser {
            lexer,
            cur_token,
            peek_token,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        }
    }

    fn next_token(&mut self) {
        self.cur_token = mem::take(&mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn register_prefix_infix_fns(&mut self) {
        self.register_prefix(TokenType::Ident, Parser::parse_identifier);
        self.register_prefix(TokenType::Int, Parser::parse_integer_literal);
    }

    fn parse_program(&mut self) -> Program {
        self.register_prefix_infix_fns();

        let mut program = Program { statements: vec![] };
        while self.cur_token.token_type != TokenType::Eof {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let let_token = mem::take(&mut self.cur_token);
        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let identifier_value = self.cur_token.literal.clone();
        let identifier_token = mem::take(&mut self.cur_token);

        let identifier = Identifier {
            token: identifier_token,
            value: identifier_value,
        };

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

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let return_token = mem::take(&mut self.cur_token);

        self.next_token();

        //TODO: parse the expression part

        while !self.cur_token_is(TokenType::SemiColon) {
            self.next_token();
        }

        Some(Statement::Return(ReturnStatement {
            token: return_token,
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let cur_token = self.cur_token.clone();
        let expression = self.parse_expression(Precedence::Lowest)?;

        // semicolon is optional, like if 5 + 5 is typed to the REPL
        if self.peek_token_is(TokenType::SemiColon) {
            self.next_token();
        }

        Some(Statement::Expression(ExpressionStatement {
            token: cur_token,
            expression,
        }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix_fn = self.prefix_parse_fns.get(&self.cur_token.token_type)?;

        let left_exp = prefix_fn(self);

        left_exp
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        let identifier_value = self.cur_token.literal.clone();
        let identifier_token = mem::take(&mut self.cur_token);

        Some(Expression::Ident(Identifier {
            token: identifier_token,
            value: identifier_value,
        }))
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let num_val: i64 = self.cur_token.literal.parse().ok()?;

        Some(Expression::Int(IntegerLiteral {
            token: mem::take(&mut self.cur_token),
            value: num_val,
        }))
    }

    // helper functions
    fn cur_token_is(&self, token_type: TokenType) -> bool {
        self.cur_token.token_type == token_type
    }

    fn peek_token_is(&self, token_type: TokenType) -> bool {
        self.peek_token.token_type == token_type
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            true
        } else {
            self.peek_error(token_type);
            false
        }
    }

    fn peek_error(&mut self, token_type: TokenType) {
        let message = format!(
            "expected next token to be {}, got {} instead",
            token_type, self.peek_token.token_type
        );
        self.errors.push(message);
    }

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
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
            let stmt = &program.statements[index];
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

            if let Statement::Let(let_statement) = stmt {
                let ident_val = &let_statement.name.value;
                let ident_literal = &let_statement.name.token_literal();
                assert_eq!(
                    ident_val, expected_identifier,
                    "let_statment.name.value not {expected_identifier}. got={ident_val}"
                );

                assert_eq!(ident_literal, expected_identifier, "let_statement.name.token_literal() not {expected_identifier}. got={ident_literal}");
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
            return 5;
            return 10;
            return 993322;
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

        for stmt in program.statements {
            let node_type = stmt.node_type();
            assert_eq!(
                node_type,
                NodeType::Return,
                "stmt not ast.ReturnStatement. got={node_type}"
            );
            if let Statement::Return(return_statement) = stmt {
                let return_token_literal = return_statement.token_literal();
                assert_eq!(
                    return_token_literal, "return",
                    "return_statement.token_literal not 'return', got {return_token_literal}"
                );
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        let stmt_len = program.statements.len();
        assert_eq!(
            stmt_len, 1,
            "program statements length is wrong, got={stmt_len}"
        );

        let stmt = &program.statements[0];
        let node_type = stmt.node_type();
        assert_eq!(
            node_type,
            NodeType::ExpressionStatement,
            "program.statements[0] is not ast::ExpressionStatement. got = {node_type}"
        );

        if let Statement::Expression(ident_expr) = stmt {
            let node_type = ident_expr.expression.node_type();
            assert_eq!(
                node_type,
                NodeType::Identifier,
                "exp not ast::Identifier. got {node_type}"
            );

            if let Expression::Ident(ident) = &ident_expr.expression {
                let ident_val = &ident.value;
                let ident_token_literal = ident.token_literal();
                assert_eq!(
                    ident_val, "foobar",
                    "ident.value not foobar, got={ident_val}"
                );
                assert_eq!(
                    ident_token_literal, "foobar",
                    "ident.token_literal() not foobar, got={ident_token_literal}"
                );
            }
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program doesn't have enough statements. got={}",
            program.statements.len()
        );
        assert_eq!(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
            "program.statements[0] is not NodeType::ExpressionSttaement. got={}",
            program.statements[0].node_type()
        );

        if let Statement::Expression(expr_stmt) = &program.statements[0] {
            assert_eq!(
                expr_stmt.expression.node_type(),
                NodeType::IntegerLiteral,
                "exp nit ast::IntegerLiteral. got={}",
                expr_stmt.expression.node_type()
            );

            if let Expression::Int(int_literal) = &expr_stmt.expression {
                assert_eq!(
                    int_literal.value, 5,
                    "int_literal.value not {}. got={}",
                    5, int_literal.value
                );
                assert_eq!(
                    int_literal.token_literal(),
                    "5",
                    "int_literal.token_literal not {}. got={}",
                    "5",
                    int_literal.token_literal()
                );
            }
        }
    }

    // helper methods
    fn check_parser_errors(parser: &Parser) {
        let error_len = parser.errors.len();

        if error_len == 0 {
            return;
        }

        let mut buffer = String::new();

        write!(&mut buffer, "parser has {} errors \n", error_len).unwrap();
        for msg in &parser.errors {
            write!(&mut buffer, "parser error: {} \n", msg).unwrap();
        }

        assert_eq!(error_len, 0, "{}", buffer);
    }
}

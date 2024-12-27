use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::{collections::HashMap, fmt::Write, mem};

type PrefixParseFn = fn(&mut Parser) -> Option<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Option<Expression>;

#[derive(PartialEq, Eq, Copy, Clone, PartialOrd, Debug)]
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
    precedence_map: HashMap<TokenType, Precedence>,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        let mut precedence_map: HashMap<TokenType, Precedence> = HashMap::new();
        precedence_map.insert(TokenType::Eq, Precedence::Equals);
        precedence_map.insert(TokenType::NotEq, Precedence::Equals);
        precedence_map.insert(TokenType::LessThan, Precedence::LessGreater);
        precedence_map.insert(TokenType::GreaterThan, Precedence::LessGreater);
        precedence_map.insert(TokenType::Plus, Precedence::Sum);
        precedence_map.insert(TokenType::Minus, Precedence::Sum);
        precedence_map.insert(TokenType::Slash, Precedence::Product);
        precedence_map.insert(TokenType::Asterisk, Precedence::Product);

        let mut prefix_parse_fns: HashMap<TokenType, PrefixParseFn> = HashMap::new();
        prefix_parse_fns.insert(TokenType::Ident, Parser::parse_identifier);
        prefix_parse_fns.insert(TokenType::Int, Parser::parse_integer_literal);
        prefix_parse_fns.insert(TokenType::Bang, Parser::parse_prefix_expression);
        prefix_parse_fns.insert(TokenType::Minus, Parser::parse_prefix_expression);

        let mut infix_parse_fns: HashMap<TokenType, InfixParseFn> = HashMap::new();
        infix_parse_fns.insert(TokenType::Plus, Parser::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Minus, Parser::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Slash, Parser::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Asterisk, Parser::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Eq, Parser::parse_infix_expression);
        infix_parse_fns.insert(TokenType::NotEq, Parser::parse_infix_expression);
        infix_parse_fns.insert(TokenType::LessThan, Parser::parse_infix_expression);
        infix_parse_fns.insert(TokenType::GreaterThan, Parser::parse_infix_expression);

        Parser {
            lexer,
            cur_token,
            peek_token,
            errors: vec![],
            prefix_parse_fns,
            infix_parse_fns,
            precedence_map,
        }
    }

    fn next_token(&mut self) {
        self.cur_token = mem::take(&mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Program {
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
        if let Some(prefix_fn) = self.prefix_parse_fns.get(&self.cur_token.token_type) {
            let mut left_exp = prefix_fn(self)?;

            while !self.peek_token_is(TokenType::SemiColon) && precedence < self.peek_precedence() {
                let peek_token_type = self.peek_token.token_type.clone();
                if !self.infix_parse_fns.contains_key(&peek_token_type) {
                    return Some(left_exp);
                }

                self.next_token();
                let infix_fn = self.infix_parse_fns.get(&peek_token_type).unwrap();
                left_exp = infix_fn(self, left_exp)?;
            }

            Some(left_exp)
        } else {
            self.errors.push(format!(
                "no prefix parse function for {} found",
                &self.cur_token.token_type
            ));

            None
        }
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

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator = self.cur_token.literal.to_string();
        let cur_token = mem::take(&mut self.cur_token);
        self.next_token();
        let right_expr = self.parse_expression(Precedence::Prefix)?;

        Some(Expression::Prefix(Box::new(PrefixExpression {
            token: cur_token,
            operator: operator,
            right: Box::new(right_expr),
        })))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let operator = self.cur_token.literal.to_string();
        let precedence = self.cur_precedence();
        let cur_token = mem::take(&mut self.cur_token);
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Some(Expression::Infix(Box::new(InfixExpression {
            token: cur_token,
            left: Box::new(left),
            operator: operator,
            right: Box::new(right),
        })))
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

    fn peek_precedence(&self) -> Precedence {
        if let Some(precedence) = self.precedence_map.get(&self.peek_token.token_type) {
            *precedence
        } else {
            Precedence::Lowest
        }
    }

    fn cur_precedence(&self) -> Precedence {
        if let Some(precedence) = self.precedence_map.get(&self.cur_token.token_type) {
            *precedence
        } else {
            Precedence::Lowest
        }
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

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests: [(&str, &str, i64); 2] = [("!5;", "!", 5), ("-15;", "-", 15)];

        for (input, operator, int_val) in prefix_tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain {} statements. got={}",
                1,
                program.statements.len()
            );

            assert_eq!(
                program.statements[0].node_type(),
                NodeType::ExpressionStatement,
                "program.statements[0] is not ast::ExpressionStatement. got={}",
                program.statements[0].node_type()
            );

            if let Statement::Expression(expr_stmt) = &program.statements[0] {
                assert_eq!(
                    expr_stmt.expression.node_type(),
                    NodeType::PrefixExpression,
                    "stmt is not ast::PrefixExpression. got={}",
                    expr_stmt.expression.node_type()
                );

                if let Expression::Prefix(exp) = &expr_stmt.expression {
                    assert_eq!(
                        exp.operator, operator,
                        "exp.operator is not {}. got={}",
                        operator, exp.operator
                    );

                    test_integer_literal(exp.right.as_ref(), int_val);
                }
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests: Vec<(&str, i64, &str, i64)> = vec![
            ("5+5;", 5, "+", 5),
            ("5-5;", 5, "-", 5),
            ("5*5;", 5, "*", 5),
            ("5/5;", 5, "/", 5),
            ("5>5;", 5, ">", 5),
            ("5<5;", 5, "<", 5),
            ("5==5;", 5, "==", 5),
            ("5!=5;", 5, "!=", 5),
        ];

        for (input, left_val, operator, right_val) in infix_tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            test_statements_len(program.statements.len(), 1);
            test_node_type(
                program.statements[0].node_type(),
                NodeType::ExpressionStatement,
            );

            if let Statement::Expression(expr) = &program.statements[0] {
                test_node_type(expr.expression.node_type(), NodeType::InfixExpression);

                if let Expression::Infix(infix) = &expr.expression {
                    test_integer_literal(infix.left.as_ref(), left_val);
                    assert_eq!(
                        infix.operator, operator,
                        "exp.operator is not {}. got={}",
                        operator, infix.operator
                    );
                    test_integer_literal(infix.right.as_ref(), left_val);
                }
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests: Vec<(&str, &str)> = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 != 3 > 4", "((5 > 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            let actual = program.to_string();
            assert_eq!(actual, expected, "expected={}, got={}", expected, actual);
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

    fn test_integer_literal(int_exp: &Expression, val: i64) {
        assert_eq!(
            int_exp.node_type(),
            NodeType::IntegerLiteral,
            "il not ast::IntegerLiteral. got={}",
            int_exp
        );

        if let Expression::Int(integ) = &int_exp {
            assert_eq!(
                integ.value, val,
                "integ.value not {}. got={}",
                val, integ.value
            );

            assert_eq!(
                integ.token_literal(),
                val.to_string(),
                "integ.token_literal() not {}. got={}",
                val,
                integ.token_literal()
            );
        }
    }

    fn test_statements_len(stmt_len: usize, expected_len: usize) {
        assert_eq!(
            stmt_len, expected_len,
            "program.statements does not contain {} statements. got={}",
            expected_len, stmt_len
        );
    }

    fn test_node_type(node_type: NodeType, expected_node_type: NodeType) {
        assert_eq!(
            node_type, expected_node_type,
            "node is not of type {}. got={}",
            expected_node_type, node_type
        );
    }
}

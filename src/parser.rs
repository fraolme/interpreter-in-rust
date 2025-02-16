use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;
use std::{collections::HashMap, mem};

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
    Index = 8,
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
    prefix_parse_fns: HashMap<String, PrefixParseFn>,
    infix_parse_fns: HashMap<String, InfixParseFn>,
    precedence_map: HashMap<String, Precedence>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        let mut precedence_map: HashMap<String, Precedence> = HashMap::new();
        precedence_map.insert(Token::Eq.token_type(), Precedence::Equals);
        precedence_map.insert(Token::NotEq.token_type(), Precedence::Equals);
        precedence_map.insert(Token::LessThan.token_type(), Precedence::LessGreater);
        precedence_map.insert(Token::GreaterThan.token_type(), Precedence::LessGreater);
        precedence_map.insert(Token::Plus.token_type(), Precedence::Sum);
        precedence_map.insert(Token::Minus.token_type(), Precedence::Sum);
        precedence_map.insert(Token::Slash.token_type(), Precedence::Product);
        precedence_map.insert(Token::Asterisk.token_type(), Precedence::Product);
        precedence_map.insert(Token::Lparen.token_type(), Precedence::Call);
        precedence_map.insert(Token::Lbracket.token_type(), Precedence::Index);

        let mut prefix_parse_fns: HashMap<String, PrefixParseFn> = HashMap::new();
        prefix_parse_fns.insert(
            Token::Ident("".to_string()).token_type(),
            Parser::parse_identifier,
        );
        prefix_parse_fns.insert(
            Token::Int("".to_string()).token_type(),
            Parser::parse_integer_literal,
        );
        prefix_parse_fns.insert(Token::Bang.token_type(), Parser::parse_prefix_expression);
        prefix_parse_fns.insert(Token::Minus.token_type(), Parser::parse_prefix_expression);
        prefix_parse_fns.insert(Token::True.token_type(), Parser::parse_boolean);
        prefix_parse_fns.insert(Token::False.token_type(), Parser::parse_boolean);
        prefix_parse_fns.insert(Token::Lparen.token_type(), Parser::parse_grouped_expression);
        prefix_parse_fns.insert(Token::If.token_type(), Parser::parse_if_expression);
        prefix_parse_fns.insert(Token::Function.token_type(), Parser::parse_function_literal);
        prefix_parse_fns.insert(
            Token::String("".to_string()).token_type(),
            Parser::parse_string_literal,
        );
        prefix_parse_fns.insert(Token::Lbracket.token_type(), Parser::parse_array_literal);
        prefix_parse_fns.insert(Token::Lbrace.token_type(), Parser::parse_hash_literal);
        prefix_parse_fns.insert(Token::Macro.token_type(), Parser::parse_macro_literal);

        let mut infix_parse_fns: HashMap<String, InfixParseFn> = HashMap::new();
        infix_parse_fns.insert(Token::Plus.token_type(), Parser::parse_infix_expression);
        infix_parse_fns.insert(Token::Minus.token_type(), Parser::parse_infix_expression);
        infix_parse_fns.insert(Token::Slash.token_type(), Parser::parse_infix_expression);
        infix_parse_fns.insert(Token::Asterisk.token_type(), Parser::parse_infix_expression);
        infix_parse_fns.insert(Token::Eq.token_type(), Parser::parse_infix_expression);
        infix_parse_fns.insert(Token::NotEq.token_type(), Parser::parse_infix_expression);
        infix_parse_fns.insert(Token::LessThan.token_type(), Parser::parse_infix_expression);
        infix_parse_fns.insert(
            Token::GreaterThan.token_type(),
            Parser::parse_infix_expression,
        );
        infix_parse_fns.insert(Token::Lparen.token_type(), Parser::parse_call_expression);
        infix_parse_fns.insert(Token::Lbracket.token_type(), Parser::parse_index_expression);

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

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };
        while self.cur_token != Token::Eof {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let let_token = mem::take(&mut self.cur_token);
        if !self.expect_peek(&Token::Ident("".to_string())) {
            return None;
        }

        let identifier_value = self.cur_token.literal().to_string();
        let identifier_token = mem::take(&mut self.cur_token);

        let identifier = Expression::Ident(Identifier {
            token: identifier_token,
            value: identifier_value,
        });

        if !self.expect_peek(&Token::Assign) {
            return None;
        }

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::SemiColon) {
            self.next_token();
        }

        Some(Statement::Let(LetStatement {
            token: let_token,
            name: identifier,
            value,
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let return_token = mem::take(&mut self.cur_token);

        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::SemiColon) {
            self.next_token();
        }

        Some(Statement::Return(ReturnStatement {
            token: return_token,
            return_value,
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let cur_token = self.cur_token.clone();
        let expression = self.parse_expression(Precedence::Lowest)?;

        // semicolon is optional, like if 5 + 5 is typed to the REPL
        if self.peek_token_is(&Token::SemiColon) {
            self.next_token();
        }

        Some(Statement::Expression(ExpressionStatement {
            token: cur_token,
            expression,
        }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        if let Some(prefix_fn) = self.prefix_parse_fns.get(&self.cur_token.token_type()) {
            let mut left_exp = prefix_fn(self)?;

            while !self.peek_token_is(&Token::SemiColon) && precedence < self.peek_precedence() {
                let peek_token = self.peek_token.clone();
                if !self.infix_parse_fns.contains_key(&peek_token.token_type()) {
                    return Some(left_exp);
                }

                self.next_token();
                let infix_fn = self.infix_parse_fns.get(&peek_token.token_type()).unwrap();
                left_exp = infix_fn(self, left_exp)?;
            }

            Some(left_exp)
        } else {
            self.errors.push(format!(
                "no prefix parse function for {} found",
                &self.cur_token.token_type()
            ));

            None
        }
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        let identifier_value = self.cur_token.literal().to_string();
        let identifier_token = mem::take(&mut self.cur_token);

        Some(Expression::Ident(Identifier {
            token: identifier_token,
            value: identifier_value,
        }))
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let num_val: i64 = self.cur_token.literal().parse().ok()?;

        Some(Expression::Int(IntegerLiteral {
            token: mem::take(&mut self.cur_token),
            value: num_val,
        }))
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
        let val = self.cur_token_is(&Token::True);
        Some(Expression::Boolean(BooleanLiteral {
            token: mem::take(&mut self.cur_token),
            value: val,
        }))
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator = self.cur_token.literal().to_string();
        let cur_token = mem::take(&mut self.cur_token);
        self.next_token();
        let right_expr = self.parse_expression(Precedence::Prefix)?;

        Some(Expression::Prefix(Box::new(PrefixExpression {
            token: cur_token,
            operator,
            right: Box::new(right_expr),
        })))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let operator = self.cur_token.literal().to_string();
        let precedence = self.cur_precedence();
        let cur_token = mem::take(&mut self.cur_token);
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Some(Expression::Infix(Box::new(InfixExpression {
            token: cur_token,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&Token::Rparen) {
            return None;
        }

        Some(exp)
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let cur_token = mem::take(&mut self.cur_token);
        if !self.expect_peek(&Token::Lparen) {
            return None;
        }

        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&Token::Rparen) {
            return None;
        }

        if !self.expect_peek(&Token::Lbrace) {
            return None;
        }

        let consequence = self.parse_block_statement();
        let mut alternative: Option<Statement> = None;

        if self.peek_token_is(&Token::Else) {
            self.next_token();
            if !self.expect_peek(&Token::Lbrace) {
                return None;
            }

            alternative = Some(self.parse_block_statement());
        }

        Some(Expression::If(Box::new(IfExpression {
            token: cur_token,
            condition: Box::new(condition),
            consequence,
            alternative,
        })))
    }

    fn parse_block_statement(&mut self) -> Statement {
        let cur_token = mem::take(&mut self.cur_token);
        self.next_token();
        let mut stmts = vec![];
        while !self.cur_token_is(&Token::Rbrace) && !self.cur_token_is(&Token::Eof) {
            let stmt = self.parse_statement();
            if let Some(st) = stmt {
                stmts.push(st);
            }
            self.next_token();
        }

        Statement::Block(BlockStatement {
            token: cur_token,
            statements: stmts,
        })
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        let cur_token = mem::take(&mut self.cur_token);
        if !self.expect_peek(&Token::Lparen) {
            return None;
        }
        let params = self.parse_function_parameters()?;
        if !self.expect_peek(&Token::Lbrace) {
            return None;
        }

        let body = self.parse_block_statement();

        Some(Expression::Func(FunctionLiteral {
            token: cur_token,
            parameters: params,
            body: Box::new(body),
        }))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Expression>> {
        let mut identifiers: Vec<Expression> = vec![];

        if self.peek_token_is(&Token::Rparen) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let literal = self.cur_token.literal().to_string();
        let ident = Expression::Ident(Identifier {
            token: mem::take(&mut self.cur_token),
            value: literal,
        });
        identifiers.push(ident);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            let literal = self.cur_token.literal().to_string();
            let ident = Expression::Ident(Identifier {
                token: mem::take(&mut self.cur_token),
                value: literal,
            });
            identifiers.push(ident);
        }

        if !self.expect_peek(&Token::Rparen) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let cur_token = mem::take(&mut self.cur_token);
        let arguments = self.parse_expression_list(&Token::Rparen)?;

        Some(Expression::Call(Box::new(CallExpression {
            token: cur_token,
            function: Box::new(function),
            arguments,
        })))
    }

    fn parse_string_literal(&mut self) -> Option<Expression> {
        let cur_token = mem::take(&mut self.cur_token);
        let value = cur_token.literal().to_string();
        Some(Expression::String(StringLiteral {
            token: cur_token,
            value,
        }))
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        let cur_token = mem::take(&mut self.cur_token);
        let elements = self.parse_expression_list(&Token::Rbracket)?;

        Some(Expression::Array(ArrayLiteral {
            token: cur_token,
            elements,
        }))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        let cur_token = mem::take(&mut self.cur_token);
        self.next_token();
        let index = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&Token::Rbracket) {
            return None;
        }

        Some(Expression::Index(Box::new(IndexExpression {
            token: cur_token,
            left: Box::new(left),
            index: Box::new(index),
        })))
    }

    fn parse_hash_literal(&mut self) -> Option<Expression> {
        let cur_token = mem::take(&mut self.cur_token);
        let mut map = HashMap::new();

        while !self.peek_token_is(&Token::Rbrace) {
            self.next_token();
            let key = self.parse_expression(Precedence::Lowest)?;
            if !self.expect_peek(&Token::Colon) {
                return None;
            }
            self.next_token();
            let val = self.parse_expression(Precedence::Lowest)?;
            map.insert(key, val);

            if !self.peek_token_is(&Token::Rbrace) && !self.expect_peek(&Token::Comma) {
                return None;
            }
        }

        if !self.expect_peek(&Token::Rbrace) {
            return None;
        }

        Some(Expression::Hash(HashLiteral {
            token: cur_token,
            pairs: map,
        }))
    }

    fn parse_macro_literal(&mut self) -> Option<Expression> {
        let cur_token = mem::take(&mut self.cur_token);
        if !self.expect_peek(&Token::Lparen) {
            return None;
        }
        let parameters = self.parse_expression_list(&Token::Rparen)?;

        if !self.expect_peek(&Token::Lbrace) {
            return None;
        }
        let body = self.parse_block_statement();

        Some(Expression::Macro(MacroLiteral {
            token: cur_token,
            parameters,
            body: Box::new(body),
        }))
    }

    // helper functions
    fn parse_expression_list(&mut self, end: &Token) -> Option<Vec<Expression>> {
        if self.peek_token_is(end) {
            self.next_token();
            return Some(vec![]);
        }

        self.next_token();
        let mut list = vec![];
        let item = self.parse_expression(Precedence::Lowest)?;
        list.push(item);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();

            let item = self.parse_expression(Precedence::Lowest)?;
            list.push(item);
        }

        if !self.expect_peek(end) {
            return None;
        }

        Some(list)
    }

    fn cur_token_is(&self, token: &Token) -> bool {
        std::mem::discriminant(&self.cur_token) == std::mem::discriminant(token)
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        std::mem::discriminant(&self.peek_token) == std::mem::discriminant(token)
    }

    fn expect_peek(&mut self, token: &Token) -> bool {
        if self.peek_token_is(token) {
            self.next_token();
            true
        } else {
            self.peek_error(token);
            false
        }
    }

    fn peek_error(&mut self, token: &Token) {
        let message = format!(
            "expected next token to be {}, got {} instead",
            token.token_type(),
            self.peek_token.token_type()
        );
        self.errors.push(message);
    }

    fn peek_precedence(&self) -> Precedence {
        if let Some(precedence) = self.precedence_map.get(&self.peek_token.token_type()) {
            *precedence
        } else {
            Precedence::Lowest
        }
    }

    fn cur_precedence(&self) -> Precedence {
        if let Some(precedence) = self.precedence_map.get(&self.cur_token.token_type()) {
            *precedence
        } else {
            Precedence::Lowest
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use std::fmt::Write;

    enum Expected {
        Int64(i64),
        Str(String),
        Boolean(bool),
    }

    #[test]
    fn test_let_statements() {
        let tests: Vec<(&str, &str, Expected)> = vec![
            ("let x = 5;", "x", Expected::Int64(5)),
            ("let y = true;", "y", Expected::Boolean(true)),
            ("let foobar = y;", "foobar", Expected::Str("y".to_string())),
        ];

        for (input, expected_ident, expected_val) in tests {
            let lexer = Lexer::new(String::from(input));
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            test_statements_len(program.statements.len(), 1);
            test_let_statement(&program.statements[0], expected_ident, expected_val);
        }
    }

    #[test]
    fn test_return_statements() {
        let tests: Vec<(&str, Expected)> = vec![
            ("return 5;", Expected::Int64(5)),
            ("return true;", Expected::Boolean(true)),
            ("return y;", Expected::Str("y".to_string())),
        ];

        for (input, expected_val) in tests {
            let lexer = Lexer::new(String::from(input));
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            test_statements_len(program.statements.len(), 1);
            test_node_type(program.statements[0].node_type(), NodeType::Return);
            if let Statement::Return(ret_stmt) = &program.statements[0] {
                test_literal_expression(&ret_stmt.return_value, expected_val);
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
        let prefix_tests: Vec<(&str, &str, Expected)> = vec![
            ("!5;", "!", Expected::Int64(5)),
            ("-15;", "-", Expected::Int64(15)),
            ("!true;", "!", Expected::Boolean(true)),
            ("!false;", "!", Expected::Boolean(false)),
        ];

        for (input, operator, expected) in prefix_tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            test_statements_len(program.statements.len(), 1);
            test_node_type(
                program.statements[0].node_type(),
                NodeType::ExpressionStatement,
            );

            if let Statement::Expression(expr_stmt) = &program.statements[0] {
                test_node_type(expr_stmt.expression.node_type(), NodeType::PrefixExpression);

                if let Expression::Prefix(exp) = &expr_stmt.expression {
                    assert_eq!(
                        exp.operator, operator,
                        "exp.operator is not {}. got={}",
                        operator, exp.operator
                    );

                    test_literal_expression(exp.right.as_ref(), expected);
                }
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests: Vec<(&str, Expected, &str, Expected)> = vec![
            ("5+5;", Expected::Int64(5), "+", Expected::Int64(5)),
            ("5-5;", Expected::Int64(5), "-", Expected::Int64(5)),
            ("5*5;", Expected::Int64(5), "*", Expected::Int64(5)),
            ("5/5;", Expected::Int64(5), "/", Expected::Int64(5)),
            ("5>5;", Expected::Int64(5), ">", Expected::Int64(5)),
            ("5<5;", Expected::Int64(5), "<", Expected::Int64(5)),
            ("5==5;", Expected::Int64(5), "==", Expected::Int64(5)),
            ("5!=5;", Expected::Int64(5), "!=", Expected::Int64(5)),
            (
                "true == true",
                Expected::Boolean(true),
                "==",
                Expected::Boolean(true),
            ),
            (
                "true != false",
                Expected::Boolean(true),
                "!=",
                Expected::Boolean(false),
            ),
            (
                "false == false",
                Expected::Boolean(false),
                "==",
                Expected::Boolean(false),
            ),
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

            if let Statement::Expression(expr_stmt) = &program.statements[0] {
                test_infix_expression(&expr_stmt.expression, left_val, operator, right_val);
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
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1,2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
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

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        test_statements_len(program.statements.len(), 1);
        test_node_type(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
        );
        if let Statement::Expression(expr_stmt) = &program.statements[0] {
            test_node_type(expr_stmt.expression.node_type(), NodeType::IfExpression);

            if let Expression::If(if_expr) = &expr_stmt.expression {
                test_infix_expression(
                    &if_expr.condition,
                    Expected::Str("x".to_string()),
                    "<",
                    Expected::Str("y".to_string()),
                );

                if let Statement::Block(block_stmt) = &if_expr.consequence {
                    test_statements_len(block_stmt.statements.len(), 1);
                    test_node_type(
                        block_stmt.statements[0].node_type(),
                        NodeType::ExpressionStatement,
                    );

                    if let Statement::Expression(consq) = &block_stmt.statements[0] {
                        test_node_type(consq.expression.node_type(), NodeType::Identifier);
                        test_identifier(&consq.expression, "x");
                    }
                }

                assert!(
                    if_expr.alternative.is_none(),
                    "if_expr.alternative was not None. got={}",
                    if_expr.alternative.as_ref().unwrap()
                );
            }
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        test_statements_len(program.statements.len(), 1);
        test_node_type(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
        );
        if let Statement::Expression(expr_stmt) = &program.statements[0] {
            test_node_type(expr_stmt.expression.node_type(), NodeType::IfExpression);

            if let Expression::If(if_expr) = &expr_stmt.expression {
                test_infix_expression(
                    &if_expr.condition,
                    Expected::Str("x".to_string()),
                    "<",
                    Expected::Str("y".to_string()),
                );

                if let Statement::Block(block) = &if_expr.consequence {
                    test_statements_len(block.statements.len(), 1);
                    test_node_type(
                        block.statements[0].node_type(),
                        NodeType::ExpressionStatement,
                    );

                    if let Statement::Expression(consq) = &block.statements[0] {
                        test_node_type(consq.expression.node_type(), NodeType::Identifier);
                        test_identifier(&consq.expression, "x");
                    }
                }

                assert!(
                    if_expr.alternative.is_some(),
                    "if_expr.alternative is not Some. got=None"
                );

                if let Statement::Block(alternative) = if_expr.alternative.as_ref().unwrap() {
                    test_statements_len(alternative.statements.len(), 1);
                    test_node_type(
                        alternative.statements[0].node_type(),
                        NodeType::ExpressionStatement,
                    );

                    if let Statement::Expression(alt) = &alternative.statements[0] {
                        test_node_type(alt.expression.node_type(), NodeType::Identifier);
                        test_identifier(&alt.expression, "y");
                    }
                }
            }
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        test_statements_len(program.statements.len(), 1);
        test_node_type(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
        );

        if let Statement::Expression(stmt) = &program.statements[0] {
            test_node_type(stmt.expression.node_type(), NodeType::FunctionLiteral);

            if let Expression::Func(func) = &stmt.expression {
                test_statements_len(func.parameters.len(), 2);

                assert_eq!(func.parameters[0].token_literal(), "x");
                assert_eq!(func.parameters[1].token_literal(), "y");

                if let Statement::Block(block) = &*func.body {
                    test_statements_len(block.statements.len(), 1);
                    test_node_type(
                        block.statements[0].node_type(),
                        NodeType::ExpressionStatement,
                    );

                    if let Statement::Expression(expr) = &block.statements[0] {
                        test_infix_expression(
                            &expr.expression,
                            Expected::Str("x".to_string()),
                            "+",
                            Expected::Str("y".to_string()),
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        test_statements_len(program.statements.len(), 1);
        test_node_type(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
        );

        if let Statement::Expression(stmt) = &program.statements[0] {
            test_node_type(stmt.expression.node_type(), NodeType::CallExpression);

            if let Expression::Call(call) = &stmt.expression {
                test_identifier(&call.function, "add");

                test_statements_len(call.arguments.len(), 3);
                test_literal_expression(&call.arguments[0], Expected::Int64(1));
                test_infix_expression(
                    &call.arguments[1],
                    Expected::Int64(2),
                    "*",
                    Expected::Int64(3),
                );
                test_infix_expression(
                    &call.arguments[2],
                    Expected::Int64(4),
                    "+",
                    Expected::Int64(5),
                );
            }
        }
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#" "Hello world"; "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        test_node_type(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
        );
        if let Statement::Expression(stmt) = &program.statements[0] {
            test_node_type(stmt.expression.node_type(), NodeType::StringLiteral);
            if let Expression::String(sl) = &stmt.expression {
                assert_eq!(
                    sl.value, "Hello world",
                    "value not {}. got={}",
                    "Hello world", sl.value
                );
            }
        }
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        test_node_type(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
        );
        if let Statement::Expression(stmt) = &program.statements[0] {
            test_node_type(stmt.expression.node_type(), NodeType::ArrayLiteral);
            if let Expression::Array(arr_lit) = &stmt.expression {
                assert_eq!(
                    arr_lit.elements.len(),
                    3,
                    "array.elements.len() not 3. got={}",
                    arr_lit.elements.len()
                );
                test_integer_literal(&arr_lit.elements[0], 1);
                test_infix_expression(
                    &arr_lit.elements[1],
                    Expected::Int64(2),
                    "*",
                    Expected::Int64(2),
                );
                test_infix_expression(
                    &arr_lit.elements[2],
                    Expected::Int64(3),
                    "+",
                    Expected::Int64(3),
                );
            }
        }
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "myArray[1 + 1]";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        test_node_type(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
        );
        if let Statement::Expression(stmt) = &program.statements[0] {
            test_node_type(stmt.expression.node_type(), NodeType::IndexExpression);
            if let Expression::Index(ind_exp) = &stmt.expression {
                test_identifier(&ind_exp.left, "myArray");
                test_infix_expression(&ind_exp.index, Expected::Int64(1), "+", Expected::Int64(1));
            }
        }
    }

    #[test]
    fn test_parsing_hash_literals_string_keys() {
        let input = r#"{"one": 1, "two": 2, "three": 3}"#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        test_node_type(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
        );
        if let Statement::Expression(stmt) = &program.statements[0] {
            test_node_type(stmt.expression.node_type(), NodeType::HashLiteral);
            if let Expression::Hash(hash) = &stmt.expression {
                assert_eq!(
                    hash.pairs.len(),
                    3,
                    "hash.pairs has wrong length. got={}",
                    hash.pairs.len()
                );

                let mut expected = HashMap::new();
                expected.insert("one".to_string(), 1);
                expected.insert("two".to_string(), 2);
                expected.insert("three".to_string(), 3);

                for (key, value) in &hash.pairs {
                    if let Expression::String(lit) = key {
                        test_integer_literal(value, *expected.get(&lit.to_string()).unwrap());
                    } else {
                        panic!("key is not ast::StringLiteral. got={}", key);
                    }
                }
            }
        }
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input = "{}";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        test_node_type(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
        );
        if let Statement::Expression(stmt) = &program.statements[0] {
            test_node_type(stmt.expression.node_type(), NodeType::HashLiteral);
            if let Expression::Hash(hash) = &stmt.expression {
                assert_eq!(
                    hash.pairs.len(),
                    0,
                    "hash.pairs has wrong length. got={}",
                    hash.pairs.len()
                );
            }
        }
    }

    #[test]
    fn test_parsing_hash_literals_with_expressions() {
        let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        test_node_type(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
        );
        if let Statement::Expression(stmt) = &program.statements[0] {
            test_node_type(stmt.expression.node_type(), NodeType::HashLiteral);
            if let Expression::Hash(hash) = &stmt.expression {
                assert_eq!(
                    hash.pairs.len(),
                    3,
                    "hash.pairs has wrong length. got={}",
                    hash.pairs.len()
                );

                type TestHashPairs = HashMap<String, Box<dyn Fn(&Expression)>>;   
                let mut tests: TestHashPairs = HashMap::new();
                tests.insert(
                    "one".to_string(),
                    Box::new(|e: &Expression| {
                        test_infix_expression(e, Expected::Int64(0), "+", Expected::Int64(1));
                    }),
                );
                tests.insert(
                    "two".to_string(),
                    Box::new(|e: &Expression| {
                        test_infix_expression(e, Expected::Int64(10), "-", Expected::Int64(8));
                    }),
                );
                tests.insert(
                    "three".to_string(),
                    Box::new(|e: &Expression| {
                        test_infix_expression(e, Expected::Int64(15), "/", Expected::Int64(5));
                    }),
                );

                for (key, value) in &hash.pairs {
                    if let Expression::String(lit) = key {
                        let test_func = tests.get(&lit.to_string()).unwrap();
                        test_func(value);
                    } else {
                        panic!("key is not ast::StringLiteral. got={}", key);
                    }
                }
            }
        }
    }

    #[test]
    fn test_macro_literal_parsing() {
        let input = "macro(x, y) { x + y; }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        test_statements_len(program.statements.len(), 1);

        test_node_type(
            program.statements[0].node_type(),
            NodeType::ExpressionStatement,
        );

        if let Statement::Expression(stmt) = &program.statements[0] {
            test_node_type(stmt.expression.node_type(), NodeType::MacroLiteral);

            if let Expression::Macro(lit) = &stmt.expression {
                assert_eq!(
                    lit.parameters.len(),
                    2,
                    "macro literal parameters wrong. want=2, got={}",
                    lit.parameters.len()
                );

                test_literal_expression(&lit.parameters[0], Expected::Str("x".to_string()));
                test_literal_expression(&lit.parameters[1], Expected::Str("y".to_string()));

                test_node_type(lit.body.node_type(), NodeType::BlockStatement);

                if let Statement::Block(block) = &*lit.body {
                    test_statements_len(block.statements.len(), 1);
                    test_node_type(
                        block.statements[0].node_type(),
                        NodeType::ExpressionStatement,
                    );
                    if let Statement::Expression(body_stmt) = &block.statements[0] {
                        test_infix_expression(
                            &body_stmt.expression,
                            Expected::Str("x".to_string()),
                            "+",
                            Expected::Str("y".to_string()),
                        );
                    }
                }
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

        writeln!(&mut buffer, "parser has {} errors ", error_len).unwrap();
        for msg in &parser.errors {
            writeln!(&mut buffer, "parser error: {} ", msg).unwrap();
        }

        assert_eq!(error_len, 0, "{}", buffer);
    }

    fn test_integer_literal(int_exp: &Expression, val: i64) {
        test_node_type(int_exp.node_type(), NodeType::IntegerLiteral);

        if let Expression::Int(integ) = int_exp {
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

    fn test_boolean_literal(bol_exp: &Expression, val: bool) {
        test_node_type(bol_exp.node_type(), NodeType::BooleanLiteral);

        if let Expression::Boolean(bol) = bol_exp {
            assert_eq!(bol.value, val, "bol.value not {}. got={}", val, bol.value);

            assert_eq!(
                bol.token_literal(),
                val.to_string(),
                "bol.token_literal() not {}. got={}",
                val,
                bol.token_literal()
            );
        }
    }

    fn test_identifier(exp: &Expression, val: &str) {
        test_node_type(exp.node_type(), NodeType::Identifier);
        if let Expression::Ident(ident) = exp {
            assert_eq!(
                ident.value, val,
                "ident.value not {}. got={}",
                val, ident.value
            );
            assert_eq!(
                ident.token_literal(),
                val,
                "ident.token_literal() not {}. got={}",
                val,
                ident.token_literal()
            );
        }
    }

    fn test_literal_expression(exp: &Expression, expected: Expected) {
        match expected {
            Expected::Int64(val) => test_integer_literal(exp, val),
            Expected::Str(val) => test_identifier(exp, &val),
            Expected::Boolean(val) => test_boolean_literal(exp, val),
        }
    }

    fn test_infix_expression(exp: &Expression, left: Expected, op: &str, right: Expected) {
        test_node_type(exp.node_type(), NodeType::InfixExpression);
        if let Expression::Infix(expr) = exp {
            test_literal_expression(&expr.left, left);
            assert_eq!(
                expr.operator, op,
                "exp.operatir is not {}. got={}",
                op, expr.operator
            );
            test_literal_expression(&expr.right, right);
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

    fn test_let_statement(stmt: &Statement, name: &str, value: Expected) {
        test_node_type(stmt.node_type(), NodeType::Let);
        if let Statement::Let(let_stmt) = stmt {
            if let Expression::Ident(ident) = &let_stmt.name {
                assert_eq!(
                    ident.value, name,
                    "let_stmt.name.value not {}. got={}",
                    name, ident.value
                );

                assert_eq!(
                    ident.token_literal(),
                    name,
                    "let_stmt.name.token_literal() not {} got={}",
                    name,
                    ident.token_literal()
                );
            }

            test_literal_expression(&let_stmt.value, value);
        }
    }
}

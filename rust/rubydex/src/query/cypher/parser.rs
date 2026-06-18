use super::ast::{
    AggFn, CmpOp, Direction, Expr, Literal, NodePattern, OrderItem, PathPattern, Query, RelPattern, Return, ReturnItem,
    VarLength,
};
use super::error::CypherError;
use super::lexer::{Token, TokenKind, tokenize};

/// Parses a Cypher query string into a [`Query`] AST.
///
/// # Errors
///
/// Returns a [`CypherError::Syntax`] on any lexical or grammatical error.
pub fn parse(input: &str) -> Result<Query, CypherError> {
    let tokens = tokenize(input)?;
    let mut parser = Parser {
        tokens,
        position: 0,
        source_len: input.len(),
    };
    let query = parser.parse_query()?;
    if let Some(token) = parser.peek() {
        return Err(CypherError::syntax("unexpected trailing input", token.position));
    }
    Ok(query)
}

struct Parser {
    tokens: Vec<Token>,
    position: usize,
    source_len: usize,
}

impl Parser {
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    fn peek_kind(&self) -> Option<&TokenKind> {
        self.tokens.get(self.position).map(|t| &t.kind)
    }

    fn current_position(&self) -> usize {
        self.tokens
            .get(self.position)
            .map_or(self.source_len, |token| token.position)
    }

    fn advance(&mut self) -> Option<Token> {
        let token = self.tokens.get(self.position).cloned();
        if token.is_some() {
            self.position += 1;
        }
        token
    }

    fn expect(&mut self, kind: &TokenKind, description: &str) -> Result<(), CypherError> {
        match self.peek_kind() {
            Some(actual) if actual == kind => {
                self.position += 1;
                Ok(())
            }
            _ => Err(CypherError::syntax(
                format!("expected {description}"),
                self.current_position(),
            )),
        }
    }

    fn at_keyword(&self, keyword: &str) -> bool {
        matches!(self.peek_kind(), Some(TokenKind::Ident(name)) if name.eq_ignore_ascii_case(keyword))
    }

    fn eat_keyword(&mut self, keyword: &str) -> bool {
        if self.at_keyword(keyword) {
            self.position += 1;
            true
        } else {
            false
        }
    }

    fn expect_keyword(&mut self, keyword: &str) -> Result<(), CypherError> {
        if self.eat_keyword(keyword) {
            Ok(())
        } else {
            Err(CypherError::syntax(
                format!("expected keyword `{keyword}`"),
                self.current_position(),
            ))
        }
    }

    fn expect_ident(&mut self, description: &str) -> Result<String, CypherError> {
        match self.advance() {
            Some(Token {
                kind: TokenKind::Ident(name),
                ..
            }) => Ok(name),
            _ => Err(CypherError::syntax(
                format!("expected {description}"),
                self.current_position(),
            )),
        }
    }

    fn parse_query(&mut self) -> Result<Query, CypherError> {
        self.expect_keyword("MATCH")?;
        let patterns = self.parse_patterns()?;

        let where_clause = if self.eat_keyword("WHERE") {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.expect_keyword("RETURN")?;
        let return_clause = self.parse_return()?;

        let mut order_by = Vec::new();
        if self.eat_keyword("ORDER") {
            self.expect_keyword("BY")?;
            order_by = self.parse_order_by()?;
        }

        let skip = if self.eat_keyword("SKIP") {
            Some(self.parse_usize()?)
        } else {
            None
        };

        let limit = if self.eat_keyword("LIMIT") {
            Some(self.parse_usize()?)
        } else {
            None
        };

        Ok(Query {
            patterns,
            where_clause,
            return_clause,
            order_by,
            skip,
            limit,
        })
    }

    fn parse_usize(&mut self) -> Result<usize, CypherError> {
        match self.advance() {
            Some(Token {
                kind: TokenKind::Int(value),
                position,
            }) => usize::try_from(value).map_err(|_| CypherError::syntax("expected a non-negative integer", position)),
            _ => Err(CypherError::syntax("expected an integer", self.current_position())),
        }
    }

    fn parse_patterns(&mut self) -> Result<Vec<PathPattern>, CypherError> {
        let mut patterns = vec![self.parse_path_pattern()?];
        while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
            self.position += 1;
            patterns.push(self.parse_path_pattern()?);
        }
        Ok(patterns)
    }

    fn parse_path_pattern(&mut self) -> Result<PathPattern, CypherError> {
        let start = self.parse_node_pattern()?;
        let mut rest = Vec::new();
        while self.at_relationship_start() {
            let rel = self.parse_rel_pattern()?;
            let node = self.parse_node_pattern()?;
            rest.push((rel, node));
        }
        Ok(PathPattern { start, rest })
    }

    fn at_relationship_start(&self) -> bool {
        matches!(self.peek_kind(), Some(TokenKind::Minus | TokenKind::Lt))
    }

    fn parse_node_pattern(&mut self) -> Result<NodePattern, CypherError> {
        self.expect(&TokenKind::LParen, "`(` to start a node pattern")?;

        let var = match self.peek_kind() {
            Some(TokenKind::Ident(name)) => {
                let name = name.clone();
                self.position += 1;
                Some(name)
            }
            _ => None,
        };

        let mut labels = Vec::new();
        if matches!(self.peek_kind(), Some(TokenKind::Colon)) {
            self.position += 1;
            labels.push(self.expect_ident("a node label after `:`")?);
            while matches!(self.peek_kind(), Some(TokenKind::Pipe)) {
                self.position += 1;
                labels.push(self.expect_ident("a node label after `|`")?);
            }
        }

        let props = if matches!(self.peek_kind(), Some(TokenKind::LBrace)) {
            self.parse_prop_map()?
        } else {
            Vec::new()
        };

        self.expect(&TokenKind::RParen, "`)` to close a node pattern")?;

        Ok(NodePattern { var, labels, props })
    }

    fn parse_prop_map(&mut self) -> Result<Vec<(String, Literal)>, CypherError> {
        self.expect(&TokenKind::LBrace, "`{`")?;
        let mut props = Vec::new();

        if !matches!(self.peek_kind(), Some(TokenKind::RBrace)) {
            loop {
                let key = self.expect_ident("a property name")?;
                self.expect(&TokenKind::Colon, "`:` after property name")?;
                let value = self.parse_literal()?;
                props.push((key, value));

                if matches!(self.peek_kind(), Some(TokenKind::Comma)) {
                    self.position += 1;
                } else {
                    break;
                }
            }
        }

        self.expect(&TokenKind::RBrace, "`}` to close a property map")?;
        Ok(props)
    }

    fn parse_rel_pattern(&mut self) -> Result<RelPattern, CypherError> {
        let leading_in = matches!(self.peek_kind(), Some(TokenKind::Lt));
        if leading_in {
            self.position += 1;
        }
        self.expect(&TokenKind::Minus, "`-` in relationship pattern")?;

        let mut var = None;
        let mut types = Vec::new();
        let mut length = None;

        if matches!(self.peek_kind(), Some(TokenKind::LBracket)) {
            self.position += 1;

            if let Some(TokenKind::Ident(name)) = self.peek_kind() {
                var = Some(name.clone());
                self.position += 1;
            }

            if matches!(self.peek_kind(), Some(TokenKind::Colon)) {
                self.position += 1;
                types.push(self.expect_ident("a relationship type after `:`")?);
                while matches!(self.peek_kind(), Some(TokenKind::Pipe)) {
                    self.position += 1;
                    types.push(self.expect_ident("a relationship type after `|`")?);
                }
            }

            if matches!(self.peek_kind(), Some(TokenKind::Star)) {
                self.position += 1;
                length = Some(self.parse_var_length()?);
            }

            self.expect(&TokenKind::RBracket, "`]` to close a relationship pattern")?;
        }

        self.expect(&TokenKind::Minus, "`-` in relationship pattern")?;
        let trailing_out = matches!(self.peek_kind(), Some(TokenKind::Gt));
        if trailing_out {
            self.position += 1;
        }

        let direction = match (leading_in, trailing_out) {
            (true, false) => Direction::Incoming,
            (false, true) => Direction::Outgoing,
            (false, false) => Direction::Both,
            (true, true) => {
                return Err(CypherError::syntax(
                    "a relationship cannot point in both directions",
                    self.current_position(),
                ));
            }
        };

        Ok(RelPattern {
            var,
            types,
            direction,
            length,
        })
    }

    fn parse_var_length(&mut self) -> Result<VarLength, CypherError> {
        let mut min = 1;
        let mut max = None;

        if let Some(TokenKind::Int(value)) = self.peek_kind() {
            let lower = u32::try_from(*value)
                .map_err(|_| CypherError::syntax("variable-length bound is too large", self.current_position()))?;
            self.position += 1;

            if matches!(self.peek_kind(), Some(TokenKind::DotDot)) {
                self.position += 1;
                min = lower;
                max = self.parse_optional_length_bound()?;
            } else {
                // `*n` means exactly n hops.
                min = lower;
                max = Some(lower);
            }
        } else if matches!(self.peek_kind(), Some(TokenKind::DotDot)) {
            self.position += 1;
            max = self.parse_optional_length_bound()?;
        }

        Ok(VarLength { min, max })
    }

    fn parse_optional_length_bound(&mut self) -> Result<Option<u32>, CypherError> {
        if let Some(TokenKind::Int(value)) = self.peek_kind() {
            let upper = u32::try_from(*value)
                .map_err(|_| CypherError::syntax("variable-length bound is too large", self.current_position()))?;
            self.position += 1;
            Ok(Some(upper))
        } else {
            Ok(None)
        }
    }

    fn parse_return(&mut self) -> Result<Return, CypherError> {
        let distinct = self.eat_keyword("DISTINCT");
        let mut items = vec![self.parse_return_item()?];
        while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
            self.position += 1;
            items.push(self.parse_return_item()?);
        }
        Ok(Return { distinct, items })
    }

    fn parse_return_item(&mut self) -> Result<ReturnItem, CypherError> {
        let expr = self.parse_expr()?;
        let alias = if self.eat_keyword("AS") {
            Some(self.expect_ident("an alias after `AS`")?)
        } else {
            None
        };
        Ok(ReturnItem { expr, alias })
    }

    fn parse_order_by(&mut self) -> Result<Vec<OrderItem>, CypherError> {
        let mut items = vec![self.parse_order_item()?];
        while matches!(self.peek_kind(), Some(TokenKind::Comma)) {
            self.position += 1;
            items.push(self.parse_order_item()?);
        }
        Ok(items)
    }

    fn parse_order_item(&mut self) -> Result<OrderItem, CypherError> {
        let expr = self.parse_expr()?;
        let descending = if self.eat_keyword("DESC") {
            true
        } else {
            // ASC is the default and optional.
            let _ = self.eat_keyword("ASC");
            false
        };
        Ok(OrderItem { expr, descending })
    }

    fn parse_expr(&mut self) -> Result<Expr, CypherError> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr, CypherError> {
        let mut left = self.parse_and()?;
        while self.eat_keyword("OR") {
            let right = self.parse_and()?;
            left = Expr::Or(Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_and(&mut self) -> Result<Expr, CypherError> {
        let mut left = self.parse_not()?;
        while self.eat_keyword("AND") {
            let right = self.parse_not()?;
            left = Expr::And(Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_not(&mut self) -> Result<Expr, CypherError> {
        if self.eat_keyword("NOT") {
            let inner = self.parse_not()?;
            Ok(Expr::Not(Box::new(inner)))
        } else {
            self.parse_comparison()
        }
    }

    fn parse_comparison(&mut self) -> Result<Expr, CypherError> {
        let left = self.parse_primary()?;
        if let Some(op) = self.parse_comparison_op()? {
            let right = self.parse_primary()?;
            Ok(Expr::Compare(Box::new(left), op, Box::new(right)))
        } else {
            Ok(left)
        }
    }

    fn parse_comparison_op(&mut self) -> Result<Option<CmpOp>, CypherError> {
        let op = match self.peek_kind() {
            Some(TokenKind::Eq) => CmpOp::Eq,
            Some(TokenKind::Neq) => CmpOp::Neq,
            Some(TokenKind::Lt) => CmpOp::Lt,
            Some(TokenKind::Lte) => CmpOp::Lte,
            Some(TokenKind::Gt) => CmpOp::Gt,
            Some(TokenKind::Gte) => CmpOp::Gte,
            Some(TokenKind::Ident(name)) if name.eq_ignore_ascii_case("CONTAINS") => {
                self.position += 1;
                return Ok(Some(CmpOp::Contains));
            }
            Some(TokenKind::Ident(name)) if name.eq_ignore_ascii_case("STARTS") => {
                self.position += 1;
                self.expect_keyword("WITH")?;
                return Ok(Some(CmpOp::StartsWith));
            }
            Some(TokenKind::Ident(name)) if name.eq_ignore_ascii_case("ENDS") => {
                self.position += 1;
                self.expect_keyword("WITH")?;
                return Ok(Some(CmpOp::EndsWith));
            }
            _ => return Ok(None),
        };
        self.position += 1;
        Ok(Some(op))
    }

    fn parse_primary(&mut self) -> Result<Expr, CypherError> {
        match self.peek_kind() {
            Some(TokenKind::LParen) => {
                self.position += 1;
                let expr = self.parse_or()?;
                self.expect(&TokenKind::RParen, "`)` to close a grouped expression")?;
                Ok(expr)
            }
            Some(TokenKind::Str(_) | TokenKind::Int(_)) => Ok(Expr::Literal(self.parse_literal()?)),
            Some(TokenKind::Ident(name)) => {
                let name = name.clone();
                if name.eq_ignore_ascii_case("TRUE")
                    || name.eq_ignore_ascii_case("FALSE")
                    || name.eq_ignore_ascii_case("NULL")
                {
                    return Ok(Expr::Literal(self.parse_literal()?));
                }
                if let Some(func) = aggregate_function(&name)
                    && matches!(
                        self.tokens.get(self.position + 1).map(|t| &t.kind),
                        Some(TokenKind::LParen)
                    )
                {
                    return self.parse_aggregate(func);
                }
                self.position += 1;
                if matches!(self.peek_kind(), Some(TokenKind::Dot)) {
                    self.position += 1;
                    let prop = self.expect_ident("a property name after `.`")?;
                    Ok(Expr::Property(name, prop))
                } else {
                    Ok(Expr::Var(name))
                }
            }
            _ => Err(CypherError::syntax("expected an expression", self.current_position())),
        }
    }

    fn parse_aggregate(&mut self, func: AggFn) -> Result<Expr, CypherError> {
        self.position += 1; // function name
        self.expect(&TokenKind::LParen, "`(` after aggregate function")?;
        let distinct = self.eat_keyword("DISTINCT");

        let arg = if matches!(self.peek_kind(), Some(TokenKind::Star)) {
            if func != AggFn::Count {
                return Err(CypherError::syntax(
                    "only count(*) may use `*`",
                    self.current_position(),
                ));
            }
            self.position += 1;
            None
        } else {
            Some(Box::new(self.parse_or()?))
        };

        self.expect(&TokenKind::RParen, "`)` to close aggregate function")?;
        Ok(Expr::Aggregate { func, arg, distinct })
    }

    fn parse_literal(&mut self) -> Result<Literal, CypherError> {
        match self.advance() {
            Some(Token {
                kind: TokenKind::Str(value),
                ..
            }) => Ok(Literal::Str(value)),
            Some(Token {
                kind: TokenKind::Int(value),
                ..
            }) => Ok(Literal::Int(value)),
            Some(Token {
                kind: TokenKind::Ident(name),
                position,
            }) => {
                if name.eq_ignore_ascii_case("true") {
                    Ok(Literal::Bool(true))
                } else if name.eq_ignore_ascii_case("false") {
                    Ok(Literal::Bool(false))
                } else if name.eq_ignore_ascii_case("null") {
                    Ok(Literal::Null)
                } else {
                    Err(CypherError::syntax(
                        format!("expected a literal, found `{name}`"),
                        position,
                    ))
                }
            }
            _ => Err(CypherError::syntax("expected a literal value", self.current_position())),
        }
    }
}

fn aggregate_function(name: &str) -> Option<AggFn> {
    match name.to_ascii_lowercase().as_str() {
        "count" => Some(AggFn::Count),
        "collect" => Some(AggFn::Collect),
        "min" => Some(AggFn::Min),
        "max" => Some(AggFn::Max),
        "sum" => Some(AggFn::Sum),
        "avg" => Some(AggFn::Avg),
        _ => None,
    }
}

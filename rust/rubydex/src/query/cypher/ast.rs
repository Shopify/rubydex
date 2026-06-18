//! Abstract syntax tree for the supported subset of Cypher.

/// A complete parsed query.
#[derive(Debug, Clone, PartialEq)]
pub struct Query {
    /// One or more comma-separated path patterns from the `MATCH` clause.
    pub patterns: Vec<PathPattern>,
    pub where_clause: Option<Expr>,
    pub return_clause: Return,
    pub order_by: Vec<OrderItem>,
    pub skip: Option<usize>,
    pub limit: Option<usize>,
}

/// A path pattern: a starting node followed by zero or more relationship/node hops.
#[derive(Debug, Clone, PartialEq)]
pub struct PathPattern {
    pub start: NodePattern,
    pub rest: Vec<(RelPattern, NodePattern)>,
}

/// A node pattern such as `(c:Class {name: 'Foo'})` or `(c:Class|Module)`.
#[derive(Debug, Clone, PartialEq)]
pub struct NodePattern {
    pub var: Option<String>,
    /// Labels in a disjunction: a node matches if it has **any** of these labels. Empty means
    /// "any node".
    pub labels: Vec<String>,
    pub props: Vec<(String, Literal)>,
}

/// A relationship pattern such as `-[:INHERITS*1..3]->`.
#[derive(Debug, Clone, PartialEq)]
pub struct RelPattern {
    pub var: Option<String>,
    /// Relationship types; empty means "any type".
    pub types: Vec<String>,
    pub direction: Direction,
    /// Variable-length specification, if the pattern used `*`.
    pub length: Option<VarLength>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    /// `-[]->`
    Outgoing,
    /// `<-[]-`
    Incoming,
    /// `-[]-`
    Both,
}

/// A variable-length relationship bound, from `*`, `*n`, `*min..`, `*..max`, or `*min..max`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VarLength {
    pub min: u32,
    pub max: Option<u32>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Str(String),
    Int(i64),
    Bool(bool),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub distinct: bool,
    pub items: Vec<ReturnItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnItem {
    pub expr: Expr,
    pub alias: Option<String>,
}

impl ReturnItem {
    /// The output column name: the explicit alias, or a name derived from the expression.
    #[must_use]
    pub fn column_name(&self) -> String {
        self.alias.clone().unwrap_or_else(|| self.expr.display_name())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    Property(String, String),
    Literal(Literal),
    Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Compare(Box<Expr>, CmpOp, Box<Expr>),
    Aggregate {
        func: AggFn,
        arg: Option<Box<Expr>>,
        distinct: bool,
    },
}

impl Expr {
    /// Whether this expression tree contains an aggregate function call.
    #[must_use]
    #[allow(clippy::match_same_arms)]
    pub fn contains_aggregate(&self) -> bool {
        match self {
            Expr::Aggregate { .. } => true,
            Expr::Not(inner) => inner.contains_aggregate(),
            Expr::And(a, b) | Expr::Or(a, b) => a.contains_aggregate() || b.contains_aggregate(),
            Expr::Compare(a, _, b) => a.contains_aggregate() || b.contains_aggregate(),
            Expr::Var(_) | Expr::Property(..) | Expr::Literal(_) => false,
        }
    }

    /// A human-readable name for the expression, used as a default column header.
    #[must_use]
    pub fn display_name(&self) -> String {
        match self {
            Expr::Var(v) => v.clone(),
            Expr::Property(v, p) => format!("{v}.{p}"),
            Expr::Literal(lit) => match lit {
                Literal::Str(s) => format!("'{s}'"),
                Literal::Int(i) => i.to_string(),
                Literal::Bool(b) => b.to_string(),
                Literal::Null => "null".to_string(),
            },
            Expr::Not(inner) => format!("NOT {}", inner.display_name()),
            Expr::And(a, b) => format!("{} AND {}", a.display_name(), b.display_name()),
            Expr::Or(a, b) => format!("{} OR {}", a.display_name(), b.display_name()),
            Expr::Compare(a, op, b) => format!("{} {} {}", a.display_name(), op.as_str(), b.display_name()),
            Expr::Aggregate { func, arg, distinct } => {
                let inner = match arg {
                    Some(expr) => expr.display_name(),
                    None => "*".to_string(),
                };
                let distinct = if *distinct { "DISTINCT " } else { "" };
                format!("{}({distinct}{inner})", func.as_str())
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CmpOp {
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    Contains,
    StartsWith,
    EndsWith,
}

impl CmpOp {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            CmpOp::Eq => "=",
            CmpOp::Neq => "<>",
            CmpOp::Lt => "<",
            CmpOp::Lte => "<=",
            CmpOp::Gt => ">",
            CmpOp::Gte => ">=",
            CmpOp::Contains => "CONTAINS",
            CmpOp::StartsWith => "STARTS WITH",
            CmpOp::EndsWith => "ENDS WITH",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AggFn {
    Count,
    Collect,
    Min,
    Max,
    Sum,
    Avg,
}

impl AggFn {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            AggFn::Count => "count",
            AggFn::Collect => "collect",
            AggFn::Min => "min",
            AggFn::Max => "max",
            AggFn::Sum => "sum",
            AggFn::Avg => "avg",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct OrderItem {
    pub expr: Expr,
    pub descending: bool,
}

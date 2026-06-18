use std::collections::HashMap;
use std::collections::HashSet;

use crate::model::graph::Graph;

use super::ast::{AggFn, CmpOp, Direction, Expr, Literal, NodePattern, OrderItem, PathPattern, Query, ReturnItem};
use super::error::CypherError;
use super::schema::{self, NodeRef, RelType};
use super::value::CypherValue;

/// The tabular result of executing a query.
#[derive(Debug, Clone, PartialEq)]
pub struct ResultSet {
    pub columns: Vec<String>,
    pub rows: Vec<Vec<CypherValue>>,
}

/// A single binding row: maps pattern variable names to graph nodes.
type Row = HashMap<String, NodeRef>;

/// Executes a parsed query against the graph.
///
/// # Errors
///
/// Returns a [`CypherError::Execution`] for unknown relationship types, aggregates used in `WHERE`,
/// or `ORDER BY` expressions that cannot be resolved under aggregation.
pub fn execute(graph: &Graph, query: &Query) -> Result<ResultSet, CypherError> {
    let mut executor = Executor {
        graph,
        reverse_cache: HashMap::new(),
    };
    executor.run(query)
}

struct Executor<'a> {
    graph: &'a Graph,
    reverse_cache: HashMap<RelType, HashMap<NodeRef, Vec<NodeRef>>>,
}

impl Executor<'_> {
    fn run(&mut self, query: &Query) -> Result<ResultSet, CypherError> {
        let mut rows: Vec<Row> = vec![Row::new()];
        for pattern in &query.patterns {
            rows = self.eval_pattern(rows, pattern)?;
        }

        if let Some(predicate) = &query.where_clause {
            let mut filtered = Vec::with_capacity(rows.len());
            for row in rows {
                if self.eval_expr(&row, predicate)?.is_truthy() {
                    filtered.push(row);
                }
            }
            rows = filtered;
        }

        self.project(query, &rows)
    }

    // ---- Pattern matching ------------------------------------------------

    fn eval_pattern(&mut self, base: Vec<Row>, pattern: &PathPattern) -> Result<Vec<Row>, CypherError> {
        let mut working: Vec<(Row, NodeRef)> = Vec::new();

        for row in base {
            for node in self.candidates_for_node(&row, &pattern.start) {
                let mut new_row = row.clone();
                if let Some(var) = &pattern.start.var {
                    new_row.insert(var.clone(), node);
                }
                working.push((new_row, node));
            }
        }

        for (rel, node) in &pattern.rest {
            working = self.expand_step(working, rel, node)?;
        }

        Ok(working.into_iter().map(|(row, _)| row).collect())
    }

    fn candidates_for_node(&self, row: &Row, pattern: &NodePattern) -> Vec<NodeRef> {
        if let Some(var) = &pattern.var
            && let Some(existing) = row.get(var)
        {
            return if self.node_matches(*existing, pattern) {
                vec![*existing]
            } else {
                Vec::new()
            };
        }

        schema::scan(self.graph, &pattern.labels)
            .into_iter()
            .filter(|node| self.props_match(*node, pattern))
            .collect()
    }

    fn expand_step(
        &mut self,
        working: Vec<(Row, NodeRef)>,
        rel: &super::ast::RelPattern,
        node: &NodePattern,
    ) -> Result<Vec<(Row, NodeRef)>, CypherError> {
        let rel_types = resolve_rel_types(rel)?;
        let mut next = Vec::new();

        for (row, current) in working {
            let targets = self.step_targets(current, &rel_types, rel.direction, rel.length.as_ref());
            for target in targets {
                if !self.node_matches(target, node) {
                    continue;
                }
                if let Some(var) = &node.var
                    && let Some(existing) = row.get(var)
                    && *existing != target
                {
                    continue;
                }
                let mut new_row = row.clone();
                if let Some(var) = &node.var {
                    new_row.insert(var.clone(), target);
                }
                next.push((new_row, target));
            }
        }

        Ok(next)
    }

    fn step_targets(
        &mut self,
        node: NodeRef,
        rel_types: &[RelType],
        direction: Direction,
        length: Option<&super::ast::VarLength>,
    ) -> Vec<NodeRef> {
        if let Some(var_length) = length {
            return self.var_length_targets(node, rel_types, direction, var_length);
        }

        let mut seen = HashSet::new();
        let mut targets = Vec::new();
        for rel in rel_types {
            for target in self.step_once(node, *rel, direction) {
                if seen.insert(target) {
                    targets.push(target);
                }
            }
        }
        targets
    }

    fn step_once(&mut self, node: NodeRef, rel: RelType, direction: Direction) -> Vec<NodeRef> {
        match direction {
            Direction::Outgoing => schema::expand_out(self.graph, node, rel),
            Direction::Incoming => self.incoming(node, rel),
            Direction::Both => {
                let mut seen = HashSet::new();
                let mut targets = Vec::new();
                for target in schema::expand_out(self.graph, node, rel)
                    .into_iter()
                    .chain(self.incoming(node, rel))
                {
                    if seen.insert(target) {
                        targets.push(target);
                    }
                }
                targets
            }
        }
    }

    fn incoming(&mut self, node: NodeRef, rel: RelType) -> Vec<NodeRef> {
        if !self.reverse_cache.contains_key(&rel) {
            let mut reverse: HashMap<NodeRef, Vec<NodeRef>> = HashMap::new();
            for source in schema::rel_source_nodes(self.graph, rel) {
                for target in schema::expand_out(self.graph, source, rel) {
                    reverse.entry(target).or_default().push(source);
                }
            }
            self.reverse_cache.insert(rel, reverse);
        }

        self.reverse_cache
            .get(&rel)
            .and_then(|reverse| reverse.get(&node))
            .cloned()
            .unwrap_or_default()
    }

    fn var_length_targets(
        &mut self,
        start: NodeRef,
        rel_types: &[RelType],
        direction: Direction,
        var_length: &super::ast::VarLength,
    ) -> Vec<NodeRef> {
        let max = var_length.max.unwrap_or(u32::MAX);
        let mut results = Vec::new();
        let mut result_seen = HashSet::new();

        if var_length.min == 0 {
            results.push(start);
            result_seen.insert(start);
        }

        let mut visited = HashSet::new();
        visited.insert(start);
        let mut frontier = vec![start];
        let mut depth = 0u32;

        while depth < max && !frontier.is_empty() {
            depth += 1;
            let mut next = Vec::new();
            for node in &frontier {
                for rel in rel_types {
                    for target in self.step_once(*node, *rel, direction) {
                        if visited.insert(target) {
                            next.push(target);
                            if depth >= var_length.min && result_seen.insert(target) {
                                results.push(target);
                            }
                        }
                    }
                }
            }
            frontier = next;
        }

        results
    }

    fn node_matches(&self, node: NodeRef, pattern: &NodePattern) -> bool {
        if !schema::matches_labels(self.graph, node, &pattern.labels) {
            return false;
        }
        self.props_match(node, pattern)
    }

    fn props_match(&self, node: NodeRef, pattern: &NodePattern) -> bool {
        pattern
            .props
            .iter()
            .all(|(key, literal)| schema::property(self.graph, node, key) == literal_to_value(literal))
    }

    // ---- Expression evaluation -------------------------------------------

    fn eval_expr(&self, row: &Row, expr: &Expr) -> Result<CypherValue, CypherError> {
        match expr {
            Expr::Literal(literal) => Ok(literal_to_value(literal)),
            Expr::Var(name) => Ok(row.get(name).map_or(CypherValue::Null, |node| self.node_value(*node))),
            Expr::Property(var, prop) => Ok(row
                .get(var)
                .map_or(CypherValue::Null, |node| schema::property(self.graph, *node, prop))),
            Expr::Not(inner) => Ok(CypherValue::Bool(!self.eval_expr(row, inner)?.is_truthy())),
            Expr::And(a, b) => Ok(CypherValue::Bool(
                self.eval_expr(row, a)?.is_truthy() && self.eval_expr(row, b)?.is_truthy(),
            )),
            Expr::Or(a, b) => Ok(CypherValue::Bool(
                self.eval_expr(row, a)?.is_truthy() || self.eval_expr(row, b)?.is_truthy(),
            )),
            Expr::Compare(a, op, b) => {
                let left = self.eval_expr(row, a)?;
                let right = self.eval_expr(row, b)?;
                Ok(CypherValue::Bool(compare_values(&left, *op, &right)))
            }
            Expr::Aggregate { .. } => Err(CypherError::execution("aggregate functions are only allowed in RETURN")),
        }
    }

    fn node_value(&self, node: NodeRef) -> CypherValue {
        CypherValue::Node {
            label: schema::node_label(self.graph, node),
            name: schema::node_name(self.graph, node),
        }
    }

    // ---- Projection ------------------------------------------------------

    fn project(&self, query: &Query, rows: &[Row]) -> Result<ResultSet, CypherError> {
        let items = &query.return_clause.items;
        let columns: Vec<String> = items.iter().map(ReturnItem::column_name).collect();

        let has_aggregate = items.iter().any(|item| item.expr.contains_aggregate());

        let mut values = if has_aggregate {
            self.project_aggregated(query, rows)?
        } else {
            self.project_simple(query, rows)?
        };

        if query.return_clause.distinct {
            dedupe(&mut values);
        }

        apply_order_skip_limit(query, &mut values, &columns)?;

        Ok(ResultSet { columns, rows: values })
    }

    fn project_simple(&self, query: &Query, rows: &[Row]) -> Result<Vec<Vec<CypherValue>>, CypherError> {
        let items = &query.return_clause.items;
        let mut output = Vec::with_capacity(rows.len());
        for row in rows {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(self.eval_expr(row, &item.expr)?);
            }
            output.push(values);
        }
        Ok(output)
    }

    fn project_aggregated(&self, query: &Query, rows: &[Row]) -> Result<Vec<Vec<CypherValue>>, CypherError> {
        let items = &query.return_clause.items;

        // Group rows by the values of the non-aggregate (grouping) return items.
        let mut group_order: Vec<Vec<CypherValue>> = Vec::new();
        let mut groups: HashMap<Vec<CypherValue>, Vec<usize>> = HashMap::new();

        for (index, row) in rows.iter().enumerate() {
            let mut key = Vec::new();
            for item in items {
                if !item.expr.contains_aggregate() {
                    key.push(self.eval_expr(row, &item.expr)?);
                }
            }
            if !groups.contains_key(&key) {
                group_order.push(key.clone());
            }
            groups.entry(key).or_default().push(index);
        }

        // With no grouping keys and no input rows, aggregates still produce a single row.
        let grouping_keys = items.iter().filter(|item| !item.expr.contains_aggregate()).count();
        if group_order.is_empty() && grouping_keys == 0 {
            group_order.push(Vec::new());
            groups.insert(Vec::new(), Vec::new());
        }

        let mut output = Vec::with_capacity(group_order.len());
        for key in group_order {
            let row_indices = &groups[&key];
            let group_rows: Vec<&Row> = row_indices.iter().map(|index| &rows[*index]).collect();

            let mut values = Vec::with_capacity(items.len());
            let mut key_iter = key.iter();
            for item in items {
                if item.expr.contains_aggregate() {
                    values.push(self.eval_aggregate(&item.expr, &group_rows)?);
                } else {
                    values.push(key_iter.next().cloned().unwrap_or(CypherValue::Null));
                }
            }
            output.push(values);
        }

        Ok(output)
    }

    fn eval_aggregate(&self, expr: &Expr, group: &[&Row]) -> Result<CypherValue, CypherError> {
        let Expr::Aggregate { func, arg, distinct } = expr else {
            return Err(CypherError::execution("expected an aggregate function"));
        };

        // count(*) does not evaluate an argument.
        if *func == AggFn::Count && arg.is_none() {
            return Ok(CypherValue::Int(i64::try_from(group.len()).unwrap_or(i64::MAX)));
        }

        let arg_expr = arg
            .as_ref()
            .ok_or_else(|| CypherError::execution("aggregate function requires an argument"))?;

        let mut values = Vec::new();
        for row in group {
            let value = self.eval_expr(row, arg_expr)?;
            if value != CypherValue::Null {
                values.push(value);
            }
        }

        if *distinct {
            values = dedupe_values(values);
        }

        Ok(match func {
            AggFn::Count => CypherValue::Int(i64::try_from(values.len()).unwrap_or(i64::MAX)),
            AggFn::Collect => CypherValue::List(values),
            AggFn::Min => values
                .into_iter()
                .min_by(CypherValue::total_cmp)
                .unwrap_or(CypherValue::Null),
            AggFn::Max => values
                .into_iter()
                .max_by(CypherValue::total_cmp)
                .unwrap_or(CypherValue::Null),
            AggFn::Sum => CypherValue::Int(values.iter().filter_map(CypherValue::as_int).sum()),
            AggFn::Avg => {
                let numbers: Vec<i64> = values.iter().filter_map(CypherValue::as_int).collect();
                if numbers.is_empty() {
                    CypherValue::Null
                } else {
                    CypherValue::Int(numbers.iter().sum::<i64>() / i64::try_from(numbers.len()).unwrap_or(1))
                }
            }
        })
    }
}

fn apply_order_skip_limit(
    query: &Query,
    values: &mut Vec<Vec<CypherValue>>,
    columns: &[String],
) -> Result<(), CypherError> {
    // ORDER BY operates on the projected value rows: each ORDER BY expression must resolve to
    // a RETURN column (by identical expression or by naming a column/alias).
    if !query.order_by.is_empty() {
        let mut keys: Vec<usize> = Vec::with_capacity(query.order_by.len());
        for item in &query.order_by {
            keys.push(resolve_order_column(item, &query.return_clause.items, columns)?);
        }

        values.sort_by(|a, b| {
            for (key_index, order_item) in keys.iter().zip(&query.order_by) {
                let ordering = a[*key_index].total_cmp(&b[*key_index]);
                let ordering = if order_item.descending {
                    ordering.reverse()
                } else {
                    ordering
                };
                if ordering != std::cmp::Ordering::Equal {
                    return ordering;
                }
            }
            std::cmp::Ordering::Equal
        });
    }

    if let Some(skip) = query.skip {
        if skip >= values.len() {
            values.clear();
        } else {
            values.drain(0..skip);
        }
    }

    if let Some(limit) = query.limit
        && values.len() > limit
    {
        values.truncate(limit);
    }

    Ok(())
}

fn resolve_order_column(
    order_item: &OrderItem,
    items: &[ReturnItem],
    columns: &[String],
) -> Result<usize, CypherError> {
    // Match by identical return expression first.
    if let Some(index) = items.iter().position(|item| item.expr == order_item.expr) {
        return Ok(index);
    }

    // Otherwise, a bare variable in ORDER BY may name a return column or alias.
    if let Expr::Var(name) = &order_item.expr
        && let Some(index) = columns.iter().position(|column| column == name)
    {
        return Ok(index);
    }

    Err(CypherError::execution(format!(
        "ORDER BY expression `{}` must also appear in RETURN",
        order_item.expr.display_name()
    )))
}

fn resolve_rel_types(rel: &super::ast::RelPattern) -> Result<Vec<RelType>, CypherError> {
    if rel.types.is_empty() {
        return Ok(RelType::all().to_vec());
    }

    let mut types = Vec::with_capacity(rel.types.len());
    for name in &rel.types {
        let rel_type = RelType::parse(name)
            .ok_or_else(|| CypherError::execution(format!("unknown relationship type `{name}`")))?;
        types.push(rel_type);
    }
    Ok(types)
}

fn literal_to_value(literal: &Literal) -> CypherValue {
    match literal {
        Literal::Str(value) => CypherValue::Str(value.clone()),
        Literal::Int(value) => CypherValue::Int(*value),
        Literal::Bool(value) => CypherValue::Bool(*value),
        Literal::Null => CypherValue::Null,
    }
}

fn compare_values(left: &CypherValue, op: CmpOp, right: &CypherValue) -> bool {
    if matches!(left, CypherValue::Null) || matches!(right, CypherValue::Null) {
        return false;
    }

    match op {
        CmpOp::Eq => values_equal(left, right),
        CmpOp::Neq => !values_equal(left, right),
        CmpOp::Lt | CmpOp::Lte | CmpOp::Gt | CmpOp::Gte => {
            if !same_type(left, right) {
                return false;
            }
            let ordering = left.total_cmp(right);
            match op {
                CmpOp::Lt => ordering.is_lt(),
                CmpOp::Lte => ordering.is_le(),
                CmpOp::Gt => ordering.is_gt(),
                CmpOp::Gte => ordering.is_ge(),
                _ => unreachable!(),
            }
        }
        CmpOp::Contains => string_op(left, right, |haystack, needle| haystack.contains(needle)),
        CmpOp::StartsWith => string_op(left, right, |haystack, needle| haystack.starts_with(needle)),
        CmpOp::EndsWith => string_op(left, right, |haystack, needle| haystack.ends_with(needle)),
    }
}

fn values_equal(left: &CypherValue, right: &CypherValue) -> bool {
    same_type(left, right) && left == right
}

fn same_type(left: &CypherValue, right: &CypherValue) -> bool {
    matches!(
        (left, right),
        (CypherValue::Bool(_), CypherValue::Bool(_))
            | (CypherValue::Int(_), CypherValue::Int(_))
            | (CypherValue::Str(_), CypherValue::Str(_))
            | (CypherValue::Node { .. }, CypherValue::Node { .. })
            | (CypherValue::List(_), CypherValue::List(_))
    )
}

fn string_op(left: &CypherValue, right: &CypherValue, op: impl Fn(&str, &str) -> bool) -> bool {
    match (left.as_str(), right.as_str()) {
        (Some(haystack), Some(needle)) => op(haystack, needle),
        _ => false,
    }
}

fn dedupe(output: &mut Vec<Vec<CypherValue>>) {
    let mut seen: Vec<Vec<CypherValue>> = Vec::new();
    output.retain(|values| {
        if seen.iter().any(|existing| existing == values) {
            false
        } else {
            seen.push(values.clone());
            true
        }
    });
}

fn dedupe_values(values: Vec<CypherValue>) -> Vec<CypherValue> {
    let mut result: Vec<CypherValue> = Vec::new();
    for value in values {
        if !result.contains(&value) {
            result.push(value);
        }
    }
    result
}

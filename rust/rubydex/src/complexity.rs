//! ABC code complexity reports.
//!
//! Scores accumulate per report entry as three `f64` buckets — `a` (assignments),
//! `b` (branches + block calls), `c` (calls + everything else). An entry's score is
//! `(a² + b² + c²).sqrt()`. A single nesting `multiplier` starts at 1.0 per file and is
//! raised/lowered around penalized sub-trees; every score added is `weight * multiplier`.
//! The multiplier is intentionally NOT reset when entering a `def` (a def inside
//! `class_eval do ... end` scores at the elevated multiplier).
//!
//! The scoring rules are adapted from flog 4.9.4 to Prism's AST. Deliberate deviations:
//! - No DSL grouping (`task :name do ... end` is scored as a normal call + block in the
//!   enclosing entry, not a synthetic `task#name` method).
//! - No synthetic `lambda` call for `-> {}` (we still count the `block_call`).
//! - Entries are keyed `(name, file)` per file rather than merged across files, so
//!   time-series can attribute per file.
//! - The magic-number exemption checks the source text `0`/`-1` (numeric equality with
//!   0/-1).
//! - Prism-level lowering instead of `ruby_parser` sexps. Multi-statement bodies are
//!   detected via `StatementsNode` and penalized through `visit_body`; the lowering
//!   otherwise tracks the branch/rescue/else accounting of the source metric.

use crate::{
    config::Config,
    errors::Errors,
    job_queue::{Job, JobQueue},
    listing,
};
use crossbeam_channel::{Sender, unbounded};
use line_index::{LineIndex, TextSize};
use ruby_prism::{Node, Visit};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fmt::Write as _, fs, path::PathBuf, sync::Arc};

/// Round a score to two decimal places (half away from zero).
fn round2(x: f64) -> f64 {
    (x * 100.0).round() / 100.0
}
/// Convert a byte offset to a [`TextSize`], saturating at `u32::MAX` for pathological inputs.
fn text_size(offset: usize) -> TextSize {
    TextSize::from(u32::try_from(offset).unwrap_or(u32::MAX))
}

/// Return up to `top` items from `items` (`top == 0` returns all).
fn cap_slice<T>(items: &[T], top: usize) -> &[T] {
    if top == 0 {
        items
    } else {
        &items[..top.min(items.len())]
    }
}

/// Weight table for named method calls (bucket `c` unless noted).
fn call_weight(name: &[u8]) -> f64 {
    match name {
        b"define_method" | b"eval" | b"module_eval" | b"class_eval" | b"instance_eval" => 5.0,
        b"send" => 3.0,
        b"alias_method"
        | b"extend"
        | b"include"
        | b"instance_method"
        | b"instance_methods"
        | b"method_added"
        | b"method_defined?"
        | b"method_removed"
        | b"method_undefined"
        | b"private_class_method"
        | b"private_instance_methods"
        | b"private_method_defined?"
        | b"protected_instance_methods"
        | b"protected_method_defined?"
        | b"public_class_method"
        | b"public_instance_methods"
        | b"public_method_defined?"
        | b"remove_method"
        | b"undef_method"
        | b"inject" => 2.0,
        _ => 1.0,
    }
}

/// Which bucket a score accumulates into.
#[derive(Clone, Copy)]
enum Bucket {
    Assign,
    Branch,
    Call,
}
/// One scored construct contribution within a method (the `--details` breakdown):
/// e.g. `("branch", 1.1)`, `("map", 1.0)`, `("magic_number", 0.25)`. Labels follow the
/// source metric's `call_list` (`assignment`, `branch`, `block_call`, `block_pass`,
/// `to_proc_lasgn`, `to_proc_icky!`, `alias`, `sclass`, `super`, `yield`,
/// `magic_number`, or the called method's name).
#[derive(Clone, Serialize, Deserialize, Debug, PartialEq)]
pub struct Detail {
    pub label: String,
    pub score: f64,
}

/// One scored method (or `#none` scope) in a complexity report.
#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct MethodEntry {
    /// Lexical scoped name, e.g. `Foo::Bar#baz`, `Foo.baz`, `Foo#none`, `main#foo`.
    pub name: String,
    /// Report key: the listing path with a leading `./` stripped.
    pub file: String,
    /// 1-based start line.
    pub start_line: u32,
    /// 1-based end line.
    pub end_line: u32,
    /// Assignment bucket (`a`).
    pub assignments: f64,
    /// Branch + block-call bucket (`b`).
    pub branches: f64,
    /// Call + other bucket (`c`).
    pub calls: f64,
    /// `(a² + b² + c²).sqrt()`, rounded to 2 decimals at report build time.
    pub score: f64,
    /// Per-construct contribution breakdown (`--details`), sorted by score desc then label asc.
    /// Empty for score-0 methods or when detail collection was off. `default` +
    /// `skip_serializing_if` keep JSON compact and let v1 reports (which omit the field)
    /// deserialize with an empty breakdown.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub details: Vec<Detail>,
}

/// A full complexity report.
#[derive(Serialize, Deserialize, Debug)]
pub struct Report {
    /// Report schema version (currently 1).
    pub schema_version: u32,
    /// Sum of all entry scores.
    pub total: f64,
    /// `total / methods_count` (0.0 when empty).
    pub average: f64,
    /// Number of entries.
    pub methods_count: usize,
    /// Whether methods-only mode was used (`--methods-only`: out-of-method code skipped). `true` means
    /// out-of-method code was NOT scored. `#[serde(default)]` so older reports deserialize as
    /// `false`. [`Report::diff`] rejects baselines whose setting differs.
    #[serde(default)]
    pub methods_only: bool,
    /// Entries sorted by score desc, then name asc, then file asc.
    pub methods: Vec<MethodEntry>,
}

/// A single changed entry in a diff.
#[derive(Serialize, Debug)]
pub struct DiffEntry {
    pub name: String,
    pub file: String,
    pub baseline_score: f64,
    pub current_score: f64,
    /// `current_score - baseline_score`.
    pub delta: f64,
}

/// The diff of two reports, joined on `(name, file)`.
#[derive(Serialize, Debug)]
pub struct DiffReport {
    pub schema_version: u32,
    pub baseline_total: f64,
    pub current_total: f64,
    pub total_delta: f64,
    pub baseline_average: f64,
    pub current_average: f64,
    pub average_delta: f64,
    pub baseline_methods_count: usize,
    pub current_methods_count: usize,
    /// Entries present in both with `|delta| >= 0.01`, sorted delta desc, then name, then file.
    pub changed: Vec<DiffEntry>,
    /// Entries only in current, sorted score desc, then name, then file.
    pub added: Vec<MethodEntry>,
    /// Entries only in baseline, sorted score desc, then name, then file.
    pub removed: Vec<MethodEntry>,
}

/// A class/module/singleton lexical scope frame on the visitor's scope stack.
struct ScopeFrame {
    /// The constant-path source text for class/module scopes; `None` for singleton scopes.
    name: Option<String>,
    start_line: u32,
    end_line: u32,
    is_singleton: bool,
}

/// The Prism visitor that scores a single file.
struct ComplexityVisitor {
    file: String,
    line_index: LineIndex,
    line_count: u32,
    multiplier: f64,
    /// Set while visiting the value of a constant write (flog's `cdecl` magic-number exemption).
    suppress_magic_number: bool,
    scope_stack: Vec<ScopeFrame>,
    /// Stack of `(entry key, start_line, end_line)` for the currently open `def`s.
    method_stack: Vec<(String, u32, u32)>,
    entries: Vec<MethodEntry>,
    /// `entry name -> index in entries` for per-file merging.
    index: HashMap<String, usize>,
    /// When true (`--methods-only`), scoring is skipped for code outside any `def`: the
    /// out-of-method `#none` entry is never created, and a real `def none` is unaffected.
    methods_only: bool,
    /// When false (the default for repeated runs over large codebases), the per-construct
    /// `--details` breakdown is not collected and the score path stays allocation-free.
    collect_details: bool,
    /// `entry index -> (label -> weighted score)` for the `--details` breakdown. Only
    /// populated when `collect_details` is true.
    detail_accum: HashMap<usize, HashMap<String, f64>>,
}
impl ComplexityVisitor {
    fn new(file: String, source: &str, methods_only: bool, collect_details: bool) -> Self {
        let line_index = LineIndex::new(source);
        let line_count = u32::try_from(source.bytes().filter(|&b| b == b'\n').count())
            .unwrap_or(u32::MAX)
            .saturating_add(1);
        Self {
            file,
            line_index,
            line_count,
            multiplier: 1.0,
            suppress_magic_number: false,
            scope_stack: Vec::new(),
            method_stack: Vec::new(),
            entries: Vec::new(),
            index: HashMap::new(),
            methods_only,
            collect_details,
            detail_accum: HashMap::new(),
        }
    }

    /// Run `f` with `bonus` added to the multiplier, then restore it.
    fn penalize<F: FnOnce(&mut Self)>(&mut self, bonus: f64, f: F) {
        self.multiplier += bonus;
        f(self);
        self.multiplier -= bonus;
    }

    /// Visit a body the source metric wraps in a `ruby_parser` `:block` when it is multi-statement,
    /// adding an extra +0.1 nesting penalty in that case (its `process_block` rule). Prism always
    /// wraps bodies in `StatementsNode`, so the extra penalty fires only when there is more than
    /// one statement. Method/class bodies are NOT routed through this — they stay flat.
    fn visit_body(&mut self, body: &Node<'_>) {
        let multi = body.as_statements_node().is_some_and(|s| s.body().len() > 1);
        if multi {
            self.penalize(0.1, |s| s.visit(body));
        } else {
            self.visit(body);
        }
    }

    fn namespace_string(&self) -> String {
        if self.scope_stack.is_empty() {
            return "main".to_string();
        }
        let parts: Vec<&str> = self
            .scope_stack
            .iter()
            .filter_map(|frame| frame.name.as_deref())
            .collect();
        if parts.is_empty() {
            "main".to_string()
        } else {
            parts.join("::")
        }
    }

    fn in_singleton(&self) -> bool {
        self.scope_stack.iter().any(|frame| frame.is_singleton)
    }

    /// `(start, end)` lines for the `#none` entry of the current namespace: the innermost
    /// class/module/singleton frame's span, or `(1, line_count)` for `main`.
    fn none_lines(&self) -> (u32, u32) {
        self.scope_stack
            .last()
            .map_or((1, self.line_count), |frame| (frame.start_line, frame.end_line))
    }

    fn node_lines(&self, location: &ruby_prism::Location) -> (u32, u32) {
        let start = self.line_index.line_col(text_size(location.start_offset()));
        let end = self.line_index.line_col(text_size(location.end_offset()));
        (start.line + 1, end.line + 1)
    }

    /// Add `weight * multiplier` to the current entry's bucket, creating/merging the entry.
    /// `label` is the `call_list` construct name (e.g. `assignment`, `branch`,
    /// `block_pass`, or the called method's name) recorded for the `--details` breakdown.
    fn add(&mut self, bucket: Bucket, weight: f64, label: &str) {
        // `--methods-only`: skip code outside any `def`. The out-of-method `#none` entry is never
        // created; a real `def none` (whose key is also `Foo#none`) is scored normally because
        // it is visited with `method_stack` non-empty.
        if self.methods_only && self.method_stack.is_empty() {
            return;
        }
        let value = weight * self.multiplier;
        let (key, start_line, end_line) = if let Some((k, s, e)) = self.method_stack.last() {
            (k.clone(), *s, *e)
        } else {
            let k = format!("{}#none", self.namespace_string());
            let (s, e) = self.none_lines();
            (k, s, e)
        };
        let idx = self.add_to_entry(&key, bucket, value, start_line, end_line);
        if self.collect_details && weight > 0.0 {
            let map = self.detail_accum.entry(idx).or_default();
            *map.entry(label.to_string()).or_insert(0.0) += value;
        }
    }

    /// Returns the index of the (possibly newly created) entry.
    fn add_to_entry(&mut self, key: &str, bucket: Bucket, value: f64, start_line: u32, end_line: u32) -> usize {
        let idx = if let Some(i) = self.index.get(key).copied() {
            i
        } else {
            let i = self.entries.len();
            self.entries.push(MethodEntry {
                name: key.to_string(),
                file: self.file.clone(),
                start_line,
                end_line,
                assignments: 0.0,
                branches: 0.0,
                calls: 0.0,
                score: 0.0,
                details: Vec::new(),
            });
            self.index.insert(key.to_string(), i);
            i
        };
        let entry = &mut self.entries[idx];
        match bucket {
            Bucket::Assign => entry.assignments += value,
            Bucket::Branch => entry.branches += value,
            Bucket::Call => entry.calls += value,
        }
        entry.start_line = entry.start_line.min(start_line);
        entry.end_line = entry.end_line.max(end_line);
        idx
    }

    fn maybe_magic_number(&mut self, slice: &[u8]) {
        if self.suppress_magic_number {
            return;
        }
        if slice == b"0" || slice == b"-1" {
            return;
        }
        self.add(Bucket::Call, 0.25, "magic_number");
    }
}

fn is_numeric_literal(node: &Node) -> bool {
    matches!(
        node,
        Node::IntegerNode { .. } | Node::FloatNode { .. } | Node::RationalNode { .. } | Node::ImaginaryNode { .. }
    )
}

fn constant_path_string(node: &Node) -> String {
    String::from_utf8_lossy(node.location().as_slice()).to_string()
}

/// The source metric labels an operator-write's call contribution with the binary operator minus its
/// trailing `=` (e.g. `+=` -> `+`, `<<=` -> `<<`).
fn operator_label(loc: &ruby_prism::Location) -> String {
    String::from_utf8_lossy(loc.as_slice())
        .trim_end_matches('=')
        .to_string()
}

/// The namespace half of an entry name: everything before the last `#` or `.`
fn group_of(name: &str) -> &str {
    name.rsplit_once(['#', '.']).map_or(name, |(head, _)| head)
}

/// Write one method line of a text report.
fn write_entry(out: &mut String, entry: &MethodEntry) {
    let _ = writeln!(
        out,
        "{:8.1}: {:<40} {}:{}-{}",
        entry.score, entry.name, entry.file, entry.start_line, entry.end_line
    );
}

/// Write the per-construct breakdown beneath a method line (`--details` shape).
fn write_details(out: &mut String, entry: &MethodEntry) {
    for detail in &entry.details {
        let _ = writeln!(out, "{:8.1}:   {}", detail.score, detail.label);
    }
}

impl<'pr> Visit<'pr> for ComplexityVisitor {
    // --- Scope-opening nodes -----------------------------------------------------

    // --- Top-level program ------------------------------------------------------
    // flog wraps a multi-statement file in a `:block`, and `process_block` adds
    // +0.1. `process_defn` does NOT reset the multiplier, so every method in a
    // real file (e.g. `require` + `class`) inherits the +0.1. Prism's ProgramNode
    // body is a StatementsNode; route it through `visit_body` to mirror this.
    fn visit_program_node(&mut self, node: &ruby_prism::ProgramNode<'pr>) {
        self.visit_body(&node.statements().as_node());
    }

    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'pr>) {
        let name = constant_path_string(&node.constant_path());
        let (start_line, end_line) = self.node_lines(&node.location());
        if let Some(superclass) = node.superclass() {
            // Price the superclass expression at 2.0× before entering the new scope.
            self.penalize(1.0, |s| s.visit(&superclass));
        }
        self.scope_stack.push(ScopeFrame {
            name: Some(name),
            start_line,
            end_line,
            is_singleton: false,
        });
        if let Some(body) = node.body() {
            self.visit(&body);
        }
        self.scope_stack.pop();
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode<'pr>) {
        let name = constant_path_string(&node.constant_path());
        let (start_line, end_line) = self.node_lines(&node.location());
        self.scope_stack.push(ScopeFrame {
            name: Some(name),
            start_line,
            end_line,
            is_singleton: false,
        });
        if let Some(body) = node.body() {
            self.visit(&body);
        }
        self.scope_stack.pop();
    }

    fn visit_singleton_class_node(&mut self, node: &ruby_prism::SingletonClassNode<'pr>) {
        self.add(Bucket::Call, 5.0, "sclass"); // sclass
        let (start_line, end_line) = self.node_lines(&node.location());
        self.scope_stack.push(ScopeFrame {
            name: None,
            start_line,
            end_line,
            is_singleton: true,
        });
        self.penalize(0.5, |s| {
            s.visit(&node.expression());
            if let Some(body) = node.body() {
                s.visit(&body);
            }
        });
        self.scope_stack.pop();
    }

    fn visit_def_node(&mut self, node: &ruby_prism::DefNode<'pr>) {
        let method_name = String::from_utf8_lossy(node.name().as_slice()).to_string();
        let namespace = self.namespace_string();
        let separator = if node.receiver().is_some() || self.in_singleton() {
            '.'
        } else {
            '#'
        };
        let key = format!("{namespace}{separator}{method_name}");
        let (start_line, end_line) = self.node_lines(&node.location());
        self.method_stack.push((key.clone(), start_line, end_line));
        // Methods always appear in the report, even when they score 0 (flog lists them).
        self.add_to_entry(&key, Bucket::Call, 0.0, start_line, end_line);
        if let Some(parameters) = node.parameters() {
            self.visit(&parameters.as_node());
        }
        if let Some(body) = node.body() {
            self.visit(&body);
        }
        self.method_stack.pop();
    }

    // --- Assignments (bucket a) --------------------------------------------------

    fn visit_local_variable_write_node(&mut self, node: &ruby_prism::LocalVariableWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_instance_variable_write_node(&mut self, node: &ruby_prism::InstanceVariableWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_class_variable_write_node(&mut self, node: &ruby_prism::ClassVariableWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_global_variable_write_node(&mut self, node: &ruby_prism::GlobalVariableWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_constant_write_node(&mut self, node: &ruby_prism::ConstantWriteNode<'pr>) {
        // flog scores a constant declaration (`X = ...`) as 0.0 for the binding
        // itself (flog-4.9.4/test/test_flog.rb:463-475); only the RHS contributes,
        // and a direct numeric literal is suppressed. So: no assignment bucket,
        // still visit the value, still suppress a direct numeric magic number.
        let value = node.value();
        let prev = self.suppress_magic_number;
        if is_numeric_literal(&value) {
            self.suppress_magic_number = true;
        }
        self.visit(&value);
        self.suppress_magic_number = prev;
    }

    fn visit_constant_path_write_node(&mut self, node: &ruby_prism::ConstantPathWriteNode<'pr>) {
        // As with `ConstantWriteNode`: flog scores the binding as 0.0; only the
        // RHS contributes, with a direct numeric literal suppressed.
        let value = node.value();
        let prev = self.suppress_magic_number;
        if is_numeric_literal(&value) {
            self.suppress_magic_number = true;
        }
        self.visit(&value);
        self.suppress_magic_number = prev;
    }

    // Operator writes: +1 assignment AND +1 call (the operator method).

    fn visit_local_variable_operator_write_node(&mut self, node: &ruby_prism::LocalVariableOperatorWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        let op = if self.collect_details {
            operator_label(&node.binary_operator_loc())
        } else {
            String::new()
        };
        self.add(Bucket::Call, 1.0, &op);
        self.visit(&node.value());
    }

    fn visit_instance_variable_operator_write_node(
        &mut self,
        node: &ruby_prism::InstanceVariableOperatorWriteNode<'pr>,
    ) {
        self.add(Bucket::Assign, 1.0, "assignment");
        let op = if self.collect_details {
            operator_label(&node.binary_operator_loc())
        } else {
            String::new()
        };
        self.add(Bucket::Call, 1.0, &op);
        self.visit(&node.value());
    }

    fn visit_class_variable_operator_write_node(&mut self, node: &ruby_prism::ClassVariableOperatorWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        let op = if self.collect_details {
            operator_label(&node.binary_operator_loc())
        } else {
            String::new()
        };
        self.add(Bucket::Call, 1.0, &op);
        self.visit(&node.value());
    }

    fn visit_global_variable_operator_write_node(&mut self, node: &ruby_prism::GlobalVariableOperatorWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        let op = if self.collect_details {
            operator_label(&node.binary_operator_loc())
        } else {
            String::new()
        };
        self.add(Bucket::Call, 1.0, &op);
        self.visit(&node.value());
    }

    fn visit_constant_operator_write_node(&mut self, node: &ruby_prism::ConstantOperatorWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        let op = if self.collect_details {
            operator_label(&node.binary_operator_loc())
        } else {
            String::new()
        };
        self.add(Bucket::Call, 1.0, &op);
        self.visit(&node.value());
    }

    fn visit_constant_path_operator_write_node(&mut self, node: &ruby_prism::ConstantPathOperatorWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        let op = if self.collect_details {
            operator_label(&node.binary_operator_loc())
        } else {
            String::new()
        };
        self.add(Bucket::Call, 1.0, &op);
        self.visit(&node.value());
    }

    fn visit_call_operator_write_node(&mut self, node: &ruby_prism::CallOperatorWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        let op = if self.collect_details {
            operator_label(&node.binary_operator_loc())
        } else {
            String::new()
        };
        self.add(Bucket::Call, 1.0, &op);
        if let Some(receiver) = node.receiver() {
            self.visit(&receiver); // no call penalty
        }
        self.visit(&node.value());
    }

    fn visit_index_operator_write_node(&mut self, node: &ruby_prism::IndexOperatorWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        let op = if self.collect_details {
            operator_label(&node.binary_operator_loc())
        } else {
            String::new()
        };
        self.add(Bucket::Call, 1.0, &op);
        if let Some(receiver) = node.receiver() {
            self.visit(&receiver);
        }
        if let Some(arguments) = node.arguments() {
            self.visit(&arguments.as_node());
        }
        self.visit(&node.value());
    }

    // Or/And writes: +1 assignment only.

    fn visit_local_variable_or_write_node(&mut self, node: &ruby_prism::LocalVariableOrWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_local_variable_and_write_node(&mut self, node: &ruby_prism::LocalVariableAndWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_instance_variable_or_write_node(&mut self, node: &ruby_prism::InstanceVariableOrWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_instance_variable_and_write_node(&mut self, node: &ruby_prism::InstanceVariableAndWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_class_variable_or_write_node(&mut self, node: &ruby_prism::ClassVariableOrWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_class_variable_and_write_node(&mut self, node: &ruby_prism::ClassVariableAndWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_global_variable_or_write_node(&mut self, node: &ruby_prism::GlobalVariableOrWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_global_variable_and_write_node(&mut self, node: &ruby_prism::GlobalVariableAndWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_constant_or_write_node(&mut self, node: &ruby_prism::ConstantOrWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_constant_and_write_node(&mut self, node: &ruby_prism::ConstantAndWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_constant_path_or_write_node(&mut self, node: &ruby_prism::ConstantPathOrWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_constant_path_and_write_node(&mut self, node: &ruby_prism::ConstantPathAndWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_call_or_write_node(&mut self, node: &ruby_prism::CallOrWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        if let Some(receiver) = node.receiver() {
            self.visit(&receiver);
        }
        self.visit(&node.value());
    }

    fn visit_call_and_write_node(&mut self, node: &ruby_prism::CallAndWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        if let Some(receiver) = node.receiver() {
            self.visit(&receiver);
        }
        self.visit(&node.value());
    }

    fn visit_index_or_write_node(&mut self, node: &ruby_prism::IndexOrWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        if let Some(receiver) = node.receiver() {
            self.visit(&receiver);
        }
        if let Some(arguments) = node.arguments() {
            self.visit(&arguments.as_node());
        }
        self.visit(&node.value());
    }

    fn visit_index_and_write_node(&mut self, node: &ruby_prism::IndexAndWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        if let Some(receiver) = node.receiver() {
            self.visit(&receiver);
        }
        if let Some(arguments) = node.arguments() {
            self.visit(&arguments.as_node());
        }
        self.visit(&node.value());
    }

    fn visit_multi_write_node(&mut self, node: &ruby_prism::MultiWriteNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        for left in &node.lefts() {
            self.visit(&left);
        }
        if let Some(rest) = node.rest() {
            self.visit(&rest);
        }
        for right in &node.rights() {
            self.visit(&right);
        }
        self.visit(&node.value());
    }

    // Multi-target/`for` targets: +1 each (covers `for x in xs` and `a, b = ...` targets).

    fn visit_local_variable_target_node(&mut self, _node: &ruby_prism::LocalVariableTargetNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
    }

    fn visit_instance_variable_target_node(&mut self, _node: &ruby_prism::InstanceVariableTargetNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
    }

    fn visit_class_variable_target_node(&mut self, _node: &ruby_prism::ClassVariableTargetNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
    }

    fn visit_global_variable_target_node(&mut self, _node: &ruby_prism::GlobalVariableTargetNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
    }

    fn visit_constant_target_node(&mut self, _node: &ruby_prism::ConstantTargetNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
    }

    fn visit_constant_path_target_node(&mut self, _node: &ruby_prism::ConstantPathTargetNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
    }

    fn visit_call_target_node(&mut self, _node: &ruby_prism::CallTargetNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
    }

    fn visit_index_target_node(&mut self, _node: &ruby_prism::IndexTargetNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
    }

    // Optional parameter defaults: +1 assignment.

    fn visit_optional_parameter_node(&mut self, node: &ruby_prism::OptionalParameterNode<'pr>) {
        self.add(Bucket::Assign, 1.0, "assignment");
        self.visit(&node.value());
    }

    fn visit_optional_keyword_parameter_node(&mut self, node: &ruby_prism::OptionalKeywordParameterNode<'pr>) {
        // flog scores the DEFAULT value (magic/call) but not the keyword binding
        // itself — unlike positional optional (`a = 1`), which flog scores as an
        // assignment. Only visit the value.
        self.visit(&node.value());
    }

    // --- Branches (bucket b) -----------------------------------------------------

    fn visit_if_node(&mut self, node: &ruby_prism::IfNode<'pr>) {
        self.add(Bucket::Branch, 1.0, "branch");
        self.visit(&node.predicate());
        self.penalize(0.1, |s| {
            if let Some(statements) = node.statements() {
                s.visit_body(&statements.as_node());
            }
            if let Some(subsequent) = node.subsequent() {
                s.visit(&subsequent);
            }
        });
    }

    fn visit_unless_node(&mut self, node: &ruby_prism::UnlessNode<'pr>) {
        self.add(Bucket::Branch, 1.0, "branch");
        self.visit(&node.predicate());
        self.penalize(0.1, |s| {
            if let Some(statements) = node.statements() {
                s.visit_body(&statements.as_node());
            }
            if let Some(else_clause) = node.else_clause() {
                s.visit(&else_clause.as_node());
            }
        });
    }

    fn visit_case_node(&mut self, node: &ruby_prism::CaseNode<'pr>) {
        self.add(Bucket::Branch, 1.0, "branch");
        if let Some(predicate) = node.predicate() {
            self.visit(&predicate);
        }
        self.penalize(0.1, |s| {
            for condition in &node.conditions() {
                s.visit(&condition);
            }
            if let Some(else_clause) = node.else_clause() {
                s.visit(&else_clause.as_node());
            }
        });
    }

    fn visit_case_match_node(&mut self, node: &ruby_prism::CaseMatchNode<'pr>) {
        self.add(Bucket::Branch, 1.0, "branch");
        if let Some(predicate) = node.predicate() {
            self.visit(&predicate);
        }
        self.penalize(0.1, |s| {
            for condition in &node.conditions() {
                s.visit(&condition);
            }
            if let Some(else_clause) = node.else_clause() {
                s.visit(&else_clause.as_node());
            }
        });
    }

    fn visit_when_node(&mut self, node: &ruby_prism::WhenNode<'pr>) {
        self.add(Bucket::Branch, 1.0, "branch");
        // flog's `process_when` = `process_else`: the +0.1 wraps BOTH the
        // conditions and the body, so conditions land at case+0.1 + when+0.1.
        self.penalize(0.1, |s| {
            for condition in &node.conditions() {
                s.visit(&condition);
            }
            if let Some(statements) = node.statements() {
                s.visit_body(&statements.as_node());
            }
        });
    }

    fn visit_in_node(&mut self, node: &ruby_prism::InNode<'pr>) {
        self.add(Bucket::Branch, 1.0, "branch");
        self.visit(&node.pattern());
        self.penalize(0.1, |s| {
            if let Some(statements) = node.statements() {
                s.visit_body(&statements.as_node());
            }
        });
    }

    fn visit_while_node(&mut self, node: &ruby_prism::WhileNode<'pr>) {
        self.add(Bucket::Branch, 1.0, "branch");
        self.penalize(0.1, |s| {
            s.visit(&node.predicate());
            if let Some(statements) = node.statements() {
                s.visit_body(&statements.as_node());
            }
        });
    }

    fn visit_until_node(&mut self, node: &ruby_prism::UntilNode<'pr>) {
        self.add(Bucket::Branch, 1.0, "branch");
        self.penalize(0.1, |s| {
            s.visit(&node.predicate());
            if let Some(statements) = node.statements() {
                s.visit_body(&statements.as_node());
            }
        });
    }

    fn visit_and_node(&mut self, node: &ruby_prism::AndNode<'pr>) {
        self.add(Bucket::Branch, 1.0, "branch");
        self.penalize(0.1, |s| {
            s.visit(&node.left());
            s.visit(&node.right());
        });
    }

    fn visit_or_node(&mut self, node: &ruby_prism::OrNode<'pr>) {
        self.add(Bucket::Branch, 1.0, "branch");
        self.penalize(0.1, |s| {
            s.visit(&node.left());
            s.visit(&node.right());
        });
    }

    fn visit_begin_node(&mut self, node: &ruby_prism::BeginNode<'pr>) {
        // flog lowers `begin/rescue` to a single `:rescue` sexp handled by
        // `process_rescue` (= `process_else`): ONE branch + a +0.1 nesting
        // penalty wrapping the protected body, every clause (`:resbody`, which
        // has no handler so scores nothing extra), and the `else`. `:ensure`
        // wraps the `:rescue`, so the ensure body stays outside the penalty.
        if node.rescue_clause().is_some() {
            self.add(Bucket::Branch, 1.0, "branch");
            self.penalize(0.1, |s| {
                if let Some(statements) = node.statements() {
                    s.visit_body(&statements.as_node());
                }
                if let Some(rescue) = node.rescue_clause() {
                    s.visit(&rescue.as_node());
                }
                if let Some(else_clause) = node.else_clause() {
                    s.visit(&else_clause.as_node());
                }
            });
            if let Some(ensure) = node.ensure_clause() {
                self.visit(&ensure.as_node());
            }
        } else {
            // Plain `begin` / ensure-only: flog has no `process_begin`/`process_ensure`,
            // so no branch and no nesting penalty; the body's own multi-statement
            // `:block` penalty (visit_body) still applies.
            if let Some(statements) = node.statements() {
                self.visit_body(&statements.as_node());
            }
            if let Some(else_clause) = node.else_clause() {
                self.visit(&else_clause.as_node());
            }
            if let Some(ensure) = node.ensure_clause() {
                self.visit(&ensure.as_node());
            }
        }
    }

    fn visit_rescue_modifier_node(&mut self, node: &ruby_prism::RescueModifierNode<'pr>) {
        // ruby_parser lowers `foo rescue bar` to a `:rescue`: one branch + +0.1
        // wrapping both the protected expression and the rescue expression.
        self.add(Bucket::Branch, 1.0, "branch");
        self.penalize(0.1, |s| {
            s.visit(&node.expression());
            s.visit(&node.rescue_expression());
        });
    }

    fn visit_rescue_node(&mut self, node: &ruby_prism::RescueNode<'pr>) {
        // The branch + nesting penalty are owned by the enclosing BeginNode
        // (flog's `:rescue`); each clause (`:resbody`) only contributes its
        // exceptions, reference, and body. Subsequent clauses are flattened —
        // no extra branch, no extra penalty (they inherit the begin's +0.1).
        for exception in &node.exceptions() {
            self.visit(&exception);
        }
        if let Some(reference) = node.reference() {
            self.visit(&reference);
        }
        if let Some(statements) = node.statements() {
            self.visit_body(&statements.as_node());
        }
        if let Some(subsequent) = node.subsequent() {
            self.visit(&subsequent.as_node());
        }
    }

    fn visit_else_node(&mut self, node: &ruby_prism::ElseNode<'pr>) {
        // flog's if/else is a single branch (the `else` adds no score); only the body's
        // multi-statement `:block` penalty applies, handled by `visit_body`.
        if let Some(statements) = node.statements() {
            self.visit_body(&statements.as_node());
        }
    }

    fn visit_block_node(&mut self, node: &ruby_prism::BlockNode<'pr>) {
        self.add(Bucket::Branch, 1.0, "block_call"); // block_call
        if let Some(parameters) = node.parameters() {
            self.visit(&parameters);
        }
        self.penalize(0.1, |s| {
            if let Some(body) = node.body() {
                s.visit_body(&body);
            }
        });
    }

    fn visit_lambda_node(&mut self, node: &ruby_prism::LambdaNode<'pr>) {
        self.add(Bucket::Branch, 1.0, "block_call"); // block_call (deviation: no synthetic `lambda` call)
        if let Some(parameters) = node.parameters() {
            self.visit(&parameters);
        }
        self.penalize(0.1, |s| {
            if let Some(body) = node.body() {
                s.visit_body(&body);
            }
        });
    }

    // --- Calls & other (bucket c) ------------------------------------------------

    fn visit_call_node(&mut self, node: &ruby_prism::CallNode<'pr>) {
        // A `variable_call` is an unresolved bareword method call (Prism reserves
        // `LocalVariableReadNode` for known locals), so it scores like any other call.
        if node.is_attribute_write() {
            // `attrasgn`: +1 assignment instead of a call score; no penalties on children.
            self.add(Bucket::Assign, 1.0, "assignment");
            if let Some(receiver) = node.receiver() {
                self.visit(&receiver);
            }
            if let Some(arguments) = node.arguments() {
                self.visit(&arguments.as_node());
            }
            if let Some(block) = node.block() {
                self.visit(&block);
            }
            return;
        }

        let name_label = if self.collect_details {
            String::from_utf8_lossy(node.name().as_slice()).to_string()
        } else {
            String::new()
        };
        self.add(Bucket::Call, call_weight(node.name().as_slice()), &name_label);
        let receiver_bonus = if node.is_safe_navigation() { 0.3 } else { 0.2 };
        if let Some(receiver) = node.receiver() {
            self.penalize(receiver_bonus, |s| s.visit(&receiver));
        }
        if let Some(arguments) = node.arguments() {
            self.penalize(0.2, |s| s.visit(&arguments.as_node()));
        }
        if let Some(block) = node.block() {
            self.visit(&block); // unpenalized; block rules apply in the block visitor
        }
    }

    fn visit_super_node(&mut self, node: &ruby_prism::SuperNode<'pr>) {
        self.add(Bucket::Call, 1.0, "super"); // super
        if let Some(arguments) = node.arguments() {
            self.visit(&arguments.as_node());
        }
        if let Some(block) = node.block() {
            self.visit(&block);
        }
    }

    fn visit_forwarding_super_node(&mut self, node: &ruby_prism::ForwardingSuperNode<'pr>) {
        self.add(Bucket::Call, 1.0, "super"); // super
        if let Some(block) = node.block() {
            self.visit(&block.as_node());
        }
    }

    fn visit_yield_node(&mut self, node: &ruby_prism::YieldNode<'pr>) {
        self.add(Bucket::Call, 1.0, "yield"); // yield
        if let Some(arguments) = node.arguments() {
            self.visit(&arguments.as_node());
        }
    }

    fn visit_block_argument_node(&mut self, node: &ruby_prism::BlockArgumentNode<'pr>) {
        self.add(Bucket::Call, 1.0, "block_pass"); // block_pass
        if let Some(expression) = node.expression() {
            match &expression {
                Node::LocalVariableWriteNode { .. } => self.add(Bucket::Call, 15.0, "to_proc_lasgn"), // to_proc_lasgn
                Node::InterpolatedSymbolNode { .. }
                | Node::InterpolatedStringNode { .. }
                | Node::HashNode { .. }
                | Node::IfNode { .. }
                | Node::UnlessNode { .. }
                | Node::CaseNode { .. }
                | Node::CaseMatchNode { .. }
                | Node::AndNode { .. }
                | Node::OrNode { .. } => self.add(Bucket::Call, 10.0, "to_proc_icky!"), // to_proc_icky
                Node::CallNode { .. } => {
                    if let Some(call) = expression.as_call_node()
                        && call.block().is_some()
                    {
                        self.add(Bucket::Call, 10.0, "to_proc_icky!"); // `&proc { }` / `&lambda { }`
                    }
                }
                _ => {}
            }
            self.visit(&expression);
        }
    }

    fn visit_alias_method_node(&mut self, node: &ruby_prism::AliasMethodNode<'pr>) {
        self.add(Bucket::Call, 2.0, "alias"); // alias
        self.visit(&node.new_name());
        self.visit(&node.old_name());
    }

    fn visit_alias_global_variable_node(&mut self, node: &ruby_prism::AliasGlobalVariableNode<'pr>) {
        self.add(Bucket::Call, 2.0, "alias"); // alias
        self.visit(&node.new_name());
        self.visit(&node.old_name());
    }

    // --- Magic numbers (bucket c) ------------------------------------------------

    fn visit_integer_node(&mut self, node: &ruby_prism::IntegerNode<'pr>) {
        self.maybe_magic_number(node.location().as_slice());
    }

    fn visit_float_node(&mut self, node: &ruby_prism::FloatNode<'pr>) {
        self.maybe_magic_number(node.location().as_slice());
    }

    fn visit_rational_node(&mut self, node: &ruby_prism::RationalNode<'pr>) {
        self.maybe_magic_number(node.location().as_slice());
    }

    fn visit_imaginary_node(&mut self, node: &ruby_prism::ImaginaryNode<'pr>) {
        self.maybe_magic_number(node.location().as_slice());
    }
}

/// A job that scores a single file on the work-stealing queue.
struct ComplexityJob {
    path: PathBuf,
    methods_only: bool,
    collect_details: bool,
    entries_tx: Sender<Vec<MethodEntry>>,
    errors_tx: Sender<Errors>,
}

impl ComplexityJob {
    fn new(
        path: PathBuf,
        methods_only: bool,
        collect_details: bool,
        entries_tx: Sender<Vec<MethodEntry>>,
        errors_tx: Sender<Errors>,
    ) -> Self {
        Self {
            path,
            methods_only,
            collect_details,
            entries_tx,
            errors_tx,
        }
    }

    fn send_error(&self, error: Errors) {
        self.errors_tx
            .send(error)
            .expect("errors receiver dropped before run completion");
    }
}

impl Job for ComplexityJob {
    fn run(&self) {
        let Ok(source) = fs::read_to_string(&self.path) else {
            self.send_error(Errors::FileError(format!(
                "Failed to read file `{}`",
                self.path.display()
            )));
            return;
        };

        let raw = self.path.to_string_lossy().replace('\\', "/");
        let file_string = raw.strip_prefix("./").unwrap_or(&raw).to_string();

        let entries = analyze_source_with(&file_string, &source, self.methods_only, self.collect_details);
        self.entries_tx
            .send(entries)
            .expect("entries receiver dropped before merge");
    }
}

/// Score a single source string, returning unrounded entries (the unit-test seam). Defaults to
/// `methods_only = false` and `collect_details = false` (the hot path for large codebases).
/// Scores are computed (`sqrt(a² + b² + c²)`) but not rounded; [`analyze`] rounds at report time.
#[must_use]
pub fn analyze_source(file: &str, source: &str) -> Vec<MethodEntry> {
    analyze_source_with(file, source, false, false)
}

/// Score a single source string with explicit `methods_only` (`--methods-only`) and `collect_details`
/// (`--details`) flags. When `collect_details` is true, each entry's `details` is populated with the
/// per-construct contribution breakdown sorted by score desc then label asc.
#[must_use]
pub fn analyze_source_with(file: &str, source: &str, methods_only: bool, collect_details: bool) -> Vec<MethodEntry> {
    let result = ruby_prism::parse(source.as_bytes());
    let mut visitor = ComplexityVisitor::new(file.to_string(), source, methods_only, collect_details);
    visitor.visit(&result.node());
    let mut detail_accum = std::mem::take(&mut visitor.detail_accum);
    for (i, entry) in visitor.entries.iter_mut().enumerate() {
        entry.score =
            (entry.assignments * entry.assignments + entry.branches * entry.branches + entry.calls * entry.calls)
                .sqrt();
        if let Some(map) = detail_accum.remove(&i) {
            let mut details: Vec<Detail> = map.into_iter().map(|(label, score)| Detail { label, score }).collect();
            details.sort_by(|a, b| {
                b.score
                    .partial_cmp(&a.score)
                    .unwrap_or(std::cmp::Ordering::Equal)
                    .then(a.label.cmp(&b.label))
            });
            entry.details = details;
        }
    }
    visitor.entries
}

fn build_report(entries: Vec<MethodEntry>, methods_only: bool) -> Report {
    let mut methods: Vec<MethodEntry> = entries
        .into_iter()
        .map(|mut entry| {
            entry.assignments = round2(entry.assignments);
            entry.branches = round2(entry.branches);
            entry.calls = round2(entry.calls);
            // flog keeps full precision and displays to 1 decimal; rounding the
            // score to 2 decimals flips display rounding at `.xx5` boundaries
            // (e.g. 3.448 -> 3.45 -> "3.5" vs flog's "3.4"). Compute from the
            // rounded buckets but leave the score at full precision.
            entry.score =
                (entry.assignments * entry.assignments + entry.branches * entry.branches + entry.calls * entry.calls)
                    .sqrt();
            entry
        })
        .collect();

    methods.sort_by(|a, b| {
        b.score
            .partial_cmp(&a.score)
            .unwrap_or(std::cmp::Ordering::Equal)
            .then(a.name.cmp(&b.name))
            .then(a.file.cmp(&b.file))
    });

    let total = methods.iter().map(|entry| entry.score).sum::<f64>();
    let methods_count = methods.len();
    let average = if methods_count == 0 {
        0.0
    } else {
        total / f64::from(u32::try_from(methods_count).unwrap_or(u32::MAX))
    };
    Report {
        schema_version: 1,
        total,
        average,
        methods_count,
        methods_only,
        methods,
    }
}

/// Analyze the given paths and build a complexity report.
///
/// Mirrors `indexing::index_files`: workspace root → config → listing → parallel parse+score.
/// `.rbs` files are filtered out (complexity applies to Ruby only). Prism is error-tolerant, so
/// files with parse errors are still scored for whatever parsed.
///
/// # Errors
///
/// Returns `Errors::ConfigError`/`ConfigNotFound` if the workspace config fails to load.
///
/// # Panics
///
/// Panics if a worker thread panics while scoring a file.
pub fn analyze(paths: Vec<String>, methods_only: bool, collect_details: bool) -> Result<(Report, Vec<Errors>), Errors> {
    let mut config = Config::new();
    if let Some(workspace) = listing::workspace_path_for(&paths) {
        config.set_workspace_path(workspace);
    }
    config.load_default()?;

    // Complexity exclusions are decoupled from the indexer's `exclude` so a file can be indexed
    // but skipped by `rdx complexity`. Both share the default skipped directories.
    let excluded = config.complexity_excluded_patterns();
    let (file_paths, listing_errors) = listing::collect_file_paths(paths, &excluded);

    let ruby_paths: Vec<PathBuf> = file_paths
        .into_iter()
        .filter(|path| path.extension().is_some_and(|ext| ext != "rbs"))
        .collect();

    let queue = Arc::new(JobQueue::new());
    let (entries_tx, entries_rx) = unbounded();
    let (errors_tx, errors_rx) = unbounded();

    for path in ruby_paths {
        queue.push(Box::new(ComplexityJob::new(
            path,
            methods_only,
            collect_details,
            entries_tx.clone(),
            errors_tx.clone(),
        )));
    }

    drop(entries_tx);
    drop(errors_tx);

    let handles = JobQueue::run_without_waiting(&queue);

    let mut all_entries: Vec<MethodEntry> = Vec::new();
    while let Ok(entries) = entries_rx.recv() {
        all_entries.extend(entries);
    }

    for handle in handles {
        handle.join().expect("Worker thread panicked");
    }

    let mut errors: Vec<Errors> = listing_errors;
    errors.extend(errors_rx.iter());

    Ok((build_report(all_entries, methods_only), errors))
}

impl Report {
    /// Render a text report. `top == 0` prints all entries (otherwise the top `top`
    /// by score). `details` prints the per-construct contribution breakdown under each method.
    /// `group` groups entries by namespace with a per-group subtotal;
    /// the `top` cap is applied before grouping.
    #[must_use]
    pub fn render_text(&self, top: usize, details: bool, group: bool) -> String {
        let mut out = String::new();
        let _ = writeln!(out, "{:8.1}: total complexity", self.total);
        let _ = writeln!(out, "{:8.1}: average complexity\n", self.average);

        if group {
            self.render_grouped(&mut out, top, details);
        } else {
            let limit = if top == 0 {
                self.methods.len()
            } else {
                top.min(self.methods.len())
            };
            for entry in &self.methods[..limit] {
                write_entry(&mut out, entry);
                if details {
                    write_details(&mut out, entry);
                }
            }
        }
        out
    }

    /// Group the already-top-capped entries by namespace (the part of the name before the last
    /// `#` or `.`), print each group with a subtotal over the selected set, groups sorted by
    /// subtotal desc then name; methods within a group keep the global score-desc order. `top`
    /// is the global row cap applied BEFORE grouping (threshold-before-group order),
    /// so the total displayed method rows never exceed `top`.
    fn render_grouped(&self, out: &mut String, top: usize, details: bool) {
        let limit = if top == 0 {
            self.methods.len()
        } else {
            top.min(self.methods.len())
        };
        let selected = &self.methods[..limit];

        let mut groups: Vec<(&str, Vec<&MethodEntry>)> = Vec::new();
        let mut index: HashMap<&str, usize> = HashMap::new();
        for entry in selected {
            let gname = group_of(&entry.name);
            if let Some(&i) = index.get(gname) {
                groups[i].1.push(entry);
            } else {
                index.insert(gname, groups.len());
                groups.push((gname, vec![entry]));
            }
        }
        let mut totals: Vec<(usize, f64)> = groups
            .iter()
            .enumerate()
            .map(|(i, (_, ents))| (i, ents.iter().map(|e| e.score).sum()))
            .collect();
        totals.sort_by(|a, b| {
            b.1.partial_cmp(&a.1)
                .unwrap_or(std::cmp::Ordering::Equal)
                .then(groups[a.0].0.cmp(groups[b.0].0))
        });
        for (gi, total) in totals {
            let (gname, ents) = &groups[gi];
            let _ = writeln!(out, "{total:8.1}: {gname} total");
            for entry in ents {
                write_entry(out, entry);
                if details {
                    write_details(out, entry);
                }
            }
            out.push('\n');
        }
    }

    /// Serialize the report as pretty JSON.
    ///
    /// # Panics
    ///
    /// Panics if the report fails to serialize (only possible if a field contains an interior NUL byte).
    #[must_use]
    pub fn to_json(&self) -> String {
        serde_json::to_string_pretty(self).expect("report must serialize")
    }

    /// Parse a report from JSON.
    ///
    /// # Errors
    ///
    /// Returns `Errors::ReportError` if the JSON is malformed or does not match the report shape.
    pub fn from_json(json: &str) -> Result<Report, Errors> {
        serde_json::from_str(json).map_err(|error| Errors::ReportError(error.to_string()))
    }

    /// Diff this baseline against `current`, joining entries on `(name, file)`.
    ///
    /// # Errors
    ///
    /// Returns `Errors::ReportError` if the two reports were scored with different
    /// `methods_only` modes — a `methods_only` current report against a normal baseline
    /// would surface every `#none` as removed and corrupt the total delta, and there is
    /// no safe post-hoc repair (a real `def none` is indistinguishable from out-of-method
    /// code in the stored entry). Regenerate the baseline with the same mode.
    pub fn diff(baseline: &Report, current: &Report) -> Result<DiffReport, Errors> {
        if baseline.methods_only != current.methods_only {
            return Err(Errors::ReportError(format!(
                "cannot diff reports with mismatched scoring modes: baseline methods_only={}, current methods_only={} \
                 (regenerate the baseline with the same `--methods-only` setting)",
                baseline.methods_only, current.methods_only
            )));
        }
        let baseline_map: HashMap<(String, String), &MethodEntry> = baseline
            .methods
            .iter()
            .map(|entry| ((entry.name.clone(), entry.file.clone()), entry))
            .collect();
        let current_map: HashMap<(String, String), &MethodEntry> = current
            .methods
            .iter()
            .map(|entry| ((entry.name.clone(), entry.file.clone()), entry))
            .collect();
        let mut changed = Vec::new();
        let mut added = Vec::new();

        for (key, current_entry) in &current_map {
            match baseline_map.get(key) {
                Some(baseline_entry) => {
                    let delta = current_entry.score - baseline_entry.score;
                    if delta.abs() >= 0.01 {
                        changed.push(DiffEntry {
                            name: current_entry.name.clone(),
                            file: current_entry.file.clone(),
                            baseline_score: baseline_entry.score,
                            current_score: current_entry.score,
                            delta,
                        });
                    }
                }
                None => added.push((*current_entry).clone()),
            }
        }

        let removed: Vec<MethodEntry> = baseline_map
            .iter()
            .filter(|(key, _)| !current_map.contains_key(*key))
            .map(|(_, entry)| (*entry).clone())
            .collect();

        let sort_changed = |a: &DiffEntry, b: &DiffEntry| {
            b.delta
                .partial_cmp(&a.delta)
                .unwrap_or(std::cmp::Ordering::Equal)
                .then(a.name.cmp(&b.name))
                .then(a.file.cmp(&b.file))
        };
        let sort_entries = |a: &MethodEntry, b: &MethodEntry| {
            b.score
                .partial_cmp(&a.score)
                .unwrap_or(std::cmp::Ordering::Equal)
                .then(a.name.cmp(&b.name))
                .then(a.file.cmp(&b.file))
        };
        changed.sort_by(sort_changed);
        added.sort_by(sort_entries);
        let mut removed = removed;
        removed.sort_by(sort_entries);

        Ok(DiffReport {
            schema_version: 1,
            baseline_total: baseline.total,
            current_total: current.total,
            total_delta: round2(current.total - baseline.total),
            baseline_average: baseline.average,
            current_average: current.average,
            average_delta: round2(current.average - baseline.average),
            baseline_methods_count: baseline.methods_count,
            current_methods_count: current.methods_count,
            changed,
            added,
            removed,
        })
    }
}

impl DiffReport {
    /// Render a diff as text. Each section caps at `top` rows (`0 = all`); empty sections are omitted.
    #[must_use]
    pub fn render_text(&self, top: usize) -> String {
        let mut out = String::new();
        let _ = writeln!(
            out,
            "Total:   {:.1} -> {:.1} ({:+.1})",
            self.baseline_total, self.current_total, self.total_delta
        );
        let _ = writeln!(
            out,
            "Average: {:.1} -> {:.1} ({:+.1})",
            self.baseline_average, self.current_average, self.average_delta
        );
        let _ = writeln!(
            out,
            "Methods: {} -> {} ({:+})",
            self.baseline_methods_count,
            self.current_methods_count,
            i64::try_from(self.current_methods_count).unwrap_or(i64::MAX)
                - i64::try_from(self.baseline_methods_count).unwrap_or(i64::MAX)
        );

        let regressions: Vec<&DiffEntry> = self.changed.iter().filter(|entry| entry.delta > 0.0).collect();
        let improvements: Vec<&DiffEntry> = self.changed.iter().filter(|entry| entry.delta < 0.0).collect();

        let render_diff = |label: &str, items: &[&DiffEntry], out: &mut String| {
            if items.is_empty() {
                return;
            }
            out.push_str(label);
            out.push('\n');
            for entry in cap_slice(items, top) {
                let _ = writeln!(
                    out,
                    "{:+8.1}: {} ({:.1} -> {:.1})  {}",
                    entry.delta, entry.name, entry.baseline_score, entry.current_score, entry.file
                );
            }
        };

        render_diff("Regressions:", &regressions, &mut out);
        render_diff("Improvements:", &improvements, &mut out);

        if !self.added.is_empty() {
            out.push_str("Added:\n");
            let added_refs: Vec<&MethodEntry> = self.added.iter().collect();
            for entry in cap_slice(&added_refs, top) {
                let _ = writeln!(out, "{:8.1}: {}  {}", entry.score, entry.name, entry.file);
            }
        }

        if !self.removed.is_empty() {
            out.push_str("Removed:\n");
            let removed_refs: Vec<&MethodEntry> = self.removed.iter().collect();
            for entry in cap_slice(&removed_refs, top) {
                let _ = writeln!(out, "{:8.1}: {}  {}", entry.score, entry.name, entry.file);
            }
        }

        out
    }

    /// Serialize the diff as pretty JSON.
    ///
    /// # Panics
    ///
    /// Panics if the diff fails to serialize (only possible if a field contains an interior NUL byte).
    #[must_use]
    pub fn to_json(&self) -> String {
        serde_json::to_string_pretty(self).expect("diff report must serialize")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry_for<'a>(entries: &'a [MethodEntry], name: &str) -> &'a MethodEntry {
        entries
            .iter()
            .find(|entry| entry.name == name)
            .unwrap_or_else(|| panic!("entry `{name}` not found in {entries:?}"))
    }

    fn approx(a: f64, b: f64) -> bool {
        (a - b).abs() < 0.01
    }

    #[test]
    fn empty_def() {
        let entries = analyze_source("foo.rb", "def foo; end\n");
        assert_eq!(entries.len(), 1);
        let entry = entry_for(&entries, "main#foo");
        assert!(approx(entry.assignments, 0.0));
        assert!(approx(entry.branches, 0.0));
        assert!(approx(entry.calls, 0.0));
        assert!(approx(entry.score, 0.0));
    }

    #[test]
    fn bareword_call_is_scored() {
        // `foo` is an unresolved bareword -> Prism CallNode with variable_call; flog scores it as a call.
        let entries = analyze_source("foo.rb", "def x; foo; end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.calls, 1.0));
    }

    #[test]
    fn local_variable_read_is_not_scored() {
        // `a` is a known local (assigned above) -> LocalVariableReadNode, not a call.
        let entries = analyze_source("foo.rb", "def x; a = 0; a; end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.assignments, 1.0));
        assert!(approx(entry.calls, 0.0)); // `0` is exempt, `a` is a local read
    }

    #[test]
    fn bareword_receiver_is_scored() {
        // `a` in `a.b` is an unresolved bareword call (variable_call) scored as the receiver of
        // `b` at +0.2: b 1.0 + a 1.2 = 2.2. A known local would parse as LocalVariableReadNode.
        let entries = analyze_source("foo.rb", "def x; a.b; end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.calls, 2.2));
    }

    #[test]
    fn if_with_penalized_call() {
        let source = "class Foo\n  def bar(a)\n    if a\n      baz(a)\n    end\n  end\nend\n";
        let entries = analyze_source("foo.rb", source);
        let entry = entry_for(&entries, "Foo#bar");
        assert!(approx(entry.branches, 1.0));
        assert!(approx(entry.calls, 1.1)); // baz at 0.1 penalty
        assert!(approx(entry.score, (1.0_f64 * 1.0 + 1.1 * 1.1).sqrt()));
        assert_eq!(entry.start_line, 2);
        assert_eq!(entry.end_line, 6);
    }

    #[test]
    fn assignment_with_magic_number() {
        let entries = analyze_source("foo.rb", "def x; a = 1; end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.assignments, 1.0));
        assert!(approx(entry.calls, 0.25));
        assert!(approx(entry.score, (1.0_f64 + 0.25 * 0.25).sqrt()));
    }

    #[test]
    fn send_weight() {
        let entries = analyze_source("foo.rb", "def x; send(:foo); end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.calls, 3.0));
    }

    #[test]
    fn include_into_class_none() {
        let entries = analyze_source("foo.rb", "class Foo; include Bar; end\n");
        let entry = entry_for(&entries, "Foo#none");
        assert!(approx(entry.calls, 2.0));
        assert_eq!(entry.start_line, 1);
        assert_eq!(entry.end_line, 1);
    }

    #[test]
    fn multi_write_targets_and_magic_numbers() {
        let entries = analyze_source("foo.rb", "def x; a, b = 1, 2; end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.assignments, 3.0)); // masgn + 2 targets
        assert!(approx(entry.calls, 0.5)); // two magic numbers
    }

    #[test]
    fn constant_write_scores_zero_binding() {
        // flog scores a constant declaration's binding as 0.0
        // (flog-4.9.4/test/test_flog.rb:463-475): `X = 5` contributes nothing
        // (the numeric RHS is suppressed) so no entry is produced, and `X = foo`
        // scores only the RHS call (no assignment).
        let lit = analyze_source("foo.rb", "X = 5\n");
        assert!(lit.is_empty());
        // `ConstantPathWriteNode` (`A::B = 5`) behaves the same: no assignment.
        let path = analyze_source("foo.rb", "A::B = 5\n");
        assert!(path.is_empty());
        let call = analyze_source("foo.rb", "X = foo\n");
        let call_entry = entry_for(&call, "main#none");
        assert!(approx(call_entry.assignments, 0.0));
        assert!(approx(call_entry.calls, 1.0));
    }

    #[test]
    fn zero_is_exempt_magic_number() {
        let entries = analyze_source("foo.rb", "def x; i = 0; end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.assignments, 1.0));
        assert!(approx(entry.calls, 0.0));
    }

    #[test]
    fn block_pass_symbol() {
        let entries = analyze_source("foo.rb", "def x; foo(&:bar); end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.calls, 2.0)); // foo + block_pass
    }

    #[test]
    fn block_pass_to_proc_icky() {
        let entries = analyze_source("foo.rb", "def x; foo(&proc { 1 }); end\n");
        let entry = entry_for(&entries, "main#x");
        // foo(1) + block_pass(1) + to_proc_icky(10) + proc call(1) + magic 1 at 0.1 penalty (0.275)
        assert!(approx(entry.calls, 1.0 + 1.0 + 10.0 + 1.0 + 0.275));
        assert!(approx(entry.branches, 1.0)); // block_call for the proc block
    }

    #[test]
    fn safe_navigation_and_argument_penalties() {
        let entries = analyze_source("foo.rb", "def x; a&.b(c.d); end\n");
        let entry = entry_for(&entries, "main#x");
        // b 1.0; receiver a at +0.3 -> 1.3; arg c.d at +0.2: d 1.2, c at +0.2 -> 1.4.
        // Additive penalties: 1.0 + 1.3 + 1.2 + 1.4 = 4.9
        assert!(approx(entry.calls, 4.9));
    }

    #[test]
    fn self_method_dot_separator() {
        let entries = analyze_source("foo.rb", "class Foo; def self.x; end; end\n");
        assert!(entries.iter().any(|entry| entry.name == "Foo.x"));
    }

    #[test]
    fn singleton_class_def_is_dot_separated() {
        let entries = analyze_source("foo.rb", "class Foo; class << self; def y; 1; end; end; end\n");
        let none = entry_for(&entries, "Foo#none");
        assert!(approx(none.calls, 5.0)); // sclass
        let y = entry_for(&entries, "Foo.y");
        // magic 1 at +0.5 sclass-body penalty: 0.25 * 1.5 = 0.375
        assert!(approx(y.calls, 0.375));
    }

    #[test]
    fn operator_and_or_writes() {
        let entries = analyze_source("foo.rb", "def x; a ||= 2; h[:k] += 1; end\n");
        let entry = entry_for(&entries, "main#x");
        // a ||= 2: +1 a, magic 2 (+0.25)
        // h[:k] += 1: IndexOperatorWrite +1 a +1 c (operator), receiver h is an unresolved
        // bareword call scored at 1.0, magic 1 (+0.25)
        assert!(approx(entry.assignments, 2.0));
        assert!(approx(entry.calls, 1.0 + 1.0 + 0.25 + 0.25));
    }

    #[test]
    fn nested_class_name_join() {
        let entries = analyze_source("foo.rb", "module A; class B::C; def m; end; end; end\n");
        assert!(entries.iter().any(|entry| entry.name == "A::B::C#m"));
    }

    #[test]
    fn report_json_round_trip() {
        let report = build_report(analyze_source("foo.rb", "def foo; end\n"), false);
        let json = report.to_json();
        let parsed = Report::from_json(&json).expect("round trip");
        assert_eq!(parsed.schema_version, report.schema_version);
        assert_eq!(parsed.methods_count, report.methods_count);
        assert!(approx(parsed.total, report.total));
    }

    #[test]
    fn report_from_json_garbage_is_report_error() {
        let result = Report::from_json("garbage");
        assert!(matches!(result, Err(Errors::ReportError(_))));
    }

    #[test]
    fn diff_partitions_and_orders() {
        let baseline = build_report(analyze_source("foo.rb", "class Foo; def bar; 1; end; end\n"), false);
        let current = build_report(
            analyze_source("foo.rb", "class Foo; def bar; if a; 1; end; end; def baz; end; end\n"),
            false,
        );
        let diff = Report::diff(&baseline, &current).expect("diff");

        assert!(diff.total_delta > 0.0);
        // `bar` grew (added a branch) -> regression
        assert!(
            diff.changed
                .iter()
                .any(|entry| entry.name == "Foo#bar" && entry.delta > 0.0)
        );
        // `baz` is new -> added
        assert!(diff.added.iter().any(|entry| entry.name == "Foo#baz"));
        assert!(diff.removed.is_empty());
    }

    #[test]
    fn diff_threshold_ignores_tiny_changes() {
        let baseline = build_report(analyze_source("foo.rb", "def x; 1; end\n"), false);
        let current = build_report(analyze_source("foo.rb", "def x; 1; end\n"), false);
        let diff = Report::diff(&baseline, &current).expect("diff");
        assert!(diff.changed.is_empty());
    }
    // --- flog-parity regression tests -----------------------------------------
    // These pin scoring behaviors verified to match `flog -a` exactly. Each
    // comment records the flog total so a future change that drifts is caught.

    #[test]
    fn toplevel_block_penalty_propagates() {
        // flog wraps a multi-statement file (`require` + `class`) in a `:block`;
        // `process_block` adds +0.1 and `process_defn` does NOT reset, so every
        // method inherits the +0.1. `foo`/`bar` score at 1.1, not 1.0.
        let src = "require \"x\"\nclass Foo\n  def a; foo; end\n  def b; bar; end\nend\n";
        let entries = analyze_source("foo.rb", src);
        assert!(approx(entry_for(&entries, "Foo#a").calls, 1.1));
        assert!(approx(entry_for(&entries, "Foo#b").calls, 1.1));
    }

    #[test]
    fn toplevel_single_statement_no_penalty() {
        // A single top-level class is not a `:block` -> no +0.1. `foo` at 1.0.
        let entries = analyze_source("foo.rb", "class Foo\n  def bar; foo; end\nend\n");
        assert!(approx(entry_for(&entries, "Foo#bar").calls, 1.0));
    }

    #[test]
    fn block_multistatement_body_penalty() {
        // `tap do; foo; bar; end`: block_call +0.1, and the multi-statement body
        // adds flog's `process_block` +0.1 -> foo/bar at 1.2. flog total 3.5.
        let entries = analyze_source("foo.rb", "def x; tap do; foo; bar; end; end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.branches, 1.0));
        assert!(approx(entry.calls, 1.0 + 1.2 + 1.2)); // tap + foo + bar
        assert!(approx(entry.score, 3.54));
    }

    #[test]
    fn block_single_statement_body() {
        // Single-statement block body -> no extra `process_block` +0.1. foo at 1.1.
        let entries = analyze_source("foo.rb", "def x; tap do; foo; end; end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.calls, 1.0 + 1.1)); // tap + foo
        assert!(approx(entry.score, 2.33));
    }

    #[test]
    fn if_multistatement_body_penalty() {
        let entries = analyze_source("foo.rb", "def x; if c; foo; bar; end; end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.branches, 1.0));
        assert!(approx(entry.calls, 1.0 + 1.2 + 1.2)); // c + foo + bar
        assert!(approx(entry.score, 3.54));
    }

    #[test]
    fn if_else_does_not_add_branch() {
        let entries = analyze_source("foo.rb", "def x; if c; foo; else; bar; end; end\n");
        let entry = entry_for(&entries, "main#x");
        // flog's `process_if` handles the else inline (not `process_else`) -> no
        // branch for the else. b=1.0. foo/bar at 1.1 (if +0.1 only). flog 3.4.
        assert!(approx(entry.branches, 1.0));
        assert!(approx(entry.calls, 1.0 + 1.1 + 1.1)); // c + foo + bar
        assert!(approx(entry.score, 3.35));
    }

    #[test]
    fn case_else_does_not_add_branch() {
        // ruby_parser does not wrap case-else in `:else`, so flog adds no branch
        // for it. b = case(1.0) + when(1.1) = 2.1. flog 4.2.
        let entries = analyze_source("foo.rb", "def x; case m; when 1; foo; else; bar; end; end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.branches, 2.1));
    }

    #[test]
    fn when_condition_is_penalized() {
        // flog's `process_when` = `process_else`: the +0.1 wraps BOTH condition
        // and body, so the condition call lands at case+0.1 + when+0.1 = 1.2.
        let entries = analyze_source("foo.rb", "def x; case m; when foo; bar; end; end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.branches, 2.1));
        assert!(approx(entry.calls, 3.4));
        assert!(approx(entry.score, 4.0));
    }

    #[test]
    fn hash_scores_values_not_pairs() {
        // flog scores hash VALUES (calls/magic), never the pairs themselves.
        // `{ a: foo }` -> foo call only (1.0); `{ a: :b }` -> nothing (0.0).
        let e1_entries = analyze_source("foo.rb", "def x; { a: foo }; end\n");
        assert!(approx(entry_for(&e1_entries, "main#x").calls, 1.0));
        let e2_entries = analyze_source("foo.rb", "def x; { a: :b }; end\n");
        assert!(approx(entry_for(&e2_entries, "main#x").calls, 0.0));
    }
    // --- rescue lowering & parameter parity (flog) ----------------------------

    #[test]
    fn rescue_single_clause() {
        // `begin/rescue` -> flog's `:rescue`: ONE branch + +0.1 wrapping the
        // protected body and the clause. foo/bar at 1.1. flog 2.4.
        let entries = analyze_source("foo.rb", "def x; begin; foo; rescue; bar; end; end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.branches, 1.0));
        assert!(approx(entry.calls, 1.1 + 1.1));
        assert!(approx(entry.score, 2.42));
    }

    #[test]
    fn rescue_implicit_begin() {
        // A method body with rescue is an implicit BeginNode; same scoring.
        let entries = analyze_source("foo.rb", "def x; foo; rescue; bar; end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.branches, 1.0));
        assert!(approx(entry.calls, 1.1 + 1.1));
    }

    #[test]
    fn rescue_multi_clause_is_one_branch() {
        // Multiple rescue clauses share the single `:rescue` branch; clauses are
        // unscored `:resbody`. b=1.0, foo/bar/baz at 1.1. flog 3.4.
        let src = "def x; begin; foo; rescue KeyError; bar; rescue StandardError; baz; end; end\n";
        let entries = analyze_source("foo.rb", src);
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.branches, 1.0));
        assert!(approx(entry.calls, 1.1 + 1.1 + 1.1));
    }

    #[test]
    fn rescue_modifier() {
        // `foo rescue bar` lowers to a `:rescue`: one branch + +0.1. flog 2.4.
        let entries = analyze_source("foo.rb", "def x; foo rescue bar; end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.branches, 1.0));
        assert!(approx(entry.calls, 1.1 + 1.1));
        assert!(approx(entry.score, 2.42));
    }

    #[test]
    fn rescue_else_no_extra_branch() {
        // The rescue `else` adds no branch; its body inherits the rescue +0.1.
        let src = "def x; begin; foo; rescue; bar; else; baz; end; end\n";
        let entries = analyze_source("foo.rb", src);
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.branches, 1.0));
        assert!(approx(entry.calls, 1.1 + 1.1 + 1.1));
    }

    #[test]
    fn rescue_ensure_outside_penalty() {
        // `ensure` wraps `:rescue`; the ensure body is NOT penalized (baz at 1.0).
        let src = "def x; begin; foo; rescue; bar; ensure; baz; end; end\n";
        let entries = analyze_source("foo.rb", src);
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.branches, 1.0));
        assert!(approx(entry.calls, 1.1 + 1.1 + 1.0)); // foo + bar + baz(base)
    }

    #[test]
    fn rescue_binds_exception_variable() {
        // `=> e` is an assignment at the rescue +0.1 (1.1). flog 2.7.
        let src = "def x; begin; foo; rescue KeyError => e; bar; end; end\n";
        let entries = analyze_source("foo.rb", src);
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.assignments, 1.1));
        assert!(approx(entry.branches, 1.0));
        assert!(approx(entry.calls, 1.1 + 1.1));
    }

    #[test]
    fn keyword_optional_param_is_not_an_assignment() {
        // flog scores the DEFAULT value only, not the keyword binding. `a: nil`
        // contributes 0 assignments. flog 1.0.
        let entries = analyze_source("foo.rb", "def x(a: nil); foo; end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.assignments, 0.0));
        assert!(approx(entry.calls, 1.0));
    }

    #[test]
    fn positional_optional_param_is_an_assignment() {
        // Unlike keyword optional, flog scores positional optional (`a = 1`) as an
        // assignment AND scores the default value. a=1.0, c=foo(1.0)+magic(0.25).
        let entries = analyze_source("foo.rb", "def x(a = 1); foo; end\n");
        let entry = entry_for(&entries, "main#x");
        assert!(approx(entry.assignments, 1.0));
        assert!(approx(entry.calls, 1.25));
    }

    // --- --details / --methods-only / --group ---------------------------------

    #[test]
    fn details_collects_per_construct_breakdown() {
        // flog -d on `def x; a += 1; end` shows assignment 1.0, `+` 1.0, magic_number 0.25.
        let entries = analyze_source_with("foo.rb", "def x; a += 1; end\n", false, true);
        let entry = entry_for(&entries, "main#x");
        let labels: Vec<&str> = entry.details.iter().map(|d| d.label.as_str()).collect();
        assert!(labels.contains(&"assignment"));
        assert!(labels.contains(&"+"));
        assert!(labels.contains(&"magic_number"));
        let by_label = |l: &str| entry.details.iter().find(|d| d.label == l).map(|d| d.score).unwrap();
        assert!(approx(by_label("assignment"), 1.0));
        assert!(approx(by_label("+"), 1.0));
        assert!(approx(by_label("magic_number"), 0.25));
    }

    #[test]
    fn details_are_empty_when_collection_is_off() {
        let entries = analyze_source_with("foo.rb", "def x; a += 1; end\n", false, false);
        let entry = entry_for(&entries, "main#x");
        assert!(entry.details.is_empty());
        // Buckets are still populated.
        assert!(approx(entry.assignments, 1.0));
        assert!(approx(entry.calls, 1.25));
    }

    #[test]
    fn details_label_call_name_and_block_pass() {
        // `foo(&:bar)` -> block_pass 1.0 + foo 1.0.
        let entries = analyze_source_with("foo.rb", "def x; foo(&:bar); end\n", false, true);
        let entry = entry_for(&entries, "main#x");
        let by_label = |l: &str| entry.details.iter().find(|d| d.label == l).map(|d| d.score).unwrap();
        assert!(approx(by_label("block_pass"), 1.0));
        assert!(approx(by_label("foo"), 1.0));
    }

    #[test]
    fn methods_only_skips_out_of_method_code() {
        // `class Foo; include Bar; end` -> normally Foo#none (include 2.0); with -m, no entry.
        let entries = analyze_source_with("foo.rb", "class Foo; include Bar; end\n", true, false);
        assert!(entries.is_empty());
        // Total stays zero.
        let report = build_report(
            analyze_source_with("foo.rb", "class Foo; include Bar; end\n", true, false),
            true,
        );
        assert!(approx(report.total, 0.0));
        assert_eq!(report.methods_count, 0);
    }

    #[test]
    fn methods_only_keeps_real_def_none() {
        // A real `def none` is inside a method, so -m keeps it; out-of-method `baz` is dropped.
        let entries = analyze_source_with("foo.rb", "class Foo; def none; bar; end; baz; end\n", true, false);
        let entry = entry_for(&entries, "Foo#none");
        assert!(approx(entry.calls, 1.0)); // only `bar`; `baz` (out of method) is skipped
    }

    #[test]
    fn methods_only_recorded_in_report() {
        let report = build_report(analyze_source_with("foo.rb", "def x; end\n", true, false), true);
        assert!(report.methods_only);
        let normal = build_report(analyze_source("foo.rb", "def x; end\n"), false);
        assert!(!normal.methods_only);
    }

    #[test]
    fn diff_rejects_mismatched_methods_only() {
        let baseline = build_report(analyze_source("foo.rb", "class Foo; include Bar; end\n"), false);
        let current = build_report(
            analyze_source_with("foo.rb", "class Foo; include Bar; end\n", true, false),
            true,
        );
        assert!(matches!(Report::diff(&baseline, &current), Err(Errors::ReportError(_))));
    }

    #[test]
    fn group_render_emits_subtotals() {
        let report = build_report(
            analyze_source(
                "foo.rb",
                "class Foo; def a; 1; end; def b; 2; end; end\nclass Bar; def c; 1; end; end\n",
            ),
            false,
        );
        let text = report.render_text(0, false, true);
        assert!(text.contains("Foo total"));
        assert!(text.contains("Bar total"));
        // Foo has two methods (a, b) so its subtotal ranks above Bar's single method.
        let foo_idx = text.find("Foo total").unwrap();
        let bar_idx = text.find("Bar total").unwrap();
        assert!(foo_idx < bar_idx, "Foo (2 methods) should be grouped before Bar (1)");
    }

    #[test]
    fn group_respects_global_top_cap() {
        // 3 methods across 2 classes; --top 2 must emit at most 2 method rows total.
        let report = build_report(
            analyze_source(
                "foo.rb",
                "class Foo; def a; 1; end; def b; 2; end; end\nclass Bar; def c; 1; end; end\n",
            ),
            false,
        );
        let text = report.render_text(2, false, true);
        // Count method rows: lines that contain `foo.rb:` (each entry line has the file).
        let method_rows = text.lines().filter(|l| l.contains("foo.rb:")).count();
        assert!(method_rows <= 2, "group mode emitted {method_rows} rows, expected <= 2");
    }

    #[test]
    fn details_render_under_each_method() {
        let report = build_report(
            analyze_source_with("foo.rb", "def x; a += 1; end\n", false, true),
            false,
        );
        let text = report.render_text(0, true, false);
        assert!(text.contains("main#x"));
        assert!(text.contains("assignment"));
        assert!(text.contains("magic_number"));
    }
}

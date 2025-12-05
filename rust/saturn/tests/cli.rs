use assert_cmd::prelude::*;
use predicates::prelude::*;
use regex::Regex;
use saturn::test_utils::with_context;
use std::process::Command;

#[test]
fn prints_help() {
    let mut cmd = Command::cargo_bin("saturn_cli").unwrap();
    cmd.arg("--help");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("A Static Analysis Toolkit for Ruby"))
        .stdout(predicate::str::contains("Usage:"))
        .stdout(predicate::str::contains("--visualize"));
}

#[test]
fn dir_argument_variants() {
    let mut zero = Command::cargo_bin("saturn_cli").unwrap();
    zero.assert().success().stderr(predicate::str::is_empty());

    let mut one = Command::cargo_bin("saturn_cli").unwrap();
    one.arg(".");
    one.assert().success().stderr(predicate::str::is_empty());

    let mut two = Command::cargo_bin("saturn_cli").unwrap();
    two.args(["foo", "bar"]);
    two.assert()
        .failure()
        .stderr(predicate::str::contains("unexpected argument").or(predicate::str::contains("error:")));
}

#[test]
fn prints_index_metrics() {
    with_context(|context| {
        context.write("file1.rb", "class FirstClass\nend\n");
        context.write("file2.rb", "module SecondModule\nend\n");

        let mut cmd = Command::cargo_bin("saturn_cli").unwrap();
        cmd.arg(context.absolute_path());
        let output = cmd.output().unwrap();

        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("Indexed 2 files"));
        assert!(stdout.contains("Found 3 names"));
        assert!(stdout.contains("Found 2 definitions"));
    });
}

fn normalize_visualization_output(output: &str) -> String {
    let def_re = Regex::new(r"def_-?[a-f0-9]+").unwrap();
    let uri_re = Regex::new(r#"file://[^"]+/([^/"]+\.rb)"#).unwrap();

    let normalized = def_re.replace_all(output, "def_<ID>");
    uri_re.replace_all(&normalized, "file://<PATH>/$1").to_string()
}

#[test]
fn visualize_simple_class() {
    with_context(|context| {
        context.write("simple.rb", "class SimpleClass\nend\n");

        let mut cmd = Command::cargo_bin("saturn_cli").unwrap();
        cmd.args([context.absolute_path().to_str().unwrap(), "--visualize"]);

        let output = cmd.output().unwrap();
        assert!(output.status.success());

        let stdout = String::from_utf8_lossy(&output.stdout);
        let normalized = normalize_visualization_output(&stdout);

        let expected = r#"digraph {
    rankdir=TB;

    "Name:Object" [label="Object",shape=hexagon];
    "Name:SimpleClass" [label="SimpleClass",shape=hexagon];
    "Name:SimpleClass" -> "def_<ID>" [dir=both];

    "def_<ID>" [label="Class(SimpleClass)",shape=ellipse];

    "file://<PATH>/simple.rb" [label="simple.rb",shape=box];
    "def_<ID>" -> "file://<PATH>/simple.rb";

}

"#;

        assert_eq!(normalized, expected);
    });
}

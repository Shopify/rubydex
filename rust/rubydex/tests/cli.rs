use assert_cmd::{assert::Assert, prelude::*};
use predicates::prelude::*;
use regex::Regex;
use rubydex::test_utils::{normalize_indentation, with_context};
use std::process::Command;

fn rdx_cmd(args: &[&str]) -> Command {
    let mut cmd = Command::cargo_bin("rubydex_cli").unwrap();
    cmd.args(args);
    cmd
}

fn rdx(args: &[&str]) -> Assert {
    rdx_cmd(args).assert()
}

#[test]
fn prints_help() {
    rdx(&["--help"])
        .success()
        .stdout(predicate::str::contains("A Static Analysis Toolkit for Ruby"))
        .stdout(predicate::str::contains("Usage:"))
        .stdout(predicate::str::contains("--stats"))
        .stdout(predicate::str::contains("--visualize"))
        .stdout(predicate::str::contains("--stop-after"));
}

#[test]
fn paths_argument_variants() {
    rdx(&[])
        .success()
        .stderr(predicate::str::is_empty())
        .stdout(predicate::str::contains("Indexed 0 files"));

    rdx(&["."])
        .success()
        .stderr(predicate::str::is_empty())
        .stdout(predicate::str::contains("Indexed 0 files"));

    with_context(|context| {
        context.write("dir1/file1.rb", "class Class1\nend\n");
        context.write("dir1/file2.rb", "class Class2\nend\n");
        context.write("dir2/file1.rb", "class Class3\nend\n");
        context.write("dir2/file2.rb", "class Class4\nend\n"); // not indexed

        rdx(&[
            context.absolute_path_to("dir1").to_str().unwrap(),
            context.absolute_path_to("dir2/file1.rb").to_str().unwrap(),
        ])
        .success()
        .stderr(predicate::str::is_empty())
        .stdout(predicate::str::contains("Indexed 3 files"));
    });
}

#[test]
fn prints_index_metrics() {
    with_context(|context| {
        context.write("file1.rb", "class FirstClass\nend\n");
        context.write("file2.rb", "module SecondModule\nend\n");

        rdx(&[context.absolute_path().to_str().unwrap()])
            .success()
            .stderr(predicate::str::is_empty())
            .stdout(predicate::str::contains("Indexed 2 files"))
            .stdout(predicate::str::contains("Found 5 names"))
            .stdout(predicate::str::contains("Found 2 definitions"));
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

        let output = rdx_cmd(&[context.absolute_path().to_str().unwrap(), "--visualize"])
            .output()
            .unwrap();

        assert!(output.status.success());

        let stdout = String::from_utf8_lossy(&output.stdout);
        let normalized = normalize_visualization_output(&stdout);

        let expected = normalize_indentation({
            r#"
            digraph {
                rankdir=TB;

                "Name:Class" [label="Class",shape=hexagon];
                "Name:Module" [label="Module",shape=hexagon];
                "Name:Object" [label="Object",shape=hexagon];
                "Name:SimpleClass" [label="SimpleClass",shape=hexagon];
                "Name:SimpleClass" -> "def_<ID>" [dir=both];

                "def_<ID>" [label="class(SimpleClass)",shape=ellipse];

                "file://<PATH>/simple.rb" [label="simple.rb",shape=box];
                "def_<ID>" -> "file://<PATH>/simple.rb";

            }

            "#
        });

        assert_eq!(normalized, expected);
    });
}

#[test]
fn stop_after() {
    with_context(|context| {
        context.write("file1.rb", "class Class1\nend\n");
        context.write("file2.rb", "class Class2\nend\n");

        rdx(&[
            context.absolute_path().to_str().unwrap(),
            "--stop-after",
            "listing",
            "--stats",
        ])
        .success()
        .stdout(predicate::str::contains("Listing"))
        .stdout(predicate::str::contains("Indexing").not())
        .stdout(predicate::str::contains("Resolution").not())
        .stdout(predicate::str::contains("Querying").not());

        rdx(&[
            context.absolute_path().to_str().unwrap(),
            "--stop-after",
            "indexing",
            "--stats",
        ])
        .success()
        .stdout(predicate::str::contains("Listing"))
        .stdout(predicate::str::contains("Indexing"))
        .stdout(predicate::str::contains("Resolution").not())
        .stdout(predicate::str::contains("Querying").not());

        rdx(&[
            context.absolute_path().to_str().unwrap(),
            "--stop-after",
            "resolution",
            "--stats",
        ])
        .success()
        .stdout(predicate::str::contains("Listing"))
        .stdout(predicate::str::contains("Indexing"))
        .stdout(predicate::str::contains("Resolution"))
        .stdout(predicate::str::contains("Querying").not());

        rdx(&[context.absolute_path().to_str().unwrap(), "--stats"])
            .success()
            .stdout(predicate::str::contains("Listing"))
            .stdout(predicate::str::contains("Indexing"))
            .stdout(predicate::str::contains("Resolution"))
            .stdout(predicate::str::contains("Querying"));
    });
}

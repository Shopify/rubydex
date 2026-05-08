use assert_cmd::{assert::Assert, prelude::*};
use predicates::prelude::*;
use rubydex::test_utils::with_context;
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
        .stdout(predicate::str::contains("--dot"))
        .stdout(predicate::str::contains("--stop-after"));
}

#[test]
fn paths_argument_variants() {
    rdx(&[])
        .success()
        .stderr(predicate::str::is_empty())
        .stdout(predicate::str::contains("Indexed 1 files"));

    rdx(&["."])
        .success()
        .stderr(predicate::str::is_empty())
        .stdout(predicate::str::contains("Indexed 1 files"));

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
        .stdout(predicate::str::contains("Indexed 4 files"));
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
            .stdout(predicate::str::contains("Indexed 3 files"))
            .stdout(predicate::str::contains("Found 7 names"))
            .stdout(predicate::str::contains("Found 7 definitions"));
    });
}

#[test]
fn dot_flag() {
    with_context(|context| {
        context.write("simple.rb", "class SimpleClass\nend\n");

        rdx(&[context.absolute_path().to_str().unwrap(), "--dot"])
            .success()
            .stdout(predicate::str::contains("digraph rubydex"))
            // Document node
            .stdout(predicate::str::contains("Document"))
            .stdout(predicate::str::contains("simple.rb"))
            // Definition node
            .stdout(predicate::str::contains("ClassDef"))
            .stdout(predicate::str::contains("SimpleClass"))
            // Declaration node
            .stdout(predicate::str::contains("ClassDecl"))
            // Edges
            .stdout(predicate::str::contains("defines"))
            .stdout(predicate::str::contains("declares"));
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

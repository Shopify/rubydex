use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::process::Command;

#[test]
fn prints_help() {
    let mut cmd = Command::cargo_bin("index_cli").unwrap();
    cmd.arg("--help");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("A Ruby code indexer"));
}

#[test]
fn dir_argument_variants() {
    let mut zero = Command::cargo_bin("index_cli").unwrap();
    zero.assert().success().stderr(predicate::str::is_empty());

    let mut one = Command::cargo_bin("index_cli").unwrap();
    one.arg(".");
    one.assert().success().stderr(predicate::str::is_empty());

    let mut two = Command::cargo_bin("index_cli").unwrap();
    two.args(["foo", "bar"]);
    two.assert()
        .failure()
        .stderr(predicate::str::contains("unexpected argument").or(predicate::str::contains("error:")));
}

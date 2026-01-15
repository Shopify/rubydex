use assert_cmd::prelude::*;
use rubydex::test_utils::with_context;
use serde_json::{Value, json};
use std::io::{BufRead, BufReader, Read, Write};
use std::process::{Command, Stdio};

const MAX_INDEXING_RETRIES: u64 = 60;

/// Send a JSON-RPC message followed by a newline to the process stdin.
fn send_message(stdin: &mut impl Write, msg: &Value) {
    let payload = serde_json::to_string(msg).unwrap();
    writeln!(stdin, "{payload}").unwrap();
    stdin.flush().unwrap();
}

/// Read a single JSON-RPC response line from the process stdout.
fn read_response(reader: &mut BufReader<impl std::io::Read>) -> Value {
    let mut line = String::new();
    reader.read_line(&mut line).unwrap();
    serde_json::from_str(line.trim()).unwrap()
}

fn send_request(stdin: &mut impl Write, id: u64, method: &str, params: &Value) {
    send_message(
        stdin,
        &json!({
            "jsonrpc": "2.0",
            "id": id,
            "method": method,
            "params": params,
        }),
    );
}

fn read_response_for_id(reader: &mut BufReader<impl Read>, expected_id: u64) -> Value {
    let response = read_response(reader);
    assert_eq!(response["id"], expected_id);
    response
}

fn call_tool(
    stdin: &mut impl Write,
    reader: &mut BufReader<impl Read>,
    request_id: u64,
    tool_name: &str,
    arguments: &Value,
) -> Value {
    send_request(
        stdin,
        request_id,
        "tools/call",
        &json!({
            "name": tool_name,
            "arguments": arguments,
        }),
    );

    let response = read_response_for_id(reader, request_id);
    let content_text = response["result"]["content"][0]["text"].as_str().unwrap();
    serde_json::from_str(content_text).unwrap()
}

fn call_next_tool(
    stdin: &mut impl Write,
    reader: &mut BufReader<impl Read>,
    request_id: &mut u64,
    tool_name: &str,
    arguments: &Value,
) -> Value {
    *request_id += 1;
    call_tool(stdin, reader, *request_id, tool_name, arguments)
}

fn names_from_entries(entries: &[Value]) -> Vec<String> {
    entries
        .iter()
        .filter_map(|entry| entry["name"].as_str().map(ToOwned::to_owned))
        .collect()
}

fn assert_has_name(names: &[String], expected_name: &str, context: &str) {
    assert!(
        names.iter().any(|name| name == expected_name),
        "Expected {context} to include {expected_name}, got: {names:?}"
    );
}

fn initialize_session(stdin: &mut impl Write, reader: &mut BufReader<impl Read>) {
    send_request(
        stdin,
        1,
        "initialize",
        &json!({
            "protocolVersion": "2025-03-26",
            "capabilities": {},
            "clientInfo": { "name": "test-client", "version": "0.1.0" }
        }),
    );

    let response = read_response_for_id(reader, 1);
    assert!(response["result"]["capabilities"]["tools"].is_object());

    send_message(
        stdin,
        &json!({
            "jsonrpc": "2.0",
            "method": "notifications/initialized"
        }),
    );
}

fn assert_tools_are_registered(stdin: &mut impl Write, reader: &mut BufReader<impl Read>) {
    send_request(stdin, 2, "tools/list", &json!({}));

    let response = read_response_for_id(reader, 2);
    let tools = response["result"]["tools"].as_array().unwrap();
    let tool_names: Vec<&str> = tools.iter().map(|tool| tool["name"].as_str().unwrap()).collect();

    assert!(
        tool_names.contains(&"search_declarations"),
        "Missing search_declarations tool"
    );
    assert!(tool_names.contains(&"get_declaration"), "Missing get_declaration tool");
    assert!(tool_names.contains(&"get_descendants"), "Missing get_descendants tool");
    assert!(
        tool_names.contains(&"find_constant_references"),
        "Missing find_constant_references tool"
    );
    assert!(
        tool_names.contains(&"get_file_declarations"),
        "Missing get_file_declarations tool"
    );
    assert!(tool_names.contains(&"codebase_stats"), "Missing codebase_stats tool");
    assert_eq!(tool_names.len(), 6, "Expected exactly 6 tools");
}

fn wait_for_indexing_to_complete(
    stdin: &mut impl Write,
    reader: &mut BufReader<impl Read>,
    request_id: &mut u64,
) -> Value {
    for _ in 0..MAX_INDEXING_RETRIES {
        let parsed = call_tool(stdin, reader, *request_id, "codebase_stats", &json!({}));
        if parsed.get("error").is_none() {
            return parsed;
        }

        assert_eq!(
            parsed["error"].as_str(),
            Some("indexing"),
            "Expected transient indexing error while booting, got: {parsed}"
        );

        *request_id += 1;
        std::thread::sleep(std::time::Duration::from_millis(50));
    }

    panic!("Timed out waiting for indexing to complete");
}

#[test]
#[allow(clippy::too_many_lines)]
fn mcp_server_e2e() {
    with_context(|context| {
        context.write(
            "app.rb",
            r#"
                class Animal
                  def speak
                    "..."
                  end
                end

                class Dog < Animal
                  def speak
                    "Woof!"
                  end
                end

                module Greetable
                  def greet
                    "Hello"
                  end
                end

                class Kennel
                  def build
                    Animal.new
                  end
                end
            "#,
        );

        let mut child = Command::cargo_bin("rubydex_mcp")
            .unwrap()
            .args([context.absolute_path().to_str().unwrap()])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();

        let mut stdin = child.stdin.take().unwrap();
        let stdout = child.stdout.take().unwrap();
        let mut reader = BufReader::new(stdout);

        initialize_session(&mut stdin, &mut reader);
        assert_tools_are_registered(&mut stdin, &mut reader);

        // Wait for indexing readiness before asserting semantic tool results.
        let mut request_id = 3;
        let stats = wait_for_indexing_to_complete(&mut stdin, &mut reader, &mut request_id);
        assert_eq!(stats["files"], 1);
        assert!(stats["declarations"].as_u64().unwrap() > 0);

        // Semantic query: search declarations.
        let results: Vec<Value> = serde_json::from_value(call_next_tool(
            &mut stdin,
            &mut reader,
            &mut request_id,
            "search_declarations",
            &json!({ "query": "Dog" }),
        ))
        .unwrap();
        let result_names = names_from_entries(&results);
        assert_has_name(&result_names, "Dog", "search results");

        // Semantic query: inspect declaration details.
        let decl = call_next_tool(
            &mut stdin,
            &mut reader,
            &mut request_id,
            "get_declaration",
            &json!({ "name": "Dog" }),
        );
        assert_eq!(decl["name"], "Dog");
        assert_eq!(decl["kind"], "Class");
        assert!(!decl["definitions"].as_array().unwrap().is_empty());

        // Verify ancestors are included in get_declaration response
        let ancestor_entries = decl["ancestors"].as_array().unwrap();
        let ancestor_names = names_from_entries(ancestor_entries);
        assert_has_name(&ancestor_names, "Animal", "Dog ancestors");

        // Semantic query: descendants.
        let descendants = call_next_tool(
            &mut stdin,
            &mut reader,
            &mut request_id,
            "get_descendants",
            &json!({ "name": "Animal" }),
        );
        let descendant_entries = descendants["descendants"].as_array().unwrap();
        let descendant_names = names_from_entries(descendant_entries);
        assert_has_name(&descendant_names, "Dog", "Animal descendants");

        // Semantic query: resolved constant references.
        let references = call_next_tool(
            &mut stdin,
            &mut reader,
            &mut request_id,
            "find_constant_references",
            &json!({ "name": "Animal" }),
        );
        let refs = references["references"].as_array().unwrap();
        assert!(
            !refs.is_empty(),
            "Expected at least one reference to Animal, got: {references}"
        );
        assert!(
            refs.iter().all(|entry| entry["path"].as_str().is_some()),
            "Expected references to include file paths, got: {references}"
        );

        // Semantic query: file declarations.
        let file_declarations = call_next_tool(
            &mut stdin,
            &mut reader,
            &mut request_id,
            "get_file_declarations",
            &json!({ "file_path": "app.rb" }),
        );
        assert!(
            file_declarations["file"]
                .as_str()
                .is_some_and(|path| path.ends_with("app.rb")),
            "Expected file path to end with app.rb, got: {file_declarations}"
        );
        let declaration_entries = file_declarations["declarations"].as_array().unwrap();
        let declaration_names = names_from_entries(declaration_entries);
        assert_has_name(&declaration_names, "Animal", "file declarations");
        assert_has_name(&declaration_names, "Dog", "file declarations");
        assert_has_name(&declaration_names, "Greetable", "file declarations");

        // Clean up: drop stdin to signal EOF, then wait for the process to exit
        drop(stdin);
        let _ = child.wait().unwrap();
    });
}

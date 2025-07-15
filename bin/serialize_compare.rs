use index::comments::CommentData;
use prost::Message;
use std::collections::HashMap;
use std::{fs, usize};

// Include the generated prost code
include!(concat!(env!("OUT_DIR"), "/comments.rs"));

fn main() {
    println!("💾 Writing serialization format files...");
    println!("========================================\n");

    // Generate test data - same as your original script
    let mut comments = HashMap::new();

    // Create output directory
    fs::create_dir_all("tmp/serialize_compare").expect("Failed to create directory");
    //  1 files for core
    for i in 0..1 {
        println!("{}", i.to_string());
        // 100 comments per file
        for j in 0..100 {
            let entry_name = format!("entry_{}", j);
            let comment = CommentData::new(entry_name.clone(), 100);
            comments.insert(entry_name, comment);
        }

        // Write each format to a file
        write_json(&comments, i);
        write_messagepack(&comments, i);
        write_bincode(&comments, i);
        write_postcard(&comments, i);
        write_cbor(&comments, i);
        write_prost(&comments, i);
    }

    println!("\n✅ All files written to tmp/serialize_compare/");
    println!("Run: ls -la tmp/serialize_compare/ to see file sizes");
}

fn write_json(data: &HashMap<String, CommentData>, file_id: usize) {
    let serialized = serde_json::to_vec(data).unwrap();
    let path = format!("tmp/serialize_compare/data_{}.json", file_id);
    fs::write(path, &serialized).unwrap();
}

fn write_messagepack(data: &HashMap<String, CommentData>, file_id: usize) {
    let serialized = rmp_serde::to_vec(data).unwrap();
    let path = format!("tmp/serialize_compare/data_{}.msgpack", file_id);
    fs::write(path, &serialized).unwrap();
}

fn write_bincode(data: &HashMap<String, CommentData>, file_id: usize) {
    let serialized = bincode::serialize(data).unwrap();
    let path = format!("tmp/serialize_compare/data_{}.bincode", file_id);
    fs::write(path, &serialized).unwrap();
}

fn write_postcard(data: &HashMap<String, CommentData>, file_id: usize) {
    let serialized = postcard::to_allocvec(data).unwrap();
    let path = format!("tmp/serialize_compare/data_{}.postcard", file_id);
    fs::write(path, &serialized).unwrap();
}

fn write_cbor(data: &HashMap<String, CommentData>, file_id: usize) {
    let mut serialized = Vec::new();
    ciborium::into_writer(data, &mut serialized).unwrap();
    let path = format!("tmp/serialize_compare/data_{}.cbor", file_id);
    fs::write(path, &serialized).unwrap();
}

fn write_prost(data: &HashMap<String, CommentData>, file_id: usize) {
    // Convert HashMap<String, CommentData> to prost ProtoCommentCollection
    let mut prost_comments = HashMap::new();
    for (key, comment) in data {
        // Create ProtoCommentData by accessing fields through serde serialization
        let serialized = serde_json::to_string(comment).unwrap();
        let json_value: serde_json::Value = serde_json::from_str(&serialized).unwrap();

        prost_comments.insert(
            key.clone(),
            ProtoCommentData {
                text: json_value["text"].as_str().unwrap().to_string(),
                entry_name: json_value["entry_name"].as_str().unwrap().to_string(),
            },
        );
    }

    let collection = ProtoCommentCollection {
        comments: prost_comments,
    };

    let serialized = collection.encode_to_vec();

    let path = format!("tmp/serialize_compare/data_{}.prost", file_id);
    fs::write(path, &serialized).unwrap();
}

use index::comments::CommentData;
use std::collections::HashMap;
use std::fs;
use std::io::Write;

fn main() {
    println!("💾 Writing serialization format files...");
    println!("========================================\n");

    // Generate test data - same as your original script
    let mut comments = HashMap::new();
    for j in 0..10 {
        let entry_name = format!("entry_{}", j);
        let comment = CommentData::new(entry_name.clone(), 1000);
        comments.insert(entry_name, comment);
    }

    // Create output directory
    fs::create_dir_all("tmp/serialize_compare").expect("Failed to create directory");

    // Write each format to a file
    write_json(&comments);
    write_messagepack(&comments);
    write_bincode(&comments);
    write_postcard(&comments);
    write_cbor(&comments);

    println!("\n✅ All files written to tmp/serialize_compare/");
    println!("Run: ls -la tmp/serialize_compare/ to see file sizes");
}

fn write_json(data: &HashMap<String, CommentData>) {
    let serialized = serde_json::to_vec(data).unwrap();
    fs::write("tmp/serialize_compare/data.json", &serialized).unwrap();
    println!("📄 JSON: tmp/serialize_compare/data.json");
}

fn write_messagepack(data: &HashMap<String, CommentData>) {
    let serialized = rmp_serde::to_vec(data).unwrap();
    fs::write("tmp/serialize_compare/data.msgpack", &serialized).unwrap();
    println!("📦 MessagePack: tmp/serialize_compare/data.msgpack");
}

fn write_bincode(data: &HashMap<String, CommentData>) {
    let serialized = bincode::serialize(data).unwrap();
    fs::write("tmp/serialize_compare/data.bincode", &serialized).unwrap();
    println!("🔧 Bincode: tmp/serialize_compare/data.bincode");
}

fn write_postcard(data: &HashMap<String, CommentData>) {
    let serialized = postcard::to_allocvec(data).unwrap();
    fs::write("tmp/serialize_compare/data.postcard", &serialized).unwrap();
    println!("📮 Postcard: tmp/serialize_compare/data.postcard");
}

fn write_cbor(data: &HashMap<String, CommentData>) {
    let mut serialized = Vec::new();
    ciborium::into_writer(data, &mut serialized).unwrap();
    fs::write("tmp/serialize_compare/data.cbor", &serialized).unwrap();
    println!("🏗️ CBOR: tmp/serialize_compare/data.cbor");
}

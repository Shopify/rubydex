use index::comments::CommentData;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufWriter;

fn main() {
    // For 100 files
    for i in 0..100 {
        let mut comments = HashMap::new();
        // 100 comments per file
        for j in 0..100 {
            let entry_name = format!("entry_{}", j);
            let comment = CommentData::new(entry_name.clone(), 100);
            comments.insert(entry_name, comment);
        }
        let path = format!("tmp/serde_json/comment_{}.json", i);
        let f = File::create(&path).expect("Unable to create file at '{path}'");
        let bw = BufWriter::new(f);
        if serde_json::ser::to_writer(bw, &comments).is_ok() {
            println!("Successfully written model to '{path}'");
        } else {
            eprintln!("Error writting model to '{path}'");
        }
    }
}

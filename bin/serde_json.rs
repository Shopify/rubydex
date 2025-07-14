use index::comments::CommentData;
use std::fs::File;
use std::io::BufWriter;

fn main() {
    let comment = CommentData::new(String::from("1"));

    let path = "tmp/comment.json";
    let f = File::create(path).expect("Unable to create file at '{path}'");
    let bw = BufWriter::new(f);
    if serde_json::ser::to_writer(bw, &comment).is_ok() {
        println!("Successfully written model to '{path}'");
    } else {
        eprintln!("Error writting model to '{path}'");
    }
}

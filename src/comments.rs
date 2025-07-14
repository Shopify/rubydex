use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct CommentData {
    text: String,
    entry_name: String,
}

impl CommentData {
    pub fn new(entry_name: String) -> Self {
        Self {
            text: generate_comment(),
            entry_name,
        }
    }
}

fn generate_comment() -> String {
    let comment = "This is a sample comment";
    String::from(comment)
}

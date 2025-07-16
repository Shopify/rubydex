use std::usize;

use rand::{Rng, distributions::Alphanumeric};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct CommentData {
    text: String,
    entry_name: String,
}

impl CommentData {
    pub fn new(entry_name: String, comment_size: usize) -> Self {
        Self {
            text: generate_comment(comment_size),
            entry_name,
        }
    }
}

fn generate_comment(length: usize) -> String {
    rand::thread_rng()
        .sample_iter(&Alphanumeric)
        .take(length)
        .map(char::from)
        .collect()
}

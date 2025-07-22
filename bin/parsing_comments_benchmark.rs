use ruby_prism::Comment;

fn main() {
    let res = comments(124, "test.rb");
    println!("{}", res);
}

fn comments(entry_start_offset: usize, file_path: &str) -> String {
    let source = std::fs::read_to_string(&file_path).unwrap();
    let result = ruby_prism::parse(source.as_ref());

    let comments: Vec<ruby_prism::Comment> = result.comments().collect();

    for i in 0..comments.len() {
        let comment = &comments[i];

        // Match comments that end max 2 units before our target node's start offset
        if entry_start_offset > comment.location().end_offset()
            && entry_start_offset - comment.location().end_offset() <= 3
        {
            let mut j = i;
            let mut current_comment = &comments[j];
            let mut prev_comment = &comments[j - 1];
            let mut comment_group = String::from_utf8(current_comment.text().to_vec()).unwrap();
            while current_comment.location().start_offset() - prev_comment.location().end_offset() == 1 {
                j = j - 1;
                current_comment = prev_comment;
                prev_comment = &comments[j - 1];
                comment_group.insert_str(0, "\n");

                let current_comment_str = String::from_utf8(current_comment.text().to_vec()).unwrap();
                comment_group.insert_str(0, &current_comment_str);
            }
            // go backwards and collect comments until we find a comment that whose end offset
            // doesn't align with the current comment's start offset
            return comment_group;
        }
    }
    return String::from("");
}

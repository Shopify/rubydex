fn main() {
    let source = std::fs::read_to_string("test.rb").unwrap();

    let res = comments(123, source);
    println!("{}", res);
}

fn comments(entry_start_offset: usize, file_data: String) -> String {
    let result = ruby_prism::parse(file_data.as_ref());

    let mut comments: Vec<ruby_prism::Comment> = result.comments().collect();
    let mut prev_start_offset = None;
    let mut comment_group = String::from("");

    while let Some(comment) = comments.pop() {
        dbg!(entry_start_offset, comment.location().end_offset());
        if entry_start_offset > comment.location().end_offset()
            && entry_start_offset - comment.location().end_offset() < 3
        {
            comment_group = String::from_utf8(comment.text().to_vec()).unwrap();
            prev_start_offset = Some(comment.location().start_offset());
            break;
        }
    }

    if let Some(mut prev_start_offset) = prev_start_offset {
        while let Some(comment) = comments.pop() {
            if prev_start_offset - comment.location().end_offset() == 1 {
                comment_group.insert_str(
                    0,
                    format!("{}\n", String::from_utf8(comment.text().to_vec()).unwrap()).as_str(),
                );
                prev_start_offset = comment.location().start_offset();
            } else {
                break;
            }
        }
    }
    return comment_group;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn matches_comment_one_line_above_entry() {
        let source = r"
# This is a comment that should be matched
def foo
end";
        let result = comments(44, source.to_string());
        println!("{}", result);
        assert_eq!(result, "# This is a comment that should be matched");
    }

    #[test]
    fn matches_comment_two_lines_above_entry() {
        let source = r"
# This is a comment that should be matched

def foo
end";
        let result = comments(45, source.to_string());
        println!("{}", result);
        assert_eq!(result, "# This is a comment that should be matched");
    }

    #[test]
    fn matches_multiple_comments() {
        let source = r"
# This is a comment that should be matched
# And this one should be too
def foo
end";
        let result = comments(73, source.to_string());
        println!("{}", result);
        assert_eq!(
            result,
            "# This is a comment that should be matched\n# And this one should be too"
        );
    }

    #[test]
    fn does_not_match_comment_more_than_2_lines_above_entry() {
        let source = r"
# This comment should not be matched


def foo
end";
        let result = comments(41, source.to_string());
        println!("{}", result);
        assert_eq!(result, "");
    }
}

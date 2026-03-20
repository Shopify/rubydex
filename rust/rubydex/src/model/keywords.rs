/// A Ruby keyword with its documentation.
#[derive(Debug)]
pub struct Keyword {
    /// The keyword text as it appears in source (e.g., "yield", "defined?", "__FILE__")
    name: &'static str,
    /// Documentation string for hover display
    documentation: &'static str,
}

impl Keyword {
    #[must_use]
    pub const fn name(&self) -> &'static str {
        self.name
    }

    #[must_use]
    pub const fn documentation(&self) -> &'static str {
        self.documentation
    }
}

/// Looks up a keyword by its exact name. Returns `None` if the name is not a keyword.
///
/// Uses binary search on the sorted `KEYWORDS` array for O(log n) lookup.
#[must_use]
pub fn get(name: &str) -> Option<&'static Keyword> {
    KEYWORDS
        .binary_search_by_key(&name, |k| k.name)
        .ok()
        .map(|i| &KEYWORDS[i])
}

/// All Ruby keywords, sorted lexicographically by name for binary search.
pub static KEYWORDS: &[Keyword] = &[
    Keyword {
        name: "BEGIN",
        documentation: "Registers a block of code to be executed before the program starts. Syntax: `BEGIN { ... }`.",
    },
    Keyword {
        name: "END",
        documentation: "Registers a block of code to be executed after the program finishes. Syntax: `END { ... }`.",
    },
    Keyword {
        name: "__ENCODING__",
        documentation: "Returns the `Encoding` object representing the encoding of the current source file.",
    },
    Keyword {
        name: "__FILE__",
        documentation: "Returns the path of the current source file as a `String`.",
    },
    Keyword {
        name: "__LINE__",
        documentation: "Returns the current line number in the source file as an `Integer`.",
    },
    Keyword {
        name: "alias",
        documentation: "Creates an alias between two methods or global variables. Syntax: `alias new_name old_name`.",
    },
    Keyword {
        name: "and",
        documentation: "Low-precedence logical AND operator. Unlike `&&`, it has lower precedence than assignment.",
    },
    Keyword {
        name: "begin",
        documentation: "Opens an exception handling block. Can be followed by `rescue`, `else`, `ensure`, and closed with `end`.",
    },
    Keyword {
        name: "break",
        documentation: "Exits from a loop or block, optionally returning a value. Syntax: `break` or `break value`.",
    },
    Keyword {
        name: "case",
        documentation: "Starts a case expression for pattern matching. Used with `when` clauses and closed with `end`.",
    },
    Keyword {
        name: "class",
        documentation: "Defines a new class or opens an existing one. Syntax: `class Name < Superclass; end`.",
    },
    Keyword {
        name: "def",
        documentation: "Defines a method. Syntax: `def method_name(params); end`.",
    },
    Keyword {
        name: "defined?",
        documentation: "Returns a string describing the type of an expression, or `nil` if it is not defined. The argument is not evaluated.",
    },
    Keyword {
        name: "do",
        documentation: "Starts a block of code, typically following an iterator method call. Paired with `end`.",
    },
    Keyword {
        name: "else",
        documentation: "Provides an alternative branch in `if`, `unless`, `case`, or `begin/rescue` expressions.",
    },
    Keyword {
        name: "elsif",
        documentation: "Provides an additional conditional branch within an `if` expression.",
    },
    Keyword {
        name: "end",
        documentation: "Closes a `class`, `module`, `def`, `if`, `unless`, `case`, `while`, `until`, `for`, `begin`, or `do` block.",
    },
    Keyword {
        name: "ensure",
        documentation: "Defines a block of code within `begin`/`def` that always runs, whether an exception was raised or not.",
    },
    Keyword {
        name: "false",
        documentation: "The singleton instance of `FalseClass`. One of two falsy values in Ruby, along with `nil`.",
    },
    Keyword {
        name: "for",
        documentation: "Iterates over a collection. Syntax: `for variable in collection; end`. Prefer `.each` in idiomatic Ruby.",
    },
    Keyword {
        name: "if",
        documentation: "Conditional branch. Can be used as a statement (`if cond; end`) or a modifier (`expr if cond`).",
    },
    Keyword {
        name: "in",
        documentation: "Used with `for` loops (`for x in collection`) and pattern matching (`case value; in pattern`).",
    },
    Keyword {
        name: "module",
        documentation: "Defines a new module or opens an existing one. Modules provide namespacing and mixins via `include`/`extend`.",
    },
    Keyword {
        name: "next",
        documentation: "Skips to the next iteration of a loop or block, optionally returning a value. Syntax: `next` or `next value`.",
    },
    Keyword {
        name: "nil",
        documentation: "The singleton instance of `NilClass`. Represents the absence of a value. Falsy in boolean context.",
    },
    Keyword {
        name: "not",
        documentation: "Low-precedence logical NOT operator. Unlike `!`, it has lower precedence than most operators.",
    },
    Keyword {
        name: "or",
        documentation: "Low-precedence logical OR operator. Unlike `||`, it has lower precedence than assignment.",
    },
    Keyword {
        name: "redo",
        documentation: "Restarts the current iteration of a loop or block without re-evaluating the condition.",
    },
    Keyword {
        name: "rescue",
        documentation: "Catches exceptions in a `begin`/`def` block. Can also be used inline: `expr rescue default`.",
    },
    Keyword {
        name: "retry",
        documentation: "Re-executes the `begin` block from the start. Only valid inside a `rescue` clause.",
    },
    Keyword {
        name: "return",
        documentation: "Exits from the current method, optionally returning a value. Syntax: `return` or `return value`.",
    },
    Keyword {
        name: "self",
        documentation: "References the current object. Inside a method, it is the receiver. Inside a class/module body, it is the class/module itself.",
    },
    Keyword {
        name: "super",
        documentation: "Calls the next occurrence of the method in the ancestor chain. Forwards all arguments if called without parentheses.",
    },
    Keyword {
        name: "then",
        documentation: "Optional separator after the condition in `if`, `unless`, `when`, or `in` clauses.",
    },
    Keyword {
        name: "true",
        documentation: "The singleton instance of `TrueClass`. Truthy in boolean context.",
    },
    Keyword {
        name: "undef",
        documentation: "Removes a method definition from the current class. Syntax: `undef method_name`.",
    },
    Keyword {
        name: "unless",
        documentation: "Inverted conditional. Executes the body when the condition is falsy. Can be used as statement or modifier.",
    },
    Keyword {
        name: "until",
        documentation: "Loops while the condition is falsy. Can be used as a statement or modifier.",
    },
    Keyword {
        name: "when",
        documentation: "Defines a branch in a `case` expression. Syntax: `when value; ...`.",
    },
    Keyword {
        name: "while",
        documentation: "Loops while the condition is truthy. Can be used as a statement or modifier.",
    },
    Keyword {
        name: "yield",
        documentation: "Calls the block passed to the current method, passing optional arguments. Returns the block's return value.",
    },
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_keywords_have_documentation() {
        for keyword in KEYWORDS {
            assert!(
                !keyword.documentation().is_empty(),
                "keyword '{}' has empty documentation",
                keyword.name()
            );
        }
    }

    #[test]
    fn get_returns_none_for_non_keywords() {
        assert!(get("puts").is_none());
        assert!(get("require").is_none());
        assert!(get("").is_none());
        assert!(get("Foo").is_none());
    }

    #[test]
    fn get_returns_keyword_with_correct_data() {
        let kw = get("yield").unwrap();
        assert_eq!(kw.name(), "yield");
        assert!(kw.documentation().contains("block"));

        let kw = get("defined?").unwrap();
        assert_eq!(kw.name(), "defined?");

        let kw = get("__FILE__").unwrap();
        assert_eq!(kw.name(), "__FILE__");
    }

    #[test]
    fn keyword_names_are_unique() {
        let mut seen = std::collections::HashSet::new();
        for keyword in KEYWORDS {
            assert!(seen.insert(keyword.name()), "duplicate keyword: '{}'", keyword.name());
        }
    }

    #[test]
    fn keywords_are_sorted() {
        for window in KEYWORDS.windows(2) {
            assert!(
                window[0].name() < window[1].name(),
                "KEYWORDS not sorted: '{}' should come before '{}'",
                window[1].name(),
                window[0].name()
            );
        }
    }
}

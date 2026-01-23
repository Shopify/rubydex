---
name: review_changes
description: Reviews code changes for Rubydex
---

The user will explain what they are implementing in a set of code changes. If they don't, ask for clarification.

- If there are unstaged changes, consider those the implementation
- If there are no unstaged changes, run `bin/branch_diff` to get the implementation
- Always consider applicable documentation listed under `docs`

**`rust/rubydex` changes**:

Use the Rust and static analysis experts to review the code changes regarding the following criteria.

- Correctness: does it model Ruby code properly? Are we missing valid scenarios?
- Code quality and idiomatic patterns: does the code use idiomatic Rust? Are there opportunities to improve the code
structure?
- Performance: are there any performance pitfalls or opportunities for optimization?
- Test coverage: are we missing scenarios?
- Documentation: are the key concepts clearly explained in the documentation (files under `docs` and README)?
- Agent instructions: are there key concepts or major changes that should be reflected in AGENTS.md?

**`rust/rubydex-sys` changes**:

Review the FFI changes and consider safety and correctness when bridging between Rust and C.

**`ext` or Ruby file changes**:

Use the Ruby expert to review the code changes regarding the following criteria.

- Correctness: does it implement the intended functionality properly? Are there edge cases we are missing?
- Code quality and idiomatic patterns: does the code use idiomatic C and Ruby?
- Performance: are there any performance pitfalls or opportunities for optimization?
- Name conventions: does it follow the conventions outlined in the documentation?

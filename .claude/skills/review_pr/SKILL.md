---
name: review-pr
description: Use when the user asks to review a GitHub pull request by number, URL, or branch name for the Saturn/Rubydex project.
---

# Review PR

Review a GitHub pull request for Saturn/Rubydex, applying zone-specific criteria across Rust, FFI, C extension, and Ruby layers.

## PR Intake

1. Parse the user's input:
   - Number: `577` -- use directly
   - URL: extract number from `https://github.com/.../pull/577`
   - Branch: `gh pr list --head <branch> --json number -q '.[0].number'`
   - No argument: `gh pr view --json number -q .number` (current branch)
2. Fetch PR data (in parallel):
   - **Metadata**: `gh pr view <N> --json title,body,author,state,baseRefName,headRefName,additions,deletions,files,reviews,comments`
   - **Diff**: `gh pr diff <N>`
3. If merged or closed, warn the user and ask whether to proceed.
4. Classify each changed file into a review zone:

| Path pattern | Zone |
|---|---|
| `rust/rubydex/src/**` (excluding `rubydex-sys`) | rust-core |
| `rust/rubydex-sys/**` | ffi |
| `ext/**` | c-extension |
| `lib/**`, `test/**` | ruby |
| `docs/**`, `*.md` | docs |
| Other | meta |

## Review Workflow

### Phase 0: Comprehension

1. Read the PR description and understand the stated goal
2. Read linked issues or PRs referenced in the description
3. Scan the file list to understand scope
4. Read full file contents for modified files (not just the diff)

### Phase 1: Dispatch

Compute `total_changed = additions + deletions`. Count active zones (rust-core, ffi, c-extension, ruby -- exclude docs/meta).

**Single-pass** (main agent, no subagents): `total_changed < 100` AND one active zone.

**Parallel expert review** (subagents): otherwise.

| Zone(s) with changes | Subagent |
|---|---|
| rust-core, ffi | `rust-systems-reviewer` |
| rust-core touching `resolution.rs`, `model/graph.rs`, or type-related code | also `type-checker-architect` |
| c-extension, ruby | `ruby-vm-expert` |
| docs | `general-purpose` |

Each subagent receives: PR title/body, their zone's diff, zone review criteria, and the cross-cutting concerns from this skill.

**Large PR plan file** (10+ files OR 500+ lines changed):
1. Generate `docs/plans/YYYY-MM-DD-review-pr-<N>.md` before reviewing
2. Include: PR metadata, file-to-zone table, subagent assignments, key questions
3. Inform the user, then proceed

### Phase 2: Cross-Cutting Analysis

After zone reviews complete, apply the cross-cutting checks below.

### Phase 3: Compile Output

Merge zone findings into the output format below. Deduplicate overlapping findings. Order by severity.

## Rust Core Review Criteria

### Correctness (Tier 1)

- **Ruby behavior modeling**: Does the Rust code correctly model Ruby semantics? Cross-reference `docs/ruby-behaviors.md`.
- **Explicit match arms**: Flag catch-all `_` on enums that may gain variants (`Definition`, `Declaration`, `DeclarationKind`, `Receiver`). Always list all variants.
- **Definition/reference completeness**: New types must update all iteration sites (`str_ids()`, `name_ids()`, document removal cleanup).
- **Balanced lifecycle**: Every ref-count increment (`intern_string`, `intern_name`) needs a decrement path in document removal (`untrack_string`, `untrack_name`).

### Memory (Tier 2)

- **`assert_mem_size!` compliance**: Struct field changes require updated size assertions. 8 bytes matters at millions of instances.
- **Enum niche optimization**: `Option<Enum>` should use niche (same size as enum alone). Flag regressions.
- **String interning**: All user-facing strings go through `intern_string`/`intern_name`. Flag raw `String` fields in model structs.
- **`IdentityHashMap`**: Prefer over `HashMap` for ID-keyed maps.

### Performance (Tier 3)

- **Allocation hot paths**: In indexing visitor, flag `String::from()`, `to_string()`, `format!()`, `Vec::new()`. Prefer interning.
- **Clone avoidance**: Flag `.clone()` on `String`, `Vec`, or non-Copy types unless justified.
- **Iterators over collect**: Flag `.collect::<Vec<_>>()` feeding another iterator. Prefer lazy chains.
- **Lock contention**: `Mutex`/`RwLock` lock scope must be minimal. `Graph::merge()` changes affect parallel indexing.
- **Pre-allocation**: `Vec::with_capacity()` where size is known.

### API Design

- **Visibility**: `pub(crate)` for items not needed outside the crate. Flag unnecessary `pub`.
- **Naming precision**: Flag vague names (`process`, `handle`, `resolve` without qualifier).
- **Reuse infrastructure**: Flag new abstractions where existing patterns suffice.
- **Id discipline**: All IDs are `Id<T>` (4-byte, Copy). Never box, heap-allocate, or use raw `u32`.

### Code Quality

- `expect("descriptive message")` over bare `unwrap()`
- `if let` over single-arm `match` with wildcard
- `Entry` API over get-then-insert
- Debug-only code behind `#[cfg(debug_assertions)]`

## FFI Bridge Review Criteria

FFI bugs cause segfaults in production. Highest-risk changes.

### Safety

- **No panics across `extern "C"`**: Flag `unwrap()`, `expect()`, or panic-possible code in `extern "C"` functions.
- **Pointer lifecycle**: Every `Box::into_raw()` has a deallocation path. NULL-check before every dereference.
- **CString/CStr**: Handle null termination. Verify UTF-8 assumptions for Ruby string inputs.

### Conventions

- **Naming**: Rust FFI exports use `rdx_` prefix. Flag deviations.
- **Patterns**: New FFI functions follow existing patterns (`with_graph`, `DeclarationsIter`, `rb_ensure` cleanup).

### Boundary

- **`#[repr(C)]`** on any struct crossing the boundary with verified alignment.
- **Memory ownership**: Unambiguous -- document who allocates and who frees.
- **Error propagation**: Return null/error codes, never exceptions or panics.

## C Extension Review Criteria

### Naming Prefixes (flag all violations)

| Prefix | Meaning | Example |
|---|---|---|
| `rdx_` | Rust FFI export | `rdx_graph_new()` |
| `rdxr_` | Ruby VM callback | `rdxr_graph_resolve()` |
| `rdxi_` | Internal cross-file helper | `rdxi_check_array_of_strings()` |
| `static` (no prefix) | File-local helper | `static VALUE graph_documents_yield()` |

### Memory Safety

1. Every `rdx_*()` return that allocates has a matching `rdx_*_free()` call
2. C arrays from `rdxi_str_array_to_char()` freed element-by-element then array
3. Iterator patterns use `rb_ensure(yield_fn, args, cleanup_fn, args)`
4. Every pointer from Rust is NULL-checked before dereferencing
5. No new global mutable state (Ractor safety: `rb_ext_ractor_safe(true)`)

### Type Safety

- Validate arguments with `Check_Type()` or `rdxi_check_array_of_strings()` before Rust calls
- Enum switch `default:` cases `rb_raise` on unknown values
- `// Keep this in sync with` comments -- if Rust enums changed, verify C switch cases match

## Ruby Review Criteria

- `# frozen_string_literal: true` pragma on every `.rb` file
- Naming: `snake_case` methods, `predicate?` booleans, `bang!` mutation
- API consistency: `rb_define_method` registrations have corresponding wrappers in `lib/`
- Enumerator duality: collection APIs support both block and enumerator forms

## Ruby Behavior Correctness

When the PR modifies `ruby_indexer.rs`, `resolution.rs`, or `docs/ruby-behaviors.md`:

1. Cross-reference `docs/ruby-behaviors.md` to verify implementation matches documented semantics
2. Flag commonly-missed edge cases:
   - `private` does NOT affect `def self.method` (but DOES affect methods inside `class << self`)
   - `Class.new do ... end` blocks do NOT create lexical scope
   - Class variables (`@@var`) follow lexical scope, NOT the receiver
   - `class Foo::Bar::Baz` does not guarantee intermediate constants exist
   - Mixin deduplication: `include A` twice produces only one ancestor entry
   - `class << Foo` inside `class Bar` reopens Foo's singleton, not Bar's
3. Suggest a Ruby snippet test for any new behavior, following @Morriar's review pattern
4. Invoke `ruby-vm-expert` subagent when uncertain about Ruby VM behavior

### Test Quality

- Every test uses `with_context` for file system isolation
- Test Ruby code uses heredocs (`<<~RUBY`), not string concatenation
- Tests assert correct behavior, not just non-crash
- Resolution tests call `resolve` after `index_all` then assert graph state
- Error handling tests cover `TypeError`/`ArgumentError` for bad inputs

## Cross-Cutting Concerns

Apply after zone reviews and throughout all phases:

1. **Naming consistency**: Rust `snake_case` -> C `rdx_`/`rdxr_`/`rdxi_` -> Ruby `snake_case`
2. **API completeness**: New Rust API -> FFI exposure -> Ruby binding. Flag gaps.
3. **Memory ownership chain**: Who allocates? Who frees? Verified across FFI boundary?
4. **Error propagation chain**: Rust `Result` -> C null/error code -> Ruby exception. Complete?
5. **Test coverage**: Rust unit tests + Ruby integration tests. Flag gaps.
6. **Documentation**: Flag changes needing `docs/architecture.md`, `docs/ruby-behaviors.md`, or `AGENTS.md` updates.
7. **Cross-platform correctness**: URI/path handling on Windows.
8. **Concrete suggestions**: Every recommendation includes a code suggestion, not vague advice.

## Output Format

```
## PR Review: <title>

**Summary**: 1-3 sentences.

**Critical Issues** (must fix):
- `file.rs:42` -- Description.
  [code suggestion]

**Recommendations** (should fix):
- `file.rs:88` -- Description with code suggestion.

**Observations** (trade-offs, architecture):
- Description.

**Test Coverage**:
- What's tested, what's missing. Suggested test cases.

**Nits**:
- `file.rs:12` -- Minor suggestion.

**Files Reviewed**:
- `path/file1.rs` -- 3 findings
- `path/file2.c` -- No issues found
- `path/file3.rb` -- 1 finding
```

Rules:
- Every finding includes `file_path:line_number`
- Every recommendation includes a concrete code suggestion
- Omit empty severity sections
- Every changed file appears in "Files Reviewed"

## Anti-Rationalization

| Shortcut | Why it's wrong |
|---|---|
| Summarizing instead of reviewing | Small PRs can have critical bugs. Full criteria always. |
| Parroting the PR description | Verify every claim against the diff. Flag unsupported claims. |
| "CI is green so code is correct" | CI means existing tests pass, not that new code is tested. |
| Reviewing only Rust files | Apply correct criteria to every zone with changes. |
| Skipping "straightforward" files | Every changed file appears in output. No exceptions. |
| Treating refactors as safe | Refactors hide subtle behavioral changes. Verify equivalence. |
| Plan files for small PRs | Plan files only for 10+ files or 500+ lines. |
| Using `bin/branch_diff` or checkout | Always `gh pr diff`. Never check out the PR branch. |

## Red Flags

Stop and ask the user if:
- PR modifies `unsafe` blocks or raw pointer operations in C
- PR changes FFI signatures without corresponding C extension updates (or vice versa)
- PR removes or weakens test assertions
- PR changes `assert_mem_size!` values
- PR modifies resolution logic without updating `docs/ruby-behaviors.md`

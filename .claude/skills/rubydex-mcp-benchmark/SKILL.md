---
name: rubydex-mcp-benchmark
description: Use when benchmarking rubydex MCP against plain Grep/Glob for Ruby code tasks. Guides A/B comparison setup, candidate selection, and metrics collection.
---

# Rubydex MCP Benchmark

Compare rubydex MCP-assisted development vs standard Grep/Glob for Ruby code tasks.

## Prerequisites

Ensure the `rubydex_mcp` binary is installed:

```bash
# From the rubydex repo root
cargo install --path rust/rubydex-mcp
```

This places `rubydex_mcp` in `~/.cargo/bin/`. Verify: `rubydex_mcp --help`

## Step 1: Find a Good Benchmark Candidate

Use rubydex MCP tools to find a class or module suitable for testing:

1. Run `codebase_stats` to get an overview of the target project
2. Run `search_declarations` with kind "Class" or "Module" to list candidates
3. For each candidate, run `find_constant_references` and count references
4. Pick a target with **10-30 references across 4+ files** that has test coverage

Good candidates are:
- Self-contained (not deeply entangled with the whole codebase)
- Referenced in both lib and test directories
- Have a dedicated test file

## Step 2: Set Up the Experiment

Create two git worktrees for the target Ruby project:

```bash
cd /path/to/target-project
git worktree add ../project-with-mcp -b bench-with-mcp HEAD
git worktree add ../project-without-mcp -b bench-without-mcp HEAD
```

Add `.mcp.json` to the with-mcp worktree only:

```bash
cat > ../project-with-mcp/.mcp.json << 'EOF'
{
  "mcpServers": {
    "rubydex": {
      "command": "${HOME}/.cargo/bin/rubydex_mcp"
    }
  }
}
EOF
```

## Step 3: Generate Commands for the Developer

Pick a testing prompt based on the scenario. Keep prompts **vague and natural** - like a lazy developer would type. Don't specify full namespaces, file paths, or implementation details. Let the agent figure it out. This tests the MCP advantage fairly since the MCP agent can resolve ambiguity semantically while the non-MCP agent has to grep around.

**Guidelines for writing prompts:**
- Use short class names, not fully qualified (`Context` not `IRB::Context`)
- Don't specify file locations or paths
- Don't tell the agent how to do it, just what to do
- Always end with how to run tests

**Examples:**

Rename:
```
Rename Locale to LocaleManager in IRB. All existing tests must continue to pass. Run tests with: bundle exec rake test
```

Extract module:
```
Extract all history-related methods from Context into a new module Context::HistoryManager. All existing tests must continue to pass. Run tests with: bundle exec rake test
```

Then generate two ready-to-copy commands and present them to the developer:

**With MCP** (run in terminal 1):
```bash
cd /path/to/project-with-mcp && claude "<testing prompt>" --dangerously-skip-permissions
```

**Without MCP** (run in terminal 2):
```bash
cd /path/to/project-without-mcp && claude "<testing prompt>" --dangerously-skip-permissions
```

Tell the developer to copy and run these commands in separate terminals simultaneously.

## Step 4: Compare Results

After both sessions complete, compare:

| Metric | With MCP | Without MCP |
|--------|----------|-------------|
| Duration | (from session) | (from session) |
| Tokens | (from session stats) | (from session stats) |
| Files changed | `git diff --stat` in with-mcp worktree | `git diff --stat` in without-mcp worktree |
| Tests | pass/fail | pass/fail |

Run `git diff --stat` in each worktree to compare the changes made.

## Step 5: Cleanup

```bash
cd /path/to/target-project
git worktree remove ../project-with-mcp
git worktree remove ../project-without-mcp
git branch -D bench-with-mcp bench-without-mcp
```

## Reference: Our Benchmark Results

| Scenario | Metric | With MCP | Without MCP | MCP Advantage |
|----------|--------|----------|-------------|---------------|
| Rename (IRB::Locale, 16 refs) | Tokens | 22k | 31.8k | 30% fewer |
| Rename (IRB::Locale, 16 refs) | Time | 2m 9s | 2m 44s | 25% faster |
| Extract module (IRB::Context) | Tokens | 33.2k | 63.0k | 47% fewer |
| Extract module (IRB::Context) | Time | 1m 49s | 2m 39s | 31% faster |

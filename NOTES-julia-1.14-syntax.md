# Julia 1.14 syntax: typegroup, labeled break/continue, break with value

Investigated 2026-08-17 against julia nightly 1.14.0-DEV.1966 (juliaup) whose
vendored JuliaSyntax is the restructured "v2" line (`share/julia/JuliaSyntax/src/julia/`).
All trees below verified by parsing with `Base.JuliaSyntax.parseall(SyntaxNode, src;
version = v"1.14")` on that nightly.

## The constructs at parser level

### `break [label [value]]`, `continue [label]`

```
break            ==> (break)
break lbl        ==> (break lbl)            # lbl is a plain Identifier kid
break lbl val    ==> (break lbl val)        # value REQUIRES a label first
continue         ==> (continue)
continue lbl     ==> (continue lbl)
```

- There is **no plain `break value`** — the grammar is `break [label [value]]`; the
  first identifier after `break` is always the label.
- The parser **requires whitespace between label and value** (error node
  "expected space after break label" otherwise). A formatter must never delete
  that space.
- Labels are ordinary identifiers (also `$x`, `var"..."`, contextual keywords).
  There is **no label-definition syntax in the parser** as of DEV.1966: no
  `K"label"` kind exists, `outer: for ...` just parses as an infix `:` call.
  Label→loop resolution happens later (lowering); e.g. `break i` in
  `for i in ...` refers to the loop by its variable. Watch upstream for a
  dedicated definition syntax before assuming this is final.

### `typegroup`

```
typegroup
struct A end
struct B end
end              ==> (typegroup (block ...))
```

`typegroup` is a full keyword in ≥1.14 (identifier before, with error-recovery
special-casing). Shape matches `begin`-like keyword+block+end.

## Version gating

- Everything is gated on `ps.stream.version >= (1, 14)`
  (`min_supported_version(v"1.14", ...)`).
- Even the 1.14 nightly's default `Meta.parse` currently REJECTS these (the DEV
  version compares below the gate / stable-gated); explicit `version = v"1.14"`
  (or higher) is required at parse time.
- Runic already parses with `version = v"2-"` (both in `Context` and in the
  `format_tree!` re-parse check), which passes the gate — no plumbing needed.

## Runic status and impact

**Blocker:** Runic pins JuliaSyntax 1.0.2, which cannot parse any of this
(`typegroup` is not a keyword; `break lbl` is "unexpected token after break").
The constructs only exist on the restructured JuliaSyntax line vendored in the
nightly, which **also changes tree shapes Runic depends on** — observed:
macro names are now a nested `[macro_name]` node with an Identifier kid instead
of a `K"MacroName"` leaf (breaks `macrocall_name`, `kmatch(kids, KSet"@ MacroName")`,
docstring detection, ...). The dependency migration (cf. `external/JuliaSyntax2`,
`js4/`, `issue-juliasyntax-parse_block.md`) is the prerequisite for everything below.

Once on the new parser:

1. **typegroup indentation**: add a dispatch branch in
   `insert_delete_mark_newlines` — `indent_keyword_block_end!` fits as-is
   (keyword at index 1, block, `end`). Also add `typegroup` to the
   `remove_trailing_semicolon` KSet and check
   `no_leading_and_single_trailing_newline` interplay. Tests mirroring the
   begin/module block tests.
2. **break/continue spacing**: `spaces_around_keywords`' keyword_set contains
   `return` but not `break`/`continue` (they were childless until now). Add
   them so `break   lbl    val` normalizes to single spaces. The mandatory
   label/value space is preserved by construction (the rule inserts/normalizes,
   never removes the only space).
3. **explicit_return semantics hazard**: `explicit_return_block`'s
   `KSet"for while"` arm appends a bare `return` after a trailing loop. Under
   1.14, a loop with `break lbl value` has a value — appending `return` would
   change the function's return to `nothing`. The arm must skip loops that
   contain a value-carrying break targeting them (needs a
   `has_break_with_value`-style predicate; conservative version: any
   `(break lbl val)` anywhere in the loop body).
4. **Idempotence checks**: new node kinds flow through `format_node_with_kids!`
   generically; `(break lbl val)` kids are keyword/ws/Identifier/ws/expr leaves
   handled by existing whitespace rules once (2) is done.
5. Add cases to the `"parsing new syntax"` testset (currently only tests
   `public a, b`).

Existing formatted code is unaffected: `break lbl` was previously a parse
error, so no currently-valid source changes meaning.

## Update (same day): migration done on branch `juliasyntax-v2`

- Branch `juliasyntax-v2` (on top of `runestone-simplify`) migrates Runic to
  JuliaSyntax 2.0.0-DEV: 10 targeted edits, full test suite green, corpus
  byte-identical vs the 1.0.2 build. See the commit message for the shape-change
  catalog (macro_name nodes, StrMacroName, dotted-op kinds .op=/.=/.||/.&&,
  zero-span K"VERSION" marker nodes stripped in normalize_tree!).
- IMPORTANT: JuliaSyntax.jl repo main (a713779, 2026-02) has the 2.0 restructure
  but NOT the 1.14 syntax or VERSION markers — those are only in the copy
  vendored in the julia repo. Develop against `external/JuliaSyntax-nightly/`
  (snapshot of julia nightly 1.14.0-DEV.1966's share/julia/JuliaSyntax):
      julia --project        -e 'using Pkg; Pkg.develop(path = "external/JuliaSyntax-nightly")'
      julia --project=juliac -e 'using Pkg; Pkg.develop(path = "external/JuliaSyntax-nightly")'
  (`external/JuliaSyntax-main` is a worktree of the repo's main for reference.)
- 1.14 constructs now parse and format through Runic. Confirmed remaining rule
  work (unchanged from the list above): typegroup indentation, break/continue
  spacing, explicit_return break-with-value guard, tests.

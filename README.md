# Runic.jl

*A code formatter with rules set in stone.*

Runic is a formatter for the [Julia programming language](https://julialang.org/) built on
top of [JuliaSyntax.jl](https://github.com/JuliaLang/JuliaSyntax.jl).


Similarly to [`gofmt`](https://pkg.go.dev/cmd/gofmt), Runic have *no configuration*. The
formatting rules are set in stone (although not yet complete). This approach is something
that is appreciated by most Go programmers, see for example the following
[quote](https://www.youtube.com/watch?v=PAAkCSZUG1c&t=523s):

> Gofmt's style is no one's favorite, yet gofmt is everyone's favorite.


### Table of contents

 - [Installation](#installation)
 - [Usage](#usage)
    - [CLI](#cli)
    - [Editor integration](#editor-integration)
 - [Checking formatting](#checking-formatting)
    - [Github Actions](#github-actions)
    - [Git hooks](#git-hooks)
    - [Ignore formatting commits in git blame](#ignore-formatting-commits-in-git-blame)
 - [Formatting specification](#formatting-specification)

## Installation

Runic can be installed with Julia's package manager:

```sh
julia -e 'using Pkg; Pkg.add(url = "https://github.com/fredrikekre/Runic.jl")'
```

For CLI usage and editor integration (see [Usage](#usage)) it is recommended to install
Runic in a separate project such as e.g. the shared project `@runic`:

```sh
julia --project=@runic -e 'using Pkg; Pkg.add(url = "https://github.com/fredrikekre/Runic.jl")'
```

## Usage

### CLI

The main interface to Runic is the command line interface (CLI) through the `main` function
invoked with the `-m` flag. See the output of `julia -m Runic --help` below for usage
details.

The following snippet can be added to your shell startup file so that the CLI can be invoked
a bit more ergonomically. This assumes Runic is installed in the `@runic` shared project as
suggested in the [Installation](#installation) section above. Adjust the `--project` flag if
you installed Runic elsewhere.

```sh
alias runic="julia --project=@runic -m Runic"
```

> [!NOTE]
> The `-m` command line flag is only available in Julia 1.12 and later. In earlier versions
> you have to invoke the `main` function explicitly, for example:
> ```sh
> julia -e 'using Runic; exit(Runic.main(ARGS))' -- <args>
> ```
> For this incantation the following shell alias can be used:
> ```sh
> alias runic="julia --project=@runic -e 'using Runic; exit(Runic.main(ARGS))' --"
> ```

```
$ julia -m Runic --help
NAME
       Runic.main - format Julia source code

SYNOPSIS
       julia -m Runic [<options>] <path>...

DESCRIPTION
       `Runic.main` (typically invoked as `julia -m Runic`) formats Julia source
       code using the Runic.jl formatter.

OPTIONS
       <path>...
           Input path(s) (files and/or directories) to process. For directories,
           all files (recursively) with the '*.jl' suffix are used as input files.
           If no path is given, or if path is `-`, input is read from stdin.

       -c, --check
           Do not write output and exit with a non-zero code if the input is not
           formatted correctly.

       -d, --diff
           Print the diff between the input and formatted output to stderr.
           Requires `git` to be installed.

       --help
           Print this message.

       -i, --inplace
           Format files in place.

       -o <file>, --output=<file>
           File to write formatted output to. If no output is given, or if the file
           is `-`, output is written to stdout.
```

In addition to the CLI there is also the two function `Runic.format_file` and
`Runic.format_string`. See their respective docstrings for details.

### Editor integration

#### Neovim

Runic can be used as a formatter in [Neovim](https://neovim.io/) using
[conform.nvim](https://github.com/stevearc/conform.nvim). Refer to the conform.nvim
repository for installation and setup instructions.

Runic is not (yet) available directly in conform so the following configuration needs
to be passed to the setup function. This assumes Runic is installed in the `@runic` shared
project as suggested in the [Installation](#installation) section above. Adjust the
`--project` flag if you installed Runic elsewhere.

```lua
require("conform").setup({
    formatters = {
        runic = {
            command = "julia",
            args = {"--project=@runic", "-e", "using Runic; exit(Runic.main(ARGS))"},
        },
    },
    formatters_by_ft = {
        julia = {"runic"},
    },
    default_format_opts = {
        -- Increase the timeout in case Runic needs to precompile
        -- (e.g. after upgrading Julia and/or Runic).
        timeout_ms = 10000,
    },
})
```

Note that conform (and thus Runic) can be used as `formatexpr` for the `gq` command. This is
enabled by adding the following to your configuration:
```lua
vim.o.formatexpr = "v:lua.require('conform').formatexpr()"
```

> [!IMPORTANT]
> Note that conform is a third party plugin. It works as advertised but use it at your own
> risk.

#### VS Code

Runic can be used as a formatter in [VS Code](https://code.visualstudio.com/) using the
extension [Custom Local Formatters](https://marketplace.visualstudio.com/items?itemName=jkillian.custom-local-formatters&ssr=false#overview).

After installing the extension you can configure Runic as a local formatter by adding the
following entry to your `settings.json`:

```json
"customLocalFormatters.formatters": [
    {
      "command": "julia --project=@runic -e 'using Runic; exit(Runic.main(ARGS))'",
      "languages": ["julia"]
    }
]
```

Using the "Format Document" VS Code command will now format the file using Runic. Note that
the first time you execute the command you will be prompted to select a formatter since the
Julia language extension also comes with a formatter.

> [!IMPORTANT]
> Note that Custom Local Formatters is a third party extension. It works as advertised but
> use it at your own risk.

## Checking formatting

Runic has a check-mode that verifies whether files are correctly formatted or not. This mode
is enabled with the `--check` flag. In check mode Runic will exit with a non-zero code if
any of the input files are incorrectly formatted. As an example, the following invocation
can be used:

```sh
git ls-files -z -- '*.jl' | xargs -0 --no-run-if-empty julia --project=@runic -m Runic --check --diff
```

This will run Runic's check mode (`--check`) on all `.jl` files in the repository and print
the diff (`--diff`) if the files are not formatted correctly. If any file is incorrectly
formatted the exit code will be non-zero.


### Github Actions

You can use [`fredrikekre/runic-action`](https://github.com/fredrikekre/runic-action) to run
Runic on Github Actions:

```yaml
name: Runic formatting
on:
  push:
    branches:
      - 'master'
      - 'release-'
    tags:
      - '*'
  pull_request:
jobs:
  runic:
    name: Runic
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      # - uses: julia-actions/setup-julia@v2
      #   with:
      #     version: '1'
      # - uses: julia-actions/cache@v2
      - uses: fredrikekre/runic-action@v1
        with:
          version: '1'
```

See [`fredrikekre/runic-action`](https://github.com/fredrikekre/runic-action) for details.

> [!IMPORTANT]
> It is *highly recommended* to pin the Runic version to a full version number (e.g.
> `major.minor.patch`) to avoid CI failures due to changes in Runic.jl because even
> formatting bug fixes may result in formatting changes that would then fail the workflow.

### Git hooks

Runic can be used together with [`pre-commit`](https://pre-commit.com/) using
[`fredrikekre/runic-pre-commit`](https://github.com/fredrikekre/runic-pre-commit). After
installing `pre-commit` you can add the following to your `.pre-commit-config.yaml` to run
Runic before each commit:

```yaml
repos:
  - repo: https://github.com/fredrikekre/runic-pre-commit
    rev: v1.0.0
    hooks:
      - id: runic
```

See [`fredrikekre/runic-pre-commit`](https://github.com/fredrikekre/runic-pre-commit) for
details.

If you don't want to use `pre-commit` you can also use a plain git hook. Here is an example
hook (`.git/hooks/pre-commit`):

```sh
#!/usr/bin/env bash

# Redirect output to stderr.
exec 1>&2

# Run Runic on added and modified files
git diff-index -z --name-only --diff-filter=AM master | \
    grep -z '\.jl$' | \
    xargs -0 --no-run-if-empty julia --project=@runic -m Runic --check --diff
```

### Ignore formatting commits in git blame

When setting up Runic formatting for a repository for the first time (or when upgrading to a
new version of Runic) the formatting commit will likely result in a large diff with mostly
non functional changes such as e.g. whitespace. Since the diff is large it is likely that it
will show up and interfere when using [`git-blame`](https://git-scm.com/docs/git-blame). To
ignore commits during `git-blame` you can i) add them to a file `.git-blame-ignore-revs` and
ii) tell git to use this file as ignore file by running

```
git config blame.ignoreRevsFile .git-blame-ignore-revs
```

See the [git-blame
documentation](https://git-scm.com/docs/git-blame#Documentation/git-blame.txt---ignore-revs-fileltfilegt)
for details.

For example, such a file may look like this:
```
# Adding Runic formatting
<commit hash of formatting commit>

# Upgrading Runic from 1.0 to 2.0
<commit hash of formatting commit>
```

## Formatting specification

This is a list of things that Runic currently is doing:

 - [Toggle formatting](#toggle-formatting)
 - [Line width limit](#line-width-limit)
 - [Newlines in blocks](#newlines-in-blocks)
 - [Indentation](#indentation)
 - [Explicit `return`](#explicit-return)
 - [Spaces around operators, assignment, etc](#spaces-around-operators-assignment-etc)
 - [Spaces around keywords](#spaces-around-keywords)
 - [Multiline listlike expressions](#multiline-listlike-expressions)
 - [Spacing in listlike expressions](#spacing-in-listlike-expressions)
 - [Trailing semicolons](#trailing-semicolons)
 - [Literal floating point numbers](#literal-floating-point-numbers)
 - [Literal hex and oct numbers](#literal-hex-and-oct-numbers)
 - [Parentheses around operator calls in colon](#parentheses-around-operator-calls-in-colon)
 - [`in` instead of `∈` and `=`](#in-instead-of--and-)
 - [Braces around right hand side of `where`](#braces-around-right-hand-side-of-where)
 - [Whitespace miscellaneous](#whitespace-miscellaneous)

### Toggle formatting

It is possible to toggle formatting around expressions where you want to disable Runic's
formatting. This can be useful in cases where manual formatting increase the readability of
the code. For example, manually aligned array literals may look worse when formatted by
Runic.

The source comments `# runic: off` and `# runic: on` will toggle the formatting off and on,
respectively. The comments must be on their own line, they must be on the same level in the
syntax tree, and they must come in pairs. An exception to the pairing rule is made at top
level where a `# runic: off` comment will disable formatting for the remainder of the file.
This is so that a full file can be excluded from formatting without having to add a
`# runic: on` comment at the end of the file.

> [!NOTE]
> Note that it is enough that a comment contain the substring `# runic: off` or
> `# runic: on` so that they can be combined with other "pragmas" such as e.g.
> [Literate.jl line filters](https://fredrikekre.github.io/Literate.jl/v2/fileformat/#Filtering-lines)
> like `#src`.

> [!NOTE]
> For compatibility with [JuliaFormatter](https://github.com/domluna/JuliaFormatter.jl) the
> comments `#! format: off` and `#! format: on` are also recognized by Runic.

For example, the following code will toggle off the formatting for the array literal `A`:

```julia
function foo()
    a = rand(2)
    # runic: off
    A = [
        -1.00   1.41
         3.14  -4.05
    ]
    # runic: on
    return A * a
end
```

### Line width limit

No. Use your <kbd>Enter</kbd> key or refactor your code.

###  Newlines in blocks

The body of blocklike expressions (e.g. `if`, `for`, `while`, `function`, `struct`, etc.)
always start and end with a newline. Examples:
```diff
-if c x end
+if c
+    x
+end

-function f(x) x^2 end
+function f(x)
+    x^2
+end
```

An exception is made for empty blocks so that e.g.
```julia
struct A end
```
is allowed.

### Indentation

Consistently four spaces for each indentation level.

Standard code blocks (`function`, `for`, `while`, ...) all increase the indentation level by
one until the closing `end`. Examples:
```diff
 function f()
-  for i in 1:2
-    # loop
-  end
-  while rand() < 0.5
-    # loop
-  end
+    for i in 1:2
+        # loop
+    end
+    while rand() < 0.5
+        # loop
+    end
 end
```

Listlike expressions like e.g. tuples, function calls, array literals, etc. also increase
the indentation level by one until the closing token. This only has an effect if the list
span multiple lines. Examples:
```diff
 x = (
-  a, b, c, d,
-  e, f, g, h,
+    a, b, c, d,
+    e, f, g, h,
 )

 foo(
-  a, b, c, d,
-  e, f, g, h,
+    a, b, c, d,
+    e, f, g, h,
 )

 [
-  a, b, c, d,
-  e, f, g, h,
+    a, b, c, d,
+    e, f, g, h,
 ]
```

The examples above both result in "hard" indentation levels. Other expressions that span
multiple lines result in "soft" indentation levels. The difference between the two is that
soft indentation levels don't nest (this is really only applicable to multiline operator
call chains).

```diff
 using Foo:
-  foo, bar
+    foo, bar

 x = a + b +
-  c
+    c

 x = a ? b :
-  c
+    c
```

Without soft indentation levels operators chains can result in ugly (but logically correct)
indentation levels. For example, the following code:
```julia
x = a + b *
        c +
    d
```
would be "correct". Such a chain looks better the way it is currently formatted:
```julia
x = a + b *
    c +
    d
```

### Explicit `return`

Explicit `return` statements are ensured in function and macro definitions by adding
`return` in front of the last expression, with some exceptions listed below.

 - If the last expression is a `for` or `while` loop (which both always evaluate to
   `nothing`) `return` is added *after* the loop.
 - If the last expression is a `if` or `try` block the `return` is only added in case
   there is no `return` inside any of the branches.
 - If the last expression is a `let` or `begin` block the `return` is only added in case
   there is no `return` inside the block.
 - If the last expression is a macro call, the `return` is only added in case there is no
   `return` inside the macro.
 - No `return` is added in short form functions (`f(...) = ...`), short form anonymous
   functions (`(...) -> ...`), and `do`-blocks (`f(...) do ...; ...; end`).
 - If the last expression is a function call, and the function name is (or contains) `throw`
   or `error`, no `return` is added. This is because it is already obvious that these calls
   terminate the function and don't return any value.

Note that adding `return` changes the expression in a way that is visible to macros.
Therefore it is, in general, not valid to add `return` to a function defined inside a macro
since it isn't possible to know what the macro will expand to. For this reason this
formatting rule is disabled for functions defined inside macros with the exception of some
known and safe ones from Base (e.g. `@inline`, `@generated`, ...).

For the same reason mentioned above, if the last expression in a function is a macro call it
isn't valid to step in and add `return` inside. Instead the `return` will be added in front
of the macro call like any other expression (unless there is already a `return` inside of
the macro as described above).

Examples:
```diff
 function f(n)
-    sum(rand(n))
+    return sum(rand(n))
 end

 macro m(args...)
-    :(generate_expr(args...))
+    return :(generate_expr(args...))
 end
```

#### Potential changes
 - If the last expression is a `if` or `try` block it might be better to
   recurse into the branches and add `return` there. Looking at real code, if a
   function ends with an `if` block, it seems about 50/50 whether adding return
   *after* the block or adding return inside the branches is the best choice.
   Quite often `return if` is not the best but at least Runic's current
   formatting will force to think about the return value.
   See issue [#52](https://github.com/fredrikekre/Runic.jl/issues/52).

### Spaces around operators, assignment, etc

Runic formats spaces around infix operators, assignments, comparison chains, and type
comparisons (binary `<:` and `>:`), and some other operator-like things. If the space is
missing it will be inserted, if there are multiple spaces it will be reduced to one.
Examples:
```diff
-1+2*3
-1  +  2  *  3
+1 + 2 * 3
+1 + 2 * 3

-x=1
-x=+1
-x+=1
-x.+=1
+x = 1
+x = +1
+x += 1
+x .+= 1
-1<2>3
-1  <  2  >  3
+1 < 2 > 3
+1 < 2 > 3

-T<:Integer
-T  >:  Integer
+T <: Integer
+T >: Integer

-x->x
-a  ?  b  :  c
+x -> x
+a ? b : c
```

Note that since Runic's rules are applied consistently, no matter the context or surrounding
code, the "spaces around assignment" rule also means that there will be spaces in keyword
arguments in function definitions and calls. Examples:
```diff
-foo(; a=1) = a
-foo(a=1)
+foo(; a = 1) = a
+foo(a = 1)
```

Exceptions to the rule above are `:`, `..`, `^`, `::`, and unary `<:` and `>:`. These are
formatted *without* spaces around them. Examples:
```diff
-a : b
+a:b

-a ^ 5
+a^5

-a :: Int
+a::Int

-<: Integer
->:  Integer
+<:Integer
+>:Integer
```

#### Potential changes
 - Perhaps the rule for some of these should be "at least one space" instead. This could
   help with alignment issues. Discussed in issue
   [#12](https://github.com/fredrikekre/Runic.jl/issues/12).

### Spaces around keywords

Consistently use single space around keywords. Examples:
```diff
-struct  Foo
+struct Foo

-mutable  struct  Bar
+mutable struct Bar

-function  foo(x::T)  where  {T}
+function foo(x::T) where {T}
```

### Multiline listlike expressions

Listlike expressions (tuples, function calls/definitions, array literals, etc.) that
*already* span multiple lines are formatted to consistently have a leading and a trailing
newline. Trailing commas are enforced for array/tuple literals (where adding another item is
common) but optional for function/macro calls/definitions.
```diff
-(a,
-    b)
+(
+    a,
+    b,
+)

-foo(a,
-    b)
+foo(
+    a,
+    b
+)

-[1 2
- 3 4]
+[
+    1 2
+    3 4
+]
```

Note that currently there is no line-length limit employed so expressions that only take up
a single line, even if they are long, are not formatted like the above. Thus, only
expressions where the original author have "committed" to mulitples lines are affected by
this rule.

### Spacing in listlike expressions

Listlike expressions (tuples, function calls/definitions, array literals, etc.) use a
consistent rule of no space before `,` and a single space after `,`. Trailing commas are
enforced for array/tuple literals (where adding another item is common) but optional for
function/macro calls/definitions. Leading/trailing spaces are removed. Examples:

```diff
-f(a,b)
-(a,b)
-[a,  b]
+f(a, b)
+(a, b)
+[a, b]


-(a,b,)
+(a, b)
 (
     a,
-    b
+    b,
 )

-( a, b )
+(a, b)
```

#### Potential changes
 - Perhaps the rule for some of these should be "at least one space" instead. This could
   help with alignment issues. Discussed in issue
   [#12](https://github.com/fredrikekre/Runic.jl/issues/12).

### Trailing semicolons

Trailing semicolons are removed in the body of blocklike expressions. Examples
```diff
 function f(x)
-    y = x^2;
-    z = y^2; # z = x^4
-    return z;
+    y = x^2
+    z = y^2  # z = x^4
+    return z
 end
```

Trailing semicolons at top level and module level are kept since they are sometimes used
there for output supression (e.g. Documenter examples or scripts that are
copy-pasted/included in the REPL).

### Literal floating point numbers

Floating point literals are normalized so that they:
 - always have a decimal point
 - always have a digit before and after the decimal point
 - never have leading zeros in the integral and exponent part
 - never have trailing zeros in the fractional part
 - always use `e` instead of `E` for the exponent

Examples:
```diff
-1.
-.1
+1.0
+0.1

-01.2
-1.0e01
-0.10
+1.2
+1.0e1
+0.1

-1.2E5
+1.2e5
```

#### Potential changes
 - Always add the implicit `+` for the exponent part, i.e. `1.0e+1` instead of `1.0e1`.
   Discussed in issue [#13](https://github.com/fredrikekre/Runic.jl/issues/13).
 - Allow multiple trailing zeros in the fractional part, i.e. don't change `1.00` to `1.0`.
   Such trailing zeros are sometimes used to align numbers in literal array expressions.
   Discussed in issue [#14](https://github.com/fredrikekre/Runic.jl/issues/14).

### Literal hex and oct numbers

Hex literals are padded with zeros to better highlight the resulting type of the literal:
`UInt8` to 2 characters, `UInt16` to 4 characters, `UInt32` to 8 characters etc. Examples:
```diff
-0x1
-0x123
-0x12345
+0x01
+0x0123
+0x00012345
```

### Parentheses around operator calls in colon

Add parentheses around operator calls in colon expressions to better highlight the low
precedence of `:`. Examples:
```diff
-1 + 2:3 * 4
-1 + 2:3
-1:3 * 4
+(1 + 2):(3 * 4)
+(1 + 2):3
+1:(3 * 4)
```

### `in` instead of `∈` and `=`

The keyword `in` is used consistently instead of `∈` and `=` in `for` loops. Examples:
```diff
-for i = 1:2
+for i in 1:2

-for i ∈ 1:2
+for i in 1:2
```

Note that `∈` not replaced when used as an operator outside of loop contexts in
order to be symmetric with `∉` which doesn't have a direct ASCII equivalent.
See [#17](https://github.com/fredrikekre/Runic.jl/issues/17) for more details.

### Braces around right hand side of `where`

Braces are consistently used around the right hand side of `where` expressions. Examples:
```diff
-T where T
-T where T <: S where S <: Any
+T where {T}
+T where {T <: S} where {S <: Any}
```

### Whitespace miscellaneous


#### Trailing spaces

Trailing spaces are removed in code and comments (but not inside of multiline strings where
doing so would change the meaning of the code). Examples:
```diff
-1 + 1 
+1 + 1

-x = 2 # x is two 
+x = 2 # x is two
```

#### Tabs

Tabs are replaced with spaces. Example:
```diff
-function f()
-	return 1
-end
+function f()
+    return 1
+end
```

#### Vertical spacing

Extra vertical spacing is trimmed so that there are at maximum two empty lines
between expressions. Examples:
```diff
-function f()
-     x = 1
-
-
-
-    return x
-end
+function f()
+     x = 1
+
+
+    return x
+end
```

Any newlines at the start of a file are removed and if the file ends with more
than one newline the extra ones are removed.

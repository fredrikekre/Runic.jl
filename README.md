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
           If path is `-` input is read from stdin and output written to stdout.

       -c, --check
           Do not write output and exit with a non-zero code if the input is not
           formatted correctly.

       -d, --diff
           Print the diff between the input and formatted output to stderr.
           Requires `git` or `diff` to be installed.

       --fail-fast
           Exit immediately after the first error. Only applicable when formatting
           multiple files in the same invocation.

       --help
           Print this message.

       -i, --inplace
           Edit files in place. This option is required when passing multiple input
           paths.

       -o, --output <file>
           Output file to write formatted code to. If the specified file is `-`
           output is written to stdout. This option can not be used together with
           multiple input paths.
```

In addition to the CLI there is also the two function `Runic.format_file` and
`Runic.format_string`. See their respective docstrings for details.

### Editor integration

#### Neovim

Runic can be used as a formatter in [Neovim](https://neovim.io/) using
[`conform.nvim`](https://github.com/stevearc/conform.nvim). Refer to the `conform.nvim`
repository for installation and setup instructions.

Runic is not (yet) available directly in `conform.nvim` so the following configuration needs
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

> [!WARNING]
> Note that Custom Local Formatters is a third party extension. It works as advertised but
> use it at your own risk.

## Checking formatting

Runic has a check-mode that verifies whether files are correctly formatted or not. This mode
is enabled with the `--check` flag. In check mode Runic will exit with a non-zero code if
any of the input files are incorrectly formatted. As an example, the following invocation
can be used:

```sh
git ls-files -z -- '*.jl' | xargs -0 julia -m Runic --check --diff
```

This will run Runic's check mode (`--check`) on all `.jl` files in the repository and print
the diff (`--diff`) if the files are not formatted correctly. If any file is incorrectly
formatted the exit code will be non-zero.


### Github Actions

Here is a complete workflow file for running Runic on Github Actions:

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
      - uses: julia-actions/setup-julia@v2
        with:
          version: "nightly" # Only nightly have the -m flag currently
      - uses: julia-actions/cache@v2
      - name: Install Runic
        run: |
          julia --color=yes --project=@runic -e 'using Pkg; Pkg.add(url = "https://github.com/fredrikekre/Runic.jl")'
      - name: Run Runic
        run: |
          git ls-files -z -- '*.jl' | xargs -0 julia --project=@runic -m Runic --check --diff
```

### Git Hooks

Runic can be run in a
[Git pre-commit hook](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks)
to automatically check formatting before committing. Here is an example hook
(`.git/hooks/pre-commit`):

```sh
#!/usr/bin/env bash

# Redirect output to stderr.
exec 1>&2

# Run Runic on added and modified files
git diff-index -z --name-only --diff-filter=AM master | \
    grep -z '\.jl$' | \
    xargs -0 --no-run-if-empty -p julia --project=@runic -m Runic --check --diff
```

## Formatting specification

This is a list of things that Runic currently is doing:

 - [Line width limit](#line-width-limit)
 - [Indentation](#indentation)
 - [Spaces around operators, assignment, etc](#spaces-around-operators-assignment-etc)
 - [Spaces around keywords](#spaces-around-keywords)
 - [Multiline listlike expressions](#multiline-listlike-expressions)
 - [Spacing in listlike expressions](#spacing-in-listlike-expressions)
 - [Literal floating point numbers](#literal-floating-point-numbers)
 - [Literal hex and oct numbers](#literal-hex-and-oct-numbers)
 - [Parentheses around operator calls in colon](#parentheses-around-operator-calls-in-colon)
 - [`in` instead of `∈` and `=`](#in-instead-of--and-)
 - [Braces around right hand side of `where`](#braces-around-right-hand-side-of-where)
 - [Trailing whitespace](#trailing-whitespace)

### Line width limit

No. Use your <kbd>Enter</kbd> key or refactor your code.

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

### Vertical spacing

Runic removes empty vertical spacing so that there are at maximum two empty lines between
expressions. Examples:
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

Exceptions to the rule above are `:`, `^`, `::`, and unary `<:` and `>:`. These are
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
newline, as well as a trailing comma where applicable. Examples:
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
+    b,
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
consistent rule of no space before `,` and a single space after `,`. In single line
expressions there is no trailing `,`. In multiline expressions there is a trailing `,`.
Leading/trailing spaces are removed. Examples:
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

### Trailing whitespace

Trailing spaces are removed. Example:
```diff
-1 + 1 
+1 + 1
```

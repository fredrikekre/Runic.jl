# Runic.jl

*A code formatter with rules set in stone.*

## Installation

```julia
using Pkg
Pkg.add("Runic")
```

## Usage

The main interface to Runic is the command line interface (CLI) through the `main` function
invoked with the `-m` flag. See the output of `julia -m Runic --help` for details:

> [!NOTE]
> The `-m` command line flag is only available in Julia 1.12 and later. In earlier versions
> you have to invoke the `main` function explicitly, for example:
> ```sh
> julia -e 'using Runic; exit(Runic.main(ARGS))' -- <args>
> ```

```sh
$ julia-master -m Runic --help
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
           If path is `-` input is read from stdin.

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

## Formatting specification

This is a list of the rules and formatting transformations performed by Runic:

 - No trailing whitespace
 - Normalized line endings (`\r\n` -> `\n`) (TODO: Is this bad on Windows with Git's autocrlf? gofmt does it...)
 - Hex/octal/binary literals are padded with zeroes to better highlight the resulting UInt
   type
 - Floating point literals are normalized to always have an integral and fractional part.
   `E`-exponents are normalized to `e`-exponents. Unnecessary trailing/leading zeros from
   integral, fractional, and exponent parts are removed.

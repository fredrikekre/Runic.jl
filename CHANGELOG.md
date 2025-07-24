# Runic.jl changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v1.4.4] - 2025-07-24
### Fixed
 - Fix another edgecase where adding a trailing comma after the last item in a list results
   in invalid code. ([#151], [#152]).

## [v1.4.3] - 2025-02-28
### Fixed
 - Fix formatting of floating point literals that use `−` (Unicode U+2212) instead of the
   typically used `-` (ASCII/Unicode U+002D). The parser treats them as synonyms but the
   former triggered an assertion error in Runic while formatting. After this change Runic
   will normalize to `-` when formatting ([#137], [#138]).

## [v1.4.2] - 2025-02-18
### Fixed
 - Add missing documentation of `--lines` to the `--help` output ([#136]).

## [v1.4.1] - 2025-02-07
### Fixed
 - Include the source filename in error messages from the parser ([#132], [#133]).

## [v1.4.0] - 2025-01-24
### Added
 - The `runic` binary can now be installed directly with the package manager as a Pkg "App"
   (requires Julia 1.12). To install:
   ```
   pkg> app add Runic
   ```
   ([#129])

## [v1.3.0] - 2024-12-17
### Added
 - New executable wrapper script
   [`runic`](https://github.com/fredrikekre/Runic.jl/blob/master/bin/runic) that can be put
   in `PATH` to simplify Runic invocation. This method should be preferred over a shell
   alias since it allows processes other than the shell to invoke Runic. Refer to the
   [Installation](https://github.com/fredrikekre/Runic.jl?tab=readme-ov-file#installation)
   section of the README for details.
 - New Git integration provided by the executable
   [`git-runic`](https://github.com/fredrikekre/Runic.jl/blob/master/bin/git-runic) script
   ([#122]). This can be used to incrementally format a codebase by limiting formatting to
   changed and modified lines in each commit. Refer to the [Git
   integration](https://github.com/fredrikekre/Runic.jl?tab=readme-ov-file#git-integration)
   section of the README for installation and usage instructions.
 - New command line argument `--verbose` which enables verbose output ([#121]).
 - The library functions `Runic.format_file` and `Runic.format_string` are now considered
   part of the Runic public API and are marked with `public` in supported Julia versions.
   ([#127]).
### Changed
 - Runic is now silent by default. Use `--verbose` to enable the verbose file progress
   printing from previous releases ([#121]).
### Fixed
 - Improved the command line argument parsing to handle more edge cases related to input
   files and directories such as e.g. empty directories ([#123]).
 - Fix some error paths where the error would be printed before the file info
   output in `--verbose` mode ([#124]).

## [v1.2.0] - 2024-12-09
### Added
 - New command line option `--lines=a:b` for limiting formatting to lines `a` to `b`.
   `--lines` can be repeated to specify multiple ranges ([#114], [#120]).

## [v1.1.0] - 2024-12-04
### Added
 - Added a suggested README badge to add to your project's README to show that you are using
   Runic ([#108], [#112]). See the
   [Badge](https://github.com/fredrikekre/Runic.jl?tab=readme-ov-file#badge) section of the
   README for details.
 - Added a version policy to clarify what formatting changes are allowed in
   patch/minor/major releases. See the [Version
   policy](https://github.com/fredrikekre/Runic.jl?tab=readme-ov-file#version-policy)
   section of the README for details.
### Changed
 - Fix a bug that caused "single space after keyword" to not apply after the `function`
   keyword in non-standard function definitions ([#113]). This bug is classified as a
   [spec-bug] and the fix will result in diffs like the following:
   ```diff
   -function()
   +function ()
        # ...
    end
   ```
 - Fix a bug that caused "single space after keyword" to not apply after `let` ([#117]).
   This bug is classified as a [spec-bug] and the fix will result in diffs like the
   following when `let` is followed by multiple spaces (which should be rare) in the source:
   ```diff
   -let  a = 1
   +let a = 1
        a
    end
   ```
 - Fix formatting of whitespace in between `let`-variables ([#118]). This bug is classified
   as a [spec-bug] and the fix will result in diffs like the following in rare cases where
   e.g. multiple spaces, or spaces *before* comma, is used in the variable list:
   ```diff
   -let  a = 1,  b = 2
   +let  a = 1, b = 2
        a + b
    end
   ```
 - Fix a bug that caused multiline variable blocks in `let` to not indent correctly ([#97],
   [#116]). This bug is classified as a [spec-bug] and the fix will result in diffs like the
   following whenever multiline variable blocks exist in the source:
   ```diff
    let a = 1,
   -    b = 2
   +        b = 2
        a + b
    end
   ```

## [v1.0.1] - 2024-11-28
### Fixed
 - Fix an incorrect whitespace assertion in function indentation ([#109], [#110]).

## [v1.0.0] - 2024-11-06
First stable release of Runic.jl. See [README.md](README.md) for details and documentation.

[spec-bug]: https://github.com/fredrikekre/Runic.jl?tab=readme-ov-file#version-policy


<!-- Links generated by Changelog.jl -->

[v1.0.0]: https://github.com/fredrikekre/Runic.jl/releases/tag/v1.0.0
[v1.0.1]: https://github.com/fredrikekre/Runic.jl/releases/tag/v1.0.1
[v1.1.0]: https://github.com/fredrikekre/Runic.jl/releases/tag/v1.1.0
[v1.2.0]: https://github.com/fredrikekre/Runic.jl/releases/tag/v1.2.0
[v1.3.0]: https://github.com/fredrikekre/Runic.jl/releases/tag/v1.3.0
[v1.4.0]: https://github.com/fredrikekre/Runic.jl/releases/tag/v1.4.0
[v1.4.1]: https://github.com/fredrikekre/Runic.jl/releases/tag/v1.4.1
[v1.4.2]: https://github.com/fredrikekre/Runic.jl/releases/tag/v1.4.2
[v1.4.3]: https://github.com/fredrikekre/Runic.jl/releases/tag/v1.4.3
[#97]: https://github.com/fredrikekre/Runic.jl/issues/97
[#108]: https://github.com/fredrikekre/Runic.jl/issues/108
[#109]: https://github.com/fredrikekre/Runic.jl/issues/109
[#110]: https://github.com/fredrikekre/Runic.jl/issues/110
[#112]: https://github.com/fredrikekre/Runic.jl/issues/112
[#113]: https://github.com/fredrikekre/Runic.jl/issues/113
[#114]: https://github.com/fredrikekre/Runic.jl/issues/114
[#116]: https://github.com/fredrikekre/Runic.jl/issues/116
[#117]: https://github.com/fredrikekre/Runic.jl/issues/117
[#118]: https://github.com/fredrikekre/Runic.jl/issues/118
[#120]: https://github.com/fredrikekre/Runic.jl/issues/120
[#121]: https://github.com/fredrikekre/Runic.jl/issues/121
[#122]: https://github.com/fredrikekre/Runic.jl/issues/122
[#123]: https://github.com/fredrikekre/Runic.jl/issues/123
[#124]: https://github.com/fredrikekre/Runic.jl/issues/124
[#127]: https://github.com/fredrikekre/Runic.jl/issues/127
[#129]: https://github.com/fredrikekre/Runic.jl/issues/129
[#132]: https://github.com/fredrikekre/Runic.jl/issues/132
[#133]: https://github.com/fredrikekre/Runic.jl/issues/133
[#136]: https://github.com/fredrikekre/Runic.jl/issues/136
[#137]: https://github.com/fredrikekre/Runic.jl/issues/137
[#138]: https://github.com/fredrikekre/Runic.jl/issues/138

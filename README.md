# Runic.jl

*A code formatter with rules set in stone.*

## Formatting specification

This is a list of the rules and formatting transformations performed by Runic:

 - No trailing whitespace
 - Normalized line endings (`\r\n` -> `\n`) (TODO: Is this bad on Windows with Git's autocrlf? gofmt does it...)
 - Hex/octal/binary literals are padded with zeroes to better highlight the resulting UInt
   type
 - Floating point literals are normalized to always have an integral and fractional part.
   `E`-exponents are normalized to `e`-exponents. Unnecessary trailing/leading zeros from
   integral, fractional, and exponent parts are removed.

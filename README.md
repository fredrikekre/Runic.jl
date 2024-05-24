# Runic.jl

*A code formatter with rules set in stone.*

## Formatting specification

This is a list of the rules and formatting transformations performed by Runic:

 - No trailing whitespace
 - Normalized line endings (`\r\n` -> `\n`) (TODO: Is this bad on Windows with Git's autocrlf?)
 - Hex/octal/binary literals are padded with zeroes

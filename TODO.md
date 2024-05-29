# TODOs, notes, and various thoughts

## Inconsistencies

- The `spaces_around_operators` rule have the following inconsistencies.

   - `:`, `^`, and `::` instead fall under `no_spaces_around_colon_etc`:
     ```julia
     # current formatting      # "consistent" formatting
     a:b                       a : b                            ✖
     a^b                       a ^ b                            ✖
     a::b                      a :: b                           ✖
     ```

   - `<:` and `<:` fall under `no_spaces_around_colon_etc` if they have no LHS:
     ```julia
     # current formatting      # "consistent" formatting
     a <: b                    a <: b                           ✔
     a >: b                    a >: b                           ✔
     a{c <: b}                 a{c <: b}                        ✔
     a{c >: b}                 a{c >: b}                        ✔
     a{<:b}                    a{<: b}                          ✖
     a{>:b}                    a{>: b}                          ✖
     ```

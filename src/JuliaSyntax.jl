# SPDX-License-Identifier: MIT

# The code in this file is derived from code in the JuliaSyntax.jl package
# (https://github.com/JuliaLang/JuliaSyntax.jl) licensed under the MIT license.
# (https://github.com/JuliaLang/JuliaSyntax.jl/blob/main/LICENSE.md):

# MIT License
#
# Copyright (c) 2021 Julia Computing and contributors
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# https://github.com/JuliaLang/JuliaSyntax.jl/blob/bd290251c0aeb2d2e4c0349bb8ea4a33834d1c02/src/green_tree.jl#L45-L80
function _show_green_node(io, node, indent, pos, str, show_trivia)
    if !show_trivia && JuliaSyntax.is_trivia(node)
        return
    end
    posstr = "$(lpad(pos, 6)):$(rpad(pos + span(node) - 1, 6)) │"
    brackets = is_leaf(node) ? ("" => "") : ("[" => "]")
    line = string(
        posstr, indent, brackets.first, JuliaSyntax.summary(head(node)), brackets.second
    )
    if node.tags != 0
        line = string(rpad(line, 50), ' ', "tags: $(stringify_tags(node))")
    end
    println(io, line)
    if !is_leaf(node)
        new_indent = indent * "  "
        p = pos
        for x in verified_kids(node)
            _show_green_node(io, x, new_indent, p, str, show_trivia)
            p += x.span
        end
    end
    return
end

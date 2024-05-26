# SPDX-License-Identifier: MIT

function is_leaf(node::JuliaSyntax.GreenNode)
    return !JuliaSyntax.haschildren(node)
end

function first_leaf(node::JuliaSyntax.GreenNode)
    if is_leaf(node)
        return node
    else
        return first_leaf(first(JuliaSyntax.children(node)::AbstractVector))
    end
end

function last_leaf(node::JuliaSyntax.GreenNode)
    if is_leaf(node)
        return node
    else
        return last_leaf(last(JuliaSyntax.children(node)::AbstractVector))
    end
end

function is_assignment(node::JuliaSyntax.GreenNode)
    return JuliaSyntax.is_prec_assignment(node)
end

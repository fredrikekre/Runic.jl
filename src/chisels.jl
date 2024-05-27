# SPDX-License-Identifier: MIT

##############
# Debug info #
##############

# @lock is defined but not exported in older Julia versions
if VERSION < v"1.7.0"
    using Base: @lock
end

# Code derived from ToggleableAsserts.jl kept in a separate file
include("ToggleableAsserts.jl")

abstract type RunicException <: Exception end

struct AssertionError <: RunicException
    msg::String
end

function Base.showerror(io::IO, err::AssertionError)
    print(
        io,
        "Runic.AssertionError: `", err.msg, "`. This is unexpected, " *
        "please file an issue with a reproducible example at " *
        "https://github.com/fredrikekre/Runic.jl/issues/new.",
    )
end

function macroexpand_assert(expr)
    msg = string(expr)
    return :($(esc(expr)) || throw(AssertionError($msg)))
end


##########################
# JuliaSyntax extensions #
##########################

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

function replace_first_leaf(node::JuliaSyntax.GreenNode, child′::JuliaSyntax.GreenNode)
    if is_leaf(node)
        return child′
    else
        children′ = copy(JuliaSyntax.children(node)::AbstractVector)
        children′[1] = replace_first_leaf(children′[1], child′)
        @assert length(children′) > 0
        span′ = mapreduce(JuliaSyntax.span, +, children′; init = 0)
        return JuliaSyntax.GreenNode(JuliaSyntax.head(node), span′, children′)
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

# Just like `JuliaSyntax.is_infix_op_call`, but also check that the node is K"call"
function is_infix_op_call(node::JuliaSyntax.GreenNode)
    return JuliaSyntax.kind(node) === K"call" &&
        JuliaSyntax.is_infix_op_call(node)
end

function infix_op_call_op(node::JuliaSyntax.GreenNode)
    @assert is_infix_op_call(node)
    children = JuliaSyntax.children(node)::AbstractVector
    first_operand_index = findfirst(!JuliaSyntax.is_whitespace, children)
    op_index = findnext(JuliaSyntax.is_operator, children, first_operand_index + 1)
    return children[op_index]
end

function is_comparison_leaf(node::JuliaSyntax.GreenNode)
    return is_leaf(node) && JuliaSyntax.is_prec_comparison(node)
end

function is_operator_leaf(node::JuliaSyntax.GreenNode)
    return is_leaf(node) && JuliaSyntax.is_operator(node)
end

function first_non_whitespace_child(node::JuliaSyntax.GreenNode)
    @assert !is_leaf(node)
    children = JuliaSyntax.children(node)::AbstractVector
    idx = findfirst(!JuliaSyntax.is_whitespace, children)::Int
    return children[idx]
end

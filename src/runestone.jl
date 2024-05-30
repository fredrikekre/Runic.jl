# SPDX-License-Identifier: MIT

# This is the runestone where all the formatting transformations are implemented.

function trim_trailing_whitespace(ctx::Context, node::JuliaSyntax.GreenNode)
    JuliaSyntax.kind(node) === K"NewlineWs" || return nothing
    @assert !JuliaSyntax.haschildren(node)
    str = String(node_bytes(ctx, node))
    str′ = replace(str, r"\h*(\r\n|\r|\n)" => '\n')
    # If the next sibling is also a NewlineWs we can trim trailing
    # whitespace from this node too
    next_kind = next_sibling_kind(ctx)
    if next_kind === K"NewlineWs"
        # str′ = replace(str′, r"(\r\n|\r|\n)\h*" => '\n')
        str′ = replace(str′, r"\n\h*" => '\n')
    end
    if str == str′
        return nothing
    end
    # Write new bytes and reset the stream
    nb = replace_bytes!(ctx, str′, JuliaSyntax.span(node))
    @assert nb != JuliaSyntax.span(node)
    # Create new node and return it
    node′ = JuliaSyntax.GreenNode(JuliaSyntax.head(node), nb, ())
    return node′
end

function format_hex_literals(ctx::Context, node::JuliaSyntax.GreenNode)
    JuliaSyntax.kind(node) === K"HexInt" || return nothing
    @assert JuliaSyntax.flags(node) == 0
    @assert !JuliaSyntax.haschildren(node)
    span = JuliaSyntax.span(node)
    @assert span > 2 # 0x prefix + something more
    # Target spans(0x + maximum chars for formatted UInt8, UInt16, UInt32, UInt64, UInt128)
    target_spans = 2 .+ (2, 4, 8, 16, 32)
    if span >= 34 || span in target_spans
        # Do nothing: correctly formatted or a BigInt hex literal
        return nothing
    end
    # Insert leading zeros
    i = findfirst(x -> x > span, target_spans)::Int
    bytes = node_bytes(ctx, node)
    while length(bytes) < target_spans[i]
        insert!(bytes, 3, '0')
    end
    nb = replace_bytes!(ctx, bytes, span)
    @assert nb == length(bytes) == target_spans[i]
    # Create new node and return it
    node′ = JuliaSyntax.GreenNode(JuliaSyntax.head(node), nb, ())
    return node′
end

function format_oct_literals(ctx::Context, node::JuliaSyntax.GreenNode)
    JuliaSyntax.kind(node) === K"OctInt" || return nothing
    @assert JuliaSyntax.flags(node) == 0
    @assert !JuliaSyntax.haschildren(node)
    span = JuliaSyntax.span(node)
    @assert span > 2 # 0o prefix + something more
    # Padding depends on the value of the literal...
    str = String(node_bytes(ctx, node))
    n = tryparse(UInt128, str)
    if n === nothing
        # Do nothing: BigInt oct literal
        return nothing
    end
    # Compute the target span
    target_span_from_value =
        n <= typemax(UInt8) ? 5 : n <= typemax(UInt16) ? 8 :
        n <= typemax(UInt32) ? 13 : n <= typemax(UInt64) ? 24 :
        n <= typemax(UInt128) ? 45 : error("unreachable")
    target_spans = (5, 8, 13, 24, 45)
    i = findfirst(x -> x >= span, target_spans)::Int
    target_span_from_source = target_spans[i]
    target_span = max(target_span_from_value, target_span_from_source)
    if span == target_span
        # Do nothing: correctly formatted oct literal
        return nothing
    end
    # Insert leading zeros
    bytes = node_bytes(ctx, node)
    while length(bytes) < target_span
        insert!(bytes, 3, '0')
    end
    nb = replace_bytes!(ctx, bytes, span)
    @assert nb == length(bytes) == target_span
    # Create new node and return it
    node′ = JuliaSyntax.GreenNode(JuliaSyntax.head(node), nb, ())
    return node′
end

function format_float_literals(ctx::Context, node::JuliaSyntax.GreenNode)
    JuliaSyntax.kind(node) in KSet"Float Float32" || return nothing
    @assert JuliaSyntax.flags(node) == 0
    @assert !JuliaSyntax.haschildren(node)
    str = String(node_bytes(ctx, node))
    # Check and shortcut the happy path first
    r = r"""
    ^
    (?:[+-])?            # Optional sign
    (?:(?:[1-9]\d*)|0)   # Non-zero followed by any digit, or just a single zero
    \.                   # Decimal point
    (?:(?:\d*[1-9])|0)   # Any digit with a final nonzero, or just a single zero
    (?:[ef][+-]?[1-9]\d*)?
    $
    """x
    if occursin(r, str)
        return nothing
    end
    if occursin('_', str) || startswith(str, "0x")
        # TODO: Hex floats and floats with underscores are ignored
        return nothing
    end
    # Split up the pieces
    r = r"^(?<sgn>[+-])?(?<int>\d*)(?:\.?(?<frac>\d*))?(?:(?<epm>[eEf][+-]?)(?<exp>\d+))?$"
    m = match(r, str)
    io = IOBuffer() # TODO: Could be reused?
    # Write the sign part
    if (sgn = m[:sgn]; sgn !== nothing)
        write(io, sgn)
    end
    # Strip leading zeros from integral part
    int_part = isempty(m[:int]) ? "0" : m[:int]
    int_part = replace(int_part, r"^0*((?:[1-9]\d*)|0)$" => s"\1")
    write(io, int_part)
    # Always write the decimal point
    write(io, ".")
    # Strip trailing zeros from fractional part
    frac_part = isempty(m[:frac]) ? "0" : m[:frac]
    frac_part = replace(frac_part, r"^((?:\d*[1-9])|0)0*$" => s"\1")
    write(io, frac_part)
    # Write the exponent part
    if m[:epm] !== nothing
        write(io, replace(m[:epm], "E" => "e"))
        @assert m[:exp] !== nothing
        # Strip leading zeros from integral part
        exp_part = isempty(m[:exp]) ? "0" : m[:exp]
        exp_part = replace(exp_part, r"^0*((?:[1-9]\d*)|0)$" => s"\1")
        write(io, exp_part)
    end
    bytes = take!(io)
    nb = replace_bytes!(ctx, bytes, JuliaSyntax.span(node))
    @assert nb == length(bytes)
    # Create new node and return it
    node′ = JuliaSyntax.GreenNode(JuliaSyntax.head(node), nb, ())
    return node′
end

# Insert space around `x`, where `x` can be operators, assignments, etc. with the pattern:
# `<something><space><x><space><something>`, for example the spaces around `+` and `=` in
# `a = x + y`.
function spaces_around_x(ctx::Context, node::JuliaSyntax.GreenNode, is_x::F) where F
    # TODO: So much boilerplate here...
    @assert JuliaSyntax.haschildren(node)

    children = verified_children(node)
    children′ = children
    any_changes = false
    pos = position(ctx.fmt_io)
    ws = JuliaSyntax.GreenNode(
        JuliaSyntax.SyntaxHead(K"Whitespace", JuliaSyntax.TRIVIA_FLAG), 1, (),
    )

    # Toggle for whether we are currently looking for whitespace or not
    looking_for_whitespace = false
    looking_for_x = false

    for (i, child) in pairs(children)
        if JuliaSyntax.kind(child) === K"NewlineWs" ||
            (i == 1 && JuliaSyntax.kind(child) === K"Whitespace")
            # NewlineWs are accepted as is by this pass.
            # Whitespace is accepted as is if this is the first child even if the span is
            # larger than we expect since we don't look backwards. It should be cleaned up
            # by some other pass.
            accept_node!(ctx, child)
            any_changes && push!(children′, child)
            looking_for_whitespace = false
        elseif looking_for_whitespace
            if JuliaSyntax.kind(child) === K"Whitespace" && JuliaSyntax.span(child) == 1
                # All good, just advance the IO
                accept_node!(ctx, child)
                any_changes && push!(children′, child)
                looking_for_whitespace = false
            elseif JuliaSyntax.kind(child) === K"Whitespace"
                # Whitespace node but replace since not single space
                any_changes = true
                if children′ === children
                    children′ = children[1:i - 1]
                end
                push!(children′, ws)
                replace_bytes!(ctx, " ", JuliaSyntax.span(child))
                accept_node!(ctx, ws)
                looking_for_whitespace = false
            elseif JuliaSyntax.haschildren(child) &&
                    JuliaSyntax.kind(first_leaf(child)) === K"Whitespace"
                # Whitespace found at the beginning of next child.
                child_ws = first_leaf(child)
                looking_for_whitespace = JuliaSyntax.kind(last_leaf(child)) !== K"Whitespace"
                @assert !is_x(child)::Bool
                looking_for_x = true
                if JuliaSyntax.span(child_ws) == 1
                    # Accept the node
                    accept_node!(ctx, child)
                    any_changes && push!(children′, child)
                else
                    # Replace the whitespace node of the child
                    child′ = replace_first_leaf(child, ws)
                    @assert JuliaSyntax.span(child′) == JuliaSyntax.span(child) - JuliaSyntax.span(child_ws) + 1
                    bytes_to_skip = JuliaSyntax.span(child) - JuliaSyntax.span(child′)
                    @assert bytes_to_skip > 0
                    replace_bytes!(ctx, "", bytes_to_skip)
                    accept_node!(ctx, child′)
                    any_changes = true
                    if children′ === children
                        children′ = children[1:i - 1]
                    end
                    push!(children′, child′)
                end
            elseif JuliaSyntax.haschildren(child) &&
                    JuliaSyntax.kind(first_leaf(child)) === K"NewlineWs"
                # NewlineWs have to be accepted as is
                # @info "     ... childs first leaf is NewlineWs I'll take it"
                accept_node!(ctx, child)
                any_changes && push!(children′, child)
                looking_for_whitespace = JuliaSyntax.kind(last_leaf(child)) !== K"Whitespace"
                @assert !is_x(child)::Bool
                looking_for_x = true
            else
                # @info "     ... no whitespace, inserting" JuliaSyntax.kind(child)
                # Not a whitespace node, insert one
                any_changes = true
                if children′ === children
                    children′ = children[1:i - 1]
                end
                push!(children′, ws)
                replace_bytes!(ctx, " ", 0)
                accept_node!(ctx, ws)
                # Write and accept the node
                push!(children′, child)
                accept_node!(ctx, child)
                looking_for_whitespace = JuliaSyntax.kind(last_leaf(child)) !== K"Whitespace"
                if looking_for_x
                    @assert is_x(child)::Bool
                end
                # Flip the switch, unless child is a comment
                looking_for_x = JuliaSyntax.kind(child) === K"Comment" ? looking_for_x : !looking_for_x
            end
        else # !expect_ws
            if looking_for_x
                @assert is_x(child)::Bool
            end
            @assert JuliaSyntax.kind(child) !== K"Whitespace" # This would be weird, I think?
            any_changes && push!(children′, child)
            accept_node!(ctx, child)
            looking_for_whitespace = JuliaSyntax.kind(last_leaf(child)) !== K"Whitespace"
            # Flip the switch, unless child is a comment
            looking_for_x = JuliaSyntax.kind(child) === K"Comment" ? looking_for_x : !looking_for_x
        end
    end
    # Reset stream
    seek(ctx.fmt_io, pos)
    if any_changes
        # Create new node and return it
        return make_node(node, children′)
    else
        return nothing
    end
end

# This pass handles spaces around infix operator calls, comparison chains, and
# <: and >: operators.
function spaces_around_operators(ctx::Context, node::JuliaSyntax.GreenNode)
    if !(
        (is_infix_op_call(node) && !(JuliaSyntax.kind(infix_op_call_op(node)) in KSet": ^")) ||
        (JuliaSyntax.kind(node) in KSet"<: >:" && n_children(node) == 3) ||
        (JuliaSyntax.kind(node) === K"comparison" && !JuliaSyntax.is_trivia(node))
    )
        return nothing
    end
    @assert JuliaSyntax.kind(node) in KSet"call comparison <: >:"
    is_x = x -> is_operator_leaf(x) || is_comparison_leaf(x)
    return spaces_around_x(ctx, node, is_x)
end

function spaces_around_assignments(ctx::Context, node::JuliaSyntax.GreenNode)
    if !(is_assignment(node) && !is_leaf(node) )
        return nothing
    end
    # for-loop nodes are of kind K"=" even when `in` or `∈` is used so we need to
    # include these kinds in the predicate too.
    is_x = x -> is_assignment(x) || JuliaSyntax.kind(x) in KSet"in ∈"
    return spaces_around_x(ctx, node, is_x)
end

# Opposite of `spaces_around_x`: remove spaces around `x`
function no_spaces_around_x(ctx::Context, node::JuliaSyntax.GreenNode, is_x::F) where F
    @assert JuliaSyntax.haschildren(node)
    # TODO: Can't handle NewlineWs here right now
    if any(JuliaSyntax.kind(c) === K"NewlineWs" for c in JuliaSyntax.children(node))
        return nothing
    end

    children = verified_children(node)
    children′ = children
    any_changes = false
    pos = position(ctx.fmt_io)

    looking_for_x = false

    # K"::", K"<:", and K">:" are special cases here since they can be used without an LHS
    # in e.g. `f(::Int) = ...` and `Vector{<:Real}`.
    if JuliaSyntax.kind(node) in KSet":: <: >:"
        looking_for_x = is_x(first_non_whitespace_child(node))::Bool
    end

    for (i, child) in pairs(children)
        if (i == 1 || i == length(children)) && JuliaSyntax.kind(child) === K"Whitespace"
            accept_node!(ctx, child)
            any_changes && push!(children′, child)
        elseif JuliaSyntax.kind(child) === K"Whitespace"
            # Ignore it but need to copy children and re-write bytes
            any_changes = true
            if children′ === children
                children′ = children[1:i - 1]
            end
            replace_bytes!(ctx, "", JuliaSyntax.span(child))
        else
            @assert JuliaSyntax.kind(child) !== K"Whitespace"
            if looking_for_x
                @assert is_x(child)::Bool
            end
            any_changes && push!(children′, child)
            accept_node!(ctx, child)
            looking_for_x = !looking_for_x
        end
    end
    # Reset stream
    seek(ctx.fmt_io, pos)
    if any_changes
        # Create new node and return it
        node′ = make_node(node, children′)
        @assert JuliaSyntax.span(node′) < JuliaSyntax.span(node)
        return node′
    else
        return nothing
    end
end

# no spaces around `:`, `^`, and `::`
function no_spaces_around_colon_etc(ctx::Context, node::JuliaSyntax.GreenNode)
    if !(
        (is_infix_op_call(node) && JuliaSyntax.kind(infix_op_call_op(node)) in KSet": ^") ||
        (JuliaSyntax.kind(node) === K"::" && !is_leaf(node)) ||
        (JuliaSyntax.kind(node) in KSet"<: >:" && n_children(node) == 2)
    )
        return nothing
    end
    @assert JuliaSyntax.kind(node) in KSet"call :: <: >:"
    is_x = x -> is_leaf(x) && JuliaSyntax.kind(x) in KSet": ^ :: <: >:"
    return no_spaces_around_x(ctx, node, is_x)
end

# Replace the K"=" operator with `in`
function replace_with_in(ctx::Context, node::JuliaSyntax.GreenNode)
    @assert JuliaSyntax.kind(node) === K"=" && !is_leaf(node) && n_children(node) == 3
    children = verified_children(node)
    vars_index = findfirst(!JuliaSyntax.is_whitespace, children)
    # TODO: Need to insert whitespaces around `in` when replacing e.g. `i=I` with `iinI`.
    # However, at the moment it looks like the whitespace around operator pass does it's
    # thing first? I don't really know how though, because the for loop pass should be
    # happening before...
    in_index = findnext(!JuliaSyntax.is_whitespace, children, vars_index + 1)
    in_node = children[in_index]
    if JuliaSyntax.kind(in_node) === K"in"
        @assert JuliaSyntax.is_trivia(in_node)
        @assert is_leaf(in_node)
        return nothing
    end
    @assert JuliaSyntax.kind(in_node) in KSet"∈ ="
    @assert JuliaSyntax.is_trivia(in_node)
    @assert is_leaf(in_node)
    # Accept nodes to advance the stream
    for i in 1:(in_index - 1)
        accept_node!(ctx, children[i])
    end
    # Construct the replacement
    nb = replace_bytes!(ctx, "in", JuliaSyntax.span(in_node))
    in_node′ = JuliaSyntax.GreenNode(
        JuliaSyntax.SyntaxHead(K"in", JuliaSyntax.TRIVIA_FLAG), nb, (),
    )
    accept_node!(ctx, in_node′)
    children′ = copy(children)
    children′[in_index] = in_node′
    # Accept remaining eq_children
    for i in (in_index + 1):length(children′)
        accept_node!(ctx, children′[i])
    end
    return make_node(node, children′)
end

function replace_with_in_cartesian(ctx::Context, node::JuliaSyntax.GreenNode)
    @assert JuliaSyntax.kind(node) === K"cartesian_iterator" && !is_leaf(node)
    children = verified_children(node)
    children′ = children
    for (i, child) in pairs(children)
        if JuliaSyntax.kind(child) === K"="
            child′ = replace_with_in(ctx, child)
            if child′ !== nothing
                if children′ === children
                    children′ = copy(children)
                end
                children′[i] = child′
            else
                children′[i] = child
                accept_node!(ctx, child)
            end
        else
            children′[i] = child
            accept_node!(ctx, child)
        end
    end
    if children === children′
        return nothing
    end
    return make_node(node, children′)
end

# replace `=` and `∈` with `in` in for-loops
function for_loop_use_in(ctx::Context, node::JuliaSyntax.GreenNode)
    if !(
        (JuliaSyntax.kind(node) === K"for" && !is_leaf(node) && n_children(node) == 4) ||
        (JuliaSyntax.kind(node) === K"generator" && n_children(node) == 3) # TODO: Unsure about 3.
    )
        return nothing
    end
    pos = position(ctx.fmt_io)
    children = verified_children(node)
    for_index = findfirst(c -> JuliaSyntax.kind(c) === K"for" && is_leaf(c), children)::Int
    for_node = children[for_index]
    @assert JuliaSyntax.kind(for_node) === K"for" && JuliaSyntax.span(for_node) == 3 &&
        is_leaf(for_node) && JuliaSyntax.is_trivia(for_node)
    for i in 1:for_index
        accept_node!(ctx, children[i])
    end
    # The for loop specification node can be either K"=" or K"cartesian_iterator"
    for_spec_index = for_index + 1
    for_spec_node = children[for_spec_index]
    @assert JuliaSyntax.kind(for_spec_node) in KSet"= cartesian_iterator"
    if JuliaSyntax.kind(for_spec_node) === K"="
        for_spec_node′ = replace_with_in(ctx, for_spec_node)
    else
        @assert JuliaSyntax.kind(for_spec_node) === K"cartesian_iterator"
        for_spec_node′ = replace_with_in_cartesian(ctx, for_spec_node)
    end
    if for_spec_node′ === nothing
        seek(ctx.fmt_io, pos)
        return nothing
    end
    @assert position(ctx.fmt_io) == pos + mapreduce(JuliaSyntax.span, +, @view(children[1:for_index])) + JuliaSyntax.span(for_spec_node′)
    # Insert the new for spec node
    children′ = copy(children)
    children′[for_spec_index] = for_spec_node′
    # At this point the eq node is done, just accept any remaining nodes
    # TODO: Don't need to do this...
    for i in (for_spec_index + 1):length(children′)
        accept_node!(ctx, children′[i])
    end
    # Construct the full node and return
    node′ = make_node(node, children′)
    @assert position(ctx.fmt_io) == pos + JuliaSyntax.span(node′)
    seek(ctx.fmt_io, pos) # reset
    return node′
end

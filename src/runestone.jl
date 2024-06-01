# SPDX-License-Identifier: MIT

function dumpnode(node)
    println("node: {kind: $(kind(node)), span: $(span(node)), flags: $(flags(node)), nkids: $(length(verified_kids(node)))}")
end

# This is the runestone where all the formatting transformations are implemented.

function trim_trailing_whitespace(ctx::Context, node::Node)
    kind(node) === K"NewlineWs" || return nothing
    @assert is_leaf(node)
    str = String(read_bytes(ctx, node))
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
    nb = replace_bytes!(ctx, str′, span(node))
    @assert nb != span(node)
    # Create new node and return it
    node′ = Node(head(node), nb)
    return node′
end

function format_hex_literals(ctx::Context, node::Node)
    kind(node) === K"HexInt" || return nothing
    @assert flags(node) == 0
    @assert is_leaf(node)
    spn = span(node)
    @assert spn > 2 # 0x prefix + something more
    # Target spans(0x + maximum chars for formatted UInt8, UInt16, UInt32, UInt64, UInt128)
    target_spans = 2 .+ (2, 4, 8, 16, 32)
    if spn >= 34 || spn in target_spans
        # Do nothing: correctly formatted or a BigInt hex literal
        return nothing
    end
    # Insert leading zeros
    i = findfirst(x -> x > spn, target_spans)::Int
    bytes = read_bytes(ctx, node)
    while length(bytes) < target_spans[i]
        insert!(bytes, 3, '0')
    end
    nb = replace_bytes!(ctx, bytes, spn)
    @assert nb == length(bytes) == target_spans[i]
    # Create new node and return it
    node′ = Node(head(node), nb)
    return node′
end

function format_oct_literals(ctx::Context, node::Node)
    kind(node) === K"OctInt" || return nothing
    @assert flags(node) == 0
    @assert is_leaf(node)
    spn = span(node)
    @assert spn > 2 # 0o prefix + something more
    # Padding depends on the value of the literal...
    str = String(read_bytes(ctx, node))
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
    i = findfirst(x -> x >= spn, target_spans)::Int
    target_span_from_source = target_spans[i]
    target_span = max(target_span_from_value, target_span_from_source)
    if spn == target_span
        # Do nothing: correctly formatted oct literal
        return nothing
    end
    # Insert leading zeros
    bytes = read_bytes(ctx, node)
    while length(bytes) < target_span
        insert!(bytes, 3, '0')
    end
    nb = replace_bytes!(ctx, bytes, spn)
    @assert nb == length(bytes) == target_span
    # Create new node and return it
    node′ = Node(head(node), nb)
    return node′
end

function format_float_literals(ctx::Context, node::Node)
    kind(node) in KSet"Float Float32" || return nothing
    @assert flags(node) == 0
    @assert is_leaf(node)
    str = String(read_bytes(ctx, node))
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
    nb = replace_bytes!(ctx, bytes, span(node))
    @assert nb == length(bytes)
    # Create new node and return it
    node′ = Node(head(node), nb)
    return node′
end

# Insert space around `x`, where `x` can be operators, assignments, etc. with the pattern:
# `<something><space><x><space><something>`, for example the spaces around `+` and `=` in
# `a = x + y`.
function spaces_around_x(ctx::Context, node::Node, is_x::F) where F
    # TODO: So much boilerplate here...
    @assert !is_leaf(node)

    kids = verified_kids(node)
    kids′ = kids
    any_changes = false
    pos = position(ctx.fmt_io)
    ws = Node(JuliaSyntax.SyntaxHead(K"Whitespace", JuliaSyntax.TRIVIA_FLAG), 1)

    # Toggle for whether we are currently looking for whitespace or not
    looking_for_whitespace = false
    looking_for_x = false

    for (i, kid) in pairs(kids)
        if kind(kid) === K"NewlineWs" ||
            (i == 1 && kind(kid) === K"Whitespace")
            # NewlineWs are accepted as is by this pass.
            # Whitespace is accepted as is if this is the first kid even if the span is
            # larger than we expect since we don't look backwards. It should be cleaned up
            # by some other pass.
            accept_node!(ctx, kid)
            any_changes && push!(kids′, kid)
            looking_for_whitespace = false
        elseif looking_for_whitespace
            if kind(kid) === K"Whitespace" && span(kid) == 1
                # All good, just advance the IO
                accept_node!(ctx, kid)
                any_changes && push!(kids′, kid)
                looking_for_whitespace = false
            elseif kind(kid) === K"Whitespace"
                # Whitespace node but replace since not single space
                any_changes = true
                if kids′ === kids
                    kids′ = kids[1:i - 1]
                end
                push!(kids′, ws)
                replace_bytes!(ctx, " ", span(kid))
                accept_node!(ctx, ws)
                looking_for_whitespace = false
            elseif !is_leaf(kid) && kind(first_leaf(kid)) === K"Whitespace"
                # Whitespace found at the beginning of next kid.
                kid_ws = first_leaf(kid)
                looking_for_whitespace = kind(last_leaf(kid)) !== K"Whitespace"
                @assert !is_x(kid)::Bool
                looking_for_x = true
                if span(kid_ws) == 1
                    # Accept the node
                    accept_node!(ctx, kid)
                    any_changes && push!(kids′, kid)
                else
                    # Replace the whitespace node of the kid
                    kid′ = replace_first_leaf(kid, ws)
                    @assert span(kid′) == span(kid) - span(kid_ws) + 1
                    bytes_to_skip = span(kid) - span(kid′)
                    @assert bytes_to_skip > 0
                    replace_bytes!(ctx, "", bytes_to_skip)
                    accept_node!(ctx, kid′)
                    any_changes = true
                    if kids′ === kids
                        kids′ = kids[1:i - 1]
                    end
                    push!(kids′, kid′)
                end
            elseif !is_leaf(kid) && kind(first_leaf(kid)) === K"NewlineWs"
                # NewlineWs have to be accepted as is
                # @info "     ... kids first leaf is NewlineWs I'll take it"
                accept_node!(ctx, kid)
                any_changes && push!(kids′, kid)
                looking_for_whitespace = kind(last_leaf(kid)) !== K"Whitespace"
                @assert !is_x(kid)::Bool
                looking_for_x = true
            else
                # @info "     ... no whitespace, inserting" kind(kid)
                # Not a whitespace node, insert one
                any_changes = true
                if kids′ === kids
                    kids′ = kids[1:i - 1]
                end
                push!(kids′, ws)
                replace_bytes!(ctx, " ", 0)
                accept_node!(ctx, ws)
                # Write and accept the node
                push!(kids′, kid)
                accept_node!(ctx, kid)
                looking_for_whitespace = kind(last_leaf(kid)) !== K"Whitespace"
                if looking_for_x
                    @assert is_x(kid)::Bool
                end
                # Flip the switch, unless kid is a comment
                looking_for_x = kind(kid) === K"Comment" ? looking_for_x : !looking_for_x
            end
        else # !expect_ws
            if looking_for_x
                @assert is_x(kid)::Bool
            end
            @assert kind(kid) !== K"Whitespace" # This would be weird, I think?
            any_changes && push!(kids′, kid)
            accept_node!(ctx, kid)
            looking_for_whitespace = kind(last_leaf(kid)) !== K"Whitespace"
            # Flip the switch, unless kid is a comment
            looking_for_x = kind(kid) === K"Comment" ? looking_for_x : !looking_for_x
        end
    end
    # Reset stream
    seek(ctx.fmt_io, pos)
    if any_changes
        # Create new node and return it
        return make_node(node, kids′)
    else
        return nothing
    end
end

# This pass handles spaces around infix operator calls, comparison chains, and
# <: and >: operators.
function spaces_around_operators(ctx::Context, node::Node)
    if !(
        (is_infix_op_call(node) && !(kind(infix_op_call_op(node)) in KSet": ^")) ||
        (kind(node) in KSet"<: >:" && meta_nargs(node) == 3) ||
        (kind(node) === K"comparison" && !JuliaSyntax.is_trivia(node))
    )
        return nothing
    end
    @assert kind(node) in KSet"call comparison <: >:"
    is_x = x -> is_operator_leaf(x) || is_comparison_leaf(x)
    return spaces_around_x(ctx, node, is_x)
end

function spaces_around_assignments(ctx::Context, node::Node)
    if !(is_assignment(node) && !is_leaf(node) )
        return nothing
    end
    # for-loop nodes are of kind K"=" even when `in` or `∈` is used so we need to
    # include these kinds in the predicate too.
    is_x = x -> is_assignment(x) || kind(x) in KSet"in ∈"
    return spaces_around_x(ctx, node, is_x)
end

# Opposite of `spaces_around_x`: remove spaces around `x`
function no_spaces_around_x(ctx::Context, node::Node, is_x::F) where F
    @assert !is_leaf(node)
    # TODO: Can't handle NewlineWs here right now
    if any(kind(c) === K"NewlineWs" for c in verified_kids(node))
        return nothing
    end

    kids = verified_kids(node)
    kids′ = kids
    any_changes = false
    pos = position(ctx.fmt_io)

    looking_for_x = false

    # K"::", K"<:", and K">:" are special cases here since they can be used without an LHS
    # in e.g. `f(::Int) = ...` and `Vector{<:Real}`.
    if kind(node) in KSet":: <: >:"
        looking_for_x = is_x(first_non_whitespace_kid(node))::Bool
    end

    for (i, kid) in pairs(kids)
        if (i == 1 || i == length(kids)) && kind(kid) === K"Whitespace"
            accept_node!(ctx, kid)
            any_changes && push!(kids′, kid)
        elseif kind(kid) === K"Whitespace"
            # Ignore it but need to copy kids and re-write bytes
            any_changes = true
            if kids′ === kids
                kids′ = kids[1:i - 1]
            end
            replace_bytes!(ctx, "", span(kid))
        else
            @assert kind(kid) !== K"Whitespace"
            if looking_for_x
                @assert is_x(kid)::Bool
            end
            any_changes && push!(kids′, kid)
            accept_node!(ctx, kid)
            looking_for_x = !looking_for_x
        end
    end
    # Reset stream
    seek(ctx.fmt_io, pos)
    if any_changes
        # Create new node and return it
        node′ = make_node(node, kids′)
        @assert span(node′) < span(node)
        return node′
    else
        return nothing
    end
end

# no spaces around `:`, `^`, and `::`
function no_spaces_around_colon_etc(ctx::Context, node::Node)
    if !(
        (is_infix_op_call(node) && kind(infix_op_call_op(node)) in KSet": ^") ||
        (kind(node) === K"::" && !is_leaf(node)) ||
        (kind(node) in KSet"<: >:" && meta_nargs(node) == 2)
    )
        return nothing
    end
    @assert kind(node) in KSet"call :: <: >:"
    is_x = x -> is_leaf(x) && kind(x) in KSet": ^ :: <: >:"
    return no_spaces_around_x(ctx, node, is_x)
end

# Replace the K"=" operator with `in`
function replace_with_in(ctx::Context, node::Node)
    @assert kind(node) === K"=" && !is_leaf(node) && meta_nargs(node) == 3
    kids = verified_kids(node)
    vars_index = findfirst(!JuliaSyntax.is_whitespace, kids)
    # TODO: Need to insert whitespaces around `in` when replacing e.g. `i=I` with `iinI`.
    # However, at the moment it looks like the whitespace around operator pass does it's
    # thing first? I don't really know how though, because the for loop pass should be
    # happening before...
    in_index = findnext(!JuliaSyntax.is_whitespace, kids, vars_index + 1)
    in_node = kids[in_index]
    if kind(in_node) === K"in"
        @assert JuliaSyntax.is_trivia(in_node)
        @assert is_leaf(in_node)
        return nothing
    end
    @assert kind(in_node) in KSet"∈ ="
    @assert JuliaSyntax.is_trivia(in_node)
    @assert is_leaf(in_node)
    # Accept nodes to advance the stream
    for i in 1:(in_index - 1)
        accept_node!(ctx, kids[i])
    end
    # Construct the replacement
    nb = replace_bytes!(ctx, "in", span(in_node))
    in_node′ = Node(
        JuliaSyntax.SyntaxHead(K"in", JuliaSyntax.TRIVIA_FLAG), nb,
    )
    accept_node!(ctx, in_node′)
    kids′ = copy(kids)
    kids′[in_index] = in_node′
    # Accept remaining kids
    for i in (in_index + 1):length(kids′)
        accept_node!(ctx, kids′[i])
    end
    return make_node(node, kids′)
end

function replace_with_in_cartesian(ctx::Context, node::Node)
    @assert kind(node) === K"cartesian_iterator" && !is_leaf(node)
    kids = verified_kids(node)
    kids′ = kids
    for (i, kid) in pairs(kids)
        if kind(kid) === K"="
            kid′ = replace_with_in(ctx, kid)
            if kid′ !== nothing
                if kids′ === kids
                    kids′ = copy(kids)
                end
                kids′[i] = kid′
            else
                kids′[i] = kid
                accept_node!(ctx, kid)
            end
        else
            kids′[i] = kid
            accept_node!(ctx, kid)
        end
    end
    if kids === kids′
        return nothing
    end
    return make_node(node, kids′)
end

# replace `=` and `∈` with `in` in for-loops
function for_loop_use_in(ctx::Context, node::Node)
    if !(
        (kind(node) === K"for" && !is_leaf(node) && meta_nargs(node) == 4) ||
        (kind(node) === K"generator" && meta_nargs(node) == 3) # TODO: Unsure about 3.
    )
        return nothing
    end
    pos = position(ctx.fmt_io)
    kids = verified_kids(node)
    for_index = findfirst(c -> kind(c) === K"for" && is_leaf(c), kids)::Int
    for_node = kids[for_index]
    @assert kind(for_node) === K"for" && span(for_node) == 3 &&
        is_leaf(for_node) && JuliaSyntax.is_trivia(for_node)
    for i in 1:for_index
        accept_node!(ctx, kids[i])
    end
    # The for loop specification node can be either K"=" or K"cartesian_iterator"
    for_spec_index = for_index + 1
    for_spec_node = kids[for_spec_index]
    @assert kind(for_spec_node) in KSet"= cartesian_iterator"
    if kind(for_spec_node) === K"="
        for_spec_node′ = replace_with_in(ctx, for_spec_node)
    else
        @assert kind(for_spec_node) === K"cartesian_iterator"
        for_spec_node′ = replace_with_in_cartesian(ctx, for_spec_node)
    end
    if for_spec_node′ === nothing
        seek(ctx.fmt_io, pos)
        return nothing
    end
    @assert position(ctx.fmt_io) == pos + mapreduce(span, +, @view(kids[1:for_index])) + span(for_spec_node′)
    # Insert the new for spec node
    kids′ = copy(kids)
    kids′[for_spec_index] = for_spec_node′
    # At this point the eq node is done, just accept any remaining nodes
    # TODO: Don't need to do this...
    for i in (for_spec_index + 1):length(kids′)
        accept_node!(ctx, kids′[i])
    end
    # Construct the full node and return
    node′ = make_node(node, kids′)
    @assert position(ctx.fmt_io) == pos + span(node′)
    seek(ctx.fmt_io, pos) # reset
    return node′
end

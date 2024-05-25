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
    nb = write_and_reset(ctx, str′)
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
    nb = write_and_reset(ctx, bytes)
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
    nb = write_and_reset(ctx, bytes)
    @assert nb == length(bytes) == target_span
    # Create new node and return it
    node′ = JuliaSyntax.GreenNode(JuliaSyntax.head(node), nb, ())
    return node′
end

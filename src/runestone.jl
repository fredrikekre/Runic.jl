# SPDX-License-Identifier: MIT

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
function spaces_around_x(ctx::Context, node::Node, is_x::F, n_leaves_per_x::Int = 1) where {F}
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
    n_x_leaves_visited = 0

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
                    kids′ = kids[1:(i - 1)]
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
                        kids′ = kids[1:(i - 1)]
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
                    kids′ = kids[1:(i - 1)]
                end
                push!(kids′, ws)
                replace_bytes!(ctx, " ", 0)
                accept_node!(ctx, ws)
                # Write and accept the node
                push!(kids′, kid)
                accept_node!(ctx, kid)
                looking_for_whitespace = kind(last_leaf(kid)) !== K"Whitespace"
                # TODO: Duplicated with the branch below.
                if looking_for_x
                    @assert is_x(kid)::Bool
                    n_x_leaves_visited += 1
                    if n_x_leaves_visited == n_leaves_per_x
                        looking_for_x = false
                        n_x_leaves_visited = 0
                    else
                        looking_for_whitespace = false
                    end
                else
                    looking_for_x = kind(kid) !== K"Comment"
                end
            end
        else # !expect_ws
            # We end up here if we look for x, or the things in between x's
            @assert kind(kid) !== K"Whitespace" # This would be weird, I think?
            any_changes && push!(kids′, kid)
            accept_node!(ctx, kid)
            looking_for_whitespace = kind(last_leaf(kid)) !== K"Whitespace"
            if looking_for_x
                # We are looking for x, check we have them all otherwise keep looking
                @assert is_x(kid)::Bool
                n_x_leaves_visited += 1
                if n_x_leaves_visited == n_leaves_per_x
                    looking_for_x = false
                    n_x_leaves_visited = 0
                else
                    # Multiple x's is only for dotted operators and there should be no
                    # whitespace in between
                    looking_for_whitespace = false
                end
            else
                # This is a thing in between, but if it is a comment we still look for the
                # real thing in between
                looking_for_x = kind(kid) !== K"Comment"
            end
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

# Insert space after comma and semicolon in list-like expressions. Aim for the form
# `<nospace><item><comma><space><item><comma><space>...<item><nospace>`.
# TODO: Why did this function become sooo complicated?
function spaces_in_listlike(ctx::Context, node::Node)
    if !(
            kind(node) in KSet"tuple parameters curly braces bracescat vect ref" ||
                (kind(node) === K"call" && flags(node) == 0) || # Flag check rules out op-calls
                (kind(node) === K"dotcall" && flags(node) == 0) ||
                is_paren_block(node)
        )
        return nothing
    end
    if kind(node) === K"parameters"
        # TODO: Can probably show up elsewhere but...
        @assert ctx.lineage_kinds[end] in KSet"tuple call dotcall curly"
    end

    @assert !is_leaf(node)
    kids = verified_kids(node)
    kids′ = kids

    peek(i) = i < length(kids) ? kind(kids[i + 1]) : nothing

    ws = Node(JuliaSyntax.SyntaxHead(K"Whitespace", JuliaSyntax.TRIVIA_FLAG), 1)
    comma = Node(JuliaSyntax.SyntaxHead(K",", JuliaSyntax.TRIVIA_FLAG), 1)

    # Find the opening and closing leafs
    if kind(node) in KSet"tuple call dotcall" || is_paren_block(node)
        opening_leaf_idx = findfirst(x -> kind(x) === K"(", kids)
        if opening_leaf_idx === nothing
            # TODO: Implicit tuple without (), for example arguments in a do-block
            return nothing
        end
        closing_leaf_idx = findnext(x -> kind(x) === K")", kids, opening_leaf_idx + 1)::Int
        closing_leaf_idx == opening_leaf_idx + 1 && return nothing # empty
    elseif kind(node) in KSet"curly braces bracescat"
        opening_leaf_idx = findfirst(x -> kind(x) === K"{", kids)::Int
        closing_leaf_idx = findnext(x -> kind(x) === K"}", kids, opening_leaf_idx + 1)::Int
        closing_leaf_idx == opening_leaf_idx + 1 && return nothing # empty
    elseif kind(node) in KSet"vect ref"
        opening_leaf_idx = findfirst(x -> kind(x) === K"[", kids)::Int
        closing_leaf_idx = findnext(x -> kind(x) === K"]", kids, opening_leaf_idx + 1)::Int
        closing_leaf_idx == opening_leaf_idx + 1 && return nothing # empty
    else
        @assert kind(node) === K"parameters"
        opening_leaf_idx = findfirst(x -> kind(x) === K";", kids)::Int
        closing_leaf_idx = lastindex(kids) + 1
    end

    n_items = count(
        x -> !(JuliaSyntax.is_whitespace(x) || kind(x) === K","),
        @view(kids[(opening_leaf_idx + 1):(closing_leaf_idx - 1)]),
    )
    last_item_idx = findprev(x -> !(JuliaSyntax.is_whitespace(x) || kind(x) in KSet", ;"), kids, closing_leaf_idx - 1)
    if last_item_idx <= opening_leaf_idx
        last_item_idx = nothing
    end
    last_comma_idx = findprev(x -> kind(x) === K",", kids, closing_leaf_idx - 1)
    if last_comma_idx !== nothing && last_comma_idx <= opening_leaf_idx
        last_comma_idx = nothing
    end

    # Multiline lists require leading and trailing newline
    # multiline = contains_outer_newline(kids, opening_leaf_idx, closing_leaf_idx)
    multiline = any(y -> any_leaf(x -> kind(x) === K"NewlineWs", kids[y]), (opening_leaf_idx + 1):(closing_leaf_idx - 1))

    # A trailing comma is required if
    #  - node is a single item tuple which is not from an anonymous fn (Julia-requirement)
    #  - the closing token is not on the same line as the last item (Runic-requirement)
    require_trailing_comma = false
    if kind(node) === K"tuple" && n_items == 1 && ctx.lineage_kinds[end] !== K"function"
        # TODO: May also have to check for K"where" and K"::" in the lineage above
        require_trailing_comma = true
    elseif kind(node) in KSet"bracescat block"
        require_trailing_comma = false # Leads to parser error
    elseif kind(node) === K"parameters"
        # For parameters the trailing comma is configured from the parent
        require_trailing_comma = has_tag(node, TAG_TRAILING_COMMA)
    elseif multiline
        require_trailing_comma = true
    elseif n_items > 0
        require_trailing_comma = any(
            x -> kind(x) === K"NewlineWs", @view(kids[(last_item_idx + 1):(closing_leaf_idx - 1)]),
        ) || has_newline_after_non_whitespace(kids[last_item_idx])
    end

    # Helper to compute the new state after a given item
    function state_after_item(i)
        @assert i <= last_item_idx
        if i < last_item_idx
            return :expect_comma
        elseif i == last_item_idx && require_trailing_comma
            return :expect_comma
        else
            return :expect_closing
        end
    end

    # Keep track of the state
    state = if kind(node) === K"parameters"
        # @assert !multiline # TODO
        :expect_space
    elseif n_items > 0
        :expect_item
    else
        :expect_closing
    end
    any_kid_changed = false
    pos = position(ctx.fmt_io)

    # Accept kids up until the opening leaf
    for i in 1:opening_leaf_idx
        accept_node!(ctx, kids[i])
    end

    # Loop over the kids between the opening/closing tokens.
    for i in (opening_leaf_idx + 1):(closing_leaf_idx - 1)
        kid′ = kids[i]
        this_kid_changed = false
        if state === :expect_item
            if kind(kid′) === K"Whitespace" && peek(i) !== K"Comment"
                # Delete whitespace unless followed by a comment
                replace_bytes!(ctx, "", span(kid′))
                this_kid_changed = true
                if kids′ === kids
                    kids′ = kids[1:(i - 1)]
                end
            elseif kind(kid′) === K"NewlineWs" ||
                    (kind(kid′) === K"Whitespace" && peek(i) === K"Comment")
                # Newline here can happen if this kid is just after the opening leaf or if
                # there is an empty line between items. No state change.
                accept_node!(ctx, kid′)
                any_kid_changed && push!(kids′, kid′)
            elseif kind(kid′) === K"Comment"
                accept_node!(ctx, kid′)
                any_kid_changed && push!(kids′, kid′)
                state = :expect_space # To ensure space after the comment
            else
                # This is an item (probably?).
                # Make sure it doesn't have leading or trailing whitespace.
                if kind(first_leaf(kid′)) === K"Whitespace" && kind(second_leaf(kid′)) !== K"Comment"
                    # Delete the whitespace leaf
                    kid_ws = first_leaf(kid′)
                    replace_bytes!(ctx, "", span(kid_ws))
                    kid′ = replace_first_leaf(kid′, nullnode)
                    this_kid_changed = true
                end
                if kind(last_leaf(kid′)) === K"Whitespace"
                    @assert false # Unreachable?
                end
                # Kid is now acceptable
                any_kid_changed |= this_kid_changed
                if any_kid_changed
                    if kids′ === kids
                        kids′ = kids[1:(i - 1)]
                    end
                    push!(kids′, kid′)
                end
                accept_node!(ctx, kid′)
                # Transition to the next state
                state = state_after_item(i)
            end
        elseif state === :expect_comma
            trailing = i > last_item_idx
            if kind(kid′) === K"," || kind(kid′) === K";"
                before_last_item = i < last_item_idx
                if before_last_item || require_trailing_comma
                    # Nice, just accept it.
                    accept_node!(ctx, kid′)
                    any_kid_changed && push!(kids′, kid′)
                else
                    @assert false # Unreachable?
                end
                # Transition to the next state
                state = before_last_item ? (:expect_space) : (:expect_closing)
            elseif kind(kid′) === K"Whitespace" && peek(i) !== K"Comment"
                # Delete space (unless followed by a comment) and hope next is still comma
                # (no state change)
                this_kid_changed = true
                if kids′ === kids
                    kids′ = kids[1:(i - 1)]
                end
                replace_bytes!(ctx, "", span(kid′))
            elseif kind(kid′) === K"NewlineWs" ||
                    (kind(kid′) === K"Whitespace" && peek(i) === K"Comment") ||
                    kind(kid′) === K"Comment"
                # This branch can be reached if:
                #  - we have passed the last item and there is no trailing comma
                #  - there is a comma coming but it is on the next line (weird)
                #  - there is a comment with no space before it
                next_non_ws_idx = findnext(
                    !JuliaSyntax.is_whitespace, @view(kids[1:(closing_leaf_idx - 1)]), i + 1,
                )
                next_kind = next_non_ws_idx === nothing ? nothing : kind(kids[next_non_ws_idx])
                # Insert a comma if there isn't one coming
                if trailing && next_kind !== K","
                    @assert require_trailing_comma
                    this_kid_changed = true
                    if kids′ === kids
                        kids′ = kids[1:(i - 1)]
                    end
                    replace_bytes!(ctx, ",", 0)
                    push!(kids′, comma)
                    accept_node!(ctx, comma)
                    state = :expect_closing
                end
                if kind(kid′) === K"NewlineWs"
                    state = :expect_closing
                end
                any_kid_changed |= this_kid_changed
                # Accept the newline
                accept_node!(ctx, kid′)
                any_kid_changed && push!(kids′, kid′)
            elseif kind(kid′) === K"parameters"
                @assert kind(node) in KSet"call dotcall curly tuple" # TODO: Can this happen for named tuples?
                @assert i === last_item_idx
                @assert findnext(
                    !JuliaSyntax.is_whitespace, @view(kids[1:(closing_leaf_idx - 1)]), i + 1,
                ) === nothing
                if kind(first_leaf(kid′)) === K"Whitespace"
                    # Delete the whitespace leaf
                    kid_ws = first_leaf(kid′)
                    replace_bytes!(ctx, "", span(kid_ws))
                    kid′ = replace_first_leaf(kid′, nullnode)
                    this_kid_changed = true
                    # if kids′ === kids
                    #     kids′ = kids[1:i - 1]
                    # end
                end
                if require_trailing_comma && !has_tag(kid′, TAG_TRAILING_COMMA)
                    # Tag the parameters node to require a trailing comma
                    kid′ = add_tag(kid′, TAG_TRAILING_COMMA)
                    this_kid_changed = true
                    # if kids′ === kids
                    #     kids′ = kids[1:i - 1]
                    # end
                end
                # TODO: Tag for requiring trailing comma.
                any_kid_changed |= this_kid_changed
                accept_node!(ctx, kid′)
                if any_kid_changed
                    if kids′ === kids
                        kids′ = kids[1:(i - 1)]
                    end
                    push!(kids′, kid′)
                end
                state = :expect_closing # parameters must be the last item(?)
            else
                @assert false # Unreachable?
            end
        elseif state === :expect_space
            if (kind(kid′) === K"Whitespace" && span(kid′) == 1) ||
                    (kind(kid′) === K"Whitespace" && peek(i) === K"Comment")
                # Whitespace with correct span
                # Whitespace before a comment
                accept_node!(ctx, kid′)
                any_kid_changed && push!(kids′, kid′)
                state = :expect_item
            elseif kind(kid′) === K"Whitespace"
                # Wrong span, replace it
                this_kid_changed = true
                if kids′ === kids
                    kids′ = kids[1:(i - 1)]
                end
                replace_bytes!(ctx, " ", span(kid′))
                accept_node!(ctx, ws)
                push!(kids′, ws)
                # Transition to the next state
                state = :expect_item
            elseif kind(kid′) === K"NewlineWs"
                # NewlineWs are accepted and accounts for a space
                accept_node!(ctx, kid′)
                any_kid_changed && push!(kids′, kid′)
                state = :expect_item
            elseif kind(kid′) === K"Comment"
                # Comments are accepted, state stays the same
                # TODO: Make sure there is a space before the comment? Maybe that's not the
                # responsibility of this function though.
                accept_node!(ctx, kid′)
                any_kid_changed && push!(kids′, kid′)
            else
                # Probably a list item, look for leading whitespace, or insert.
                @assert !(kind(kid′) in KSet", ;")
                if kind(first_leaf(kid′)) === K"NewlineWs" ||
                        kind(first_leaf(kid′)) === K"Comment" ||
                        (kind(first_leaf(kid′)) === K"Whitespace" && kind(second_leaf(kid′)) === K"Comment")
                    # Newline, comment, or whitespace followed by comment
                    accept_node!(ctx, kid′)
                    any_kid_changed && push!(kids′, kid′)
                    state = state_after_item(i)
                elseif kind(first_leaf(kid′)) === K"Whitespace"
                    ws_node = first_leaf(kid′)
                    if span(ws_node) == 1
                        accept_node!(ctx, kid′)
                        any_kid_changed && push!(kids′, kid′)
                    else
                        kid′ = replace_first_leaf(kid′, ws)
                        this_kid_changed = true
                        if kids′ === kids
                            kids′ = kids[1:(i - 1)]
                        end
                        replace_bytes!(ctx, " ", span(ws_node))
                        accept_node!(ctx, kid′)
                        push!(kids′, kid′)
                    end
                    state = state_after_item(i)
                else
                    # Insert a standalone space kid and then accept the current node
                    this_kid_changed = true
                    if kids′ === kids
                        kids′ = kids[1:(i - 1)]
                    end
                    replace_bytes!(ctx, " ", 0)
                    push!(kids′, ws)
                    accept_node!(ctx, ws)
                    push!(kids′, kid′)
                    accept_node!(ctx, kid′)
                    # Here we inserted a space and consumed the next item, moving on to comma
                    state = state_after_item(i)
                end
            end
        else
            @assert state === :expect_closing
            if kind(kid′) === K"," ||
                    (kind(kid′) === K"Whitespace" && peek(i) !== K"Comment")
                # Trailing comma (when not wanted) and space not followed by a comment are
                # removed
                this_kid_changed = true
                if kids′ === kids
                    kids′ = kids[1:(i - 1)]
                end
                replace_bytes!(ctx, "", span(kid′))
            elseif kind(kid′) === K"NewlineWs" ||
                    (kind(kid′) === K"Whitespace" && peek(i) === K"Comment") ||
                    kind(kid′) === K"Comment"
                # Newlines, whitespace followed by comment, and comments are accepted.
                accept_node!(ctx, kid′)
                any_kid_changed && push!(kids′, kid′)
            else
                @assert false # Unreachable?
            end
        end # if-state
        any_kid_changed |= this_kid_changed
    end
    if state !== :expect_closing
        if state === :expect_comma
            # Need to add a trailing comma if it is expected
            @assert require_trailing_comma
            any_kid_changed = true
            if kids′ === kids
                kids′ = kids[1:(closing_leaf_idx - 1)]
            end
            replace_bytes!(ctx, ",", 0)
            push!(kids′, comma)
            accept_node!(ctx, comma)
            state = :expect_closing
        else
            @assert false # Unreachable?
        end
    end
    @assert state === :expect_closing
    # Accept kids after the closing leaf
    for i in closing_leaf_idx:length(kids)
        accept_node!(ctx, kids[i])
        any_kid_changed && push!(kids′, kids[i])
    end
    # Reset stream
    seek(ctx.fmt_io, pos)
    # Create a new node if any kids changed
    if any_kid_changed
        n = make_node(node, kids′)
        return n
    else
        @assert kids === kids′
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
    @assert kind(node) in KSet"call dotcall comparison <: >:"
    is_x = x -> is_operator_leaf(x) || is_comparison_leaf(x)
    n_leaves_per_x = kind(node) === K"dotcall" ? 2 : 1
    return spaces_around_x(ctx, node, is_x, n_leaves_per_x)
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

function spaces_around_anonymous_function(ctx::Context, node::Node)
    if !(kind(node) === K"->" && !is_leaf(node))
        return nothing
    end
    is_x = x -> kind(x) === K"->"
    return spaces_around_x(ctx, node, is_x)
end

function spaces_around_ternary(ctx::Context, node::Node)
    if !(kind(node) === K"?" && !is_leaf(node))
        return nothing
    end
    is_x = x -> is_leaf(x) && kind(x) in KSet"? :"
    return spaces_around_x(ctx, node, is_x)
end

# Opposite of `spaces_around_x`: remove spaces around `x`
function no_spaces_around_x(ctx::Context, node::Node, is_x::F) where {F}
    @assert !is_leaf(node)
    # TODO: Can't handle NewlineWs and comments here right now
    if any(kind(c) in KSet"NewlineWs Comment" for c in verified_kids(node))
        return nothing
    end

    kids = verified_kids(node)
    kids′ = kids
    any_changes = false
    pos = position(ctx.fmt_io)

    looking_for_x = false
    first_x_idx = findfirst(is_x, kids)::Int
    last_x_idx = findlast(is_x, kids)::Int

    # K"::", K"<:", and K">:" are special cases here since they can be used without an LHS
    # in e.g. `f(::Int) = ...` and `Vector{<:Real}`.
    if kind(node) in KSet":: <: >:"
        looking_for_x = is_x(first_non_whitespace_kid(node))::Bool
    end

    for (i, kid) in pairs(kids)
        if (i == 1 || i == length(kids)) && kind(kid) === K"Whitespace"
            # Leave any leading and trailing whitespace
            accept_node!(ctx, kid)
            any_changes && push!(kids′, kid)
        elseif kind(kid) === K"Whitespace"
            # Ignore it but need to copy kids and re-write bytes
            any_changes = true
            if kids′ === kids
                kids′ = kids[1:(i - 1)]
            end
            replace_bytes!(ctx, "", span(kid))
        else
            @assert !JuliaSyntax.is_whitespace(kid) # Filtered out above
            if looking_for_x
                @assert is_x(kid)::Bool
            else
                if i > first_x_idx
                    # Remove leading whitespace
                    ws_kid = first_leaf(kid)
                    if kind(ws_kid) === K"Whitespace"
                        kid = replace_first_leaf(kid, nullnode)
                        replace_bytes!(ctx, "", span(ws_kid))
                        any_changes = true
                    end
                end
                if i < last_x_idx
                    # Remove trailing whitespace
                    ws_kid = last_leaf(kid)
                    if kind(ws_kid) === K"Whitespace"
                        @assert false # Hope this doesn't happen often...
                    end
                end
            end
            if any_changes
                if kids === kids′
                    kids′ = kids[1:(i - 1)]
                end
                push!(kids′, kid)
            end
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

# Single space around keywords:
# Both sides of: `where`, `do` (if followed by arguments)
# Right hand side of: `mutable`, `struct`, `abstract`, `primitive`, `type`, `function` (if
# named function), `if`, `elseif`, `catch` (if followed by variable)
function spaces_around_keywords(ctx::Context, node::Node)
    is_leaf(node) && return nothing
    keyword_set = KSet"where do mutable struct abstract primitive type function if elseif catch"
    if !(kind(node) in keyword_set)
        return nothing
    end
    if is_longform_anon_function(node)
        # TODO: `function(` should have no space, handled elsewhere
        return nothing
    end
    kids = verified_kids(node)
    kids′ = kids
    any_changes = false
    pos = position(ctx.fmt_io)
    ws = Node(JuliaSyntax.SyntaxHead(K"Whitespace", JuliaSyntax.TRIVIA_FLAG), 1)

    peek_kinds = KSet"where do"
    state = kind(node) in peek_kinds ? (:peeking_for_keyword) : (:looking_for_keyword)
    keep_looking_for_keywords = false
    space_after = true

    for i in eachindex(kids)
        kid = kids[i]
        if state === :peeking_for_keyword
            nkid = kids[i + 1]
            if kind(nkid) in peek_kinds
                state = :looking_for_space
                keep_looking_for_keywords = true
                space_after = false
            else
                accept_node!(ctx, kid)
                any_changes && push!(kids′, kid)
                continue
            end
        end
        if state === :looking_for_keyword
            if kind(kid) in keyword_set
                accept_node!(ctx, kid)
                any_changes && push!(kids′, kid)
                if kind(kid) in KSet"mutable abstract primitive"
                    # These keywords are always followed by another keyword
                    keep_looking_for_keywords = true
                end
                state = :looking_for_space
                # `do` should only be followed by space if the argument-tuple is non-empty
                if kind(node) === K"do"
                    nkid = kids[i + 1]
                    @assert kind(nkid) === K"tuple"
                    if !any(!JuliaSyntax.is_whitespace, verified_kids(nkid))
                        state = :closing
                    end
                end
                # `catch` should only be followed by space if the error is caught in a var
                if kind(node) === K"catch"
                    nkid = kids[i + 1]
                    if kind(nkid) === K"false" && span(nkid) == 0
                        state = :closing
                    end
                end
            else
                accept_node!(ctx, kid)
                any_changes && push!(kids′, kid)
            end
        elseif state === :looking_for_space
            if (kind(kid) === K"Whitespace" && span(kid) == 1) ||
                    kind(kid) === K"NewlineWs"
                if kind(kid) === K"NewlineWs"
                    # Is a newline instead of a space accepted for any other case?
                    @assert kind(node) === K"where"
                end
                accept_node!(ctx, kid)
                any_changes && push!(kids′, kid)
            elseif kind(kid) === K"Whitespace"
                # Replace with single space.
                any_changes = true
                if kids′ === kids
                    kids′ = kids[1:(i - 1)]
                end
                replace_bytes!(ctx, " ", span(kid))
                push!(kids′, ws)
                accept_node!(ctx, ws)
            elseif space_after && kind(first_leaf(kid)) === K"Whitespace"
                kid_ws = first_leaf(kid)
                if span(kid_ws) == 1
                    accept_node!(ctx, kid)
                    any_changes && push!(kids′, kid)
                else
                    kid′ = replace_first_leaf(kid, ws)
                    @assert span(kid′) == span(kid) - span(kid_ws) + 1
                    replace_bytes!(ctx, " ", span(kid_ws))
                    accept_node!(ctx, kid′)
                    any_changes = true
                    if kids′ === kids
                        kids′ = kids[1:(i - 1)]
                    end
                    push!(kids′, kid′)
                end
            elseif !space_after && kind(last_leaf(kid)) === K"Whitespace"
                @assert false # Unreachable?
            else
                # Reachable in e.g. `T where{T}`, insert space
                @assert kind(node) === K"where"
                any_changes = true
                if kids′ === kids
                    kids′ = kids[1:(i - 1)]
                end
                # Insert the space before/after the kid depending on whether we are looking
                # for a space before or after a keyword
                if !space_after
                    push!(kids′, kid)
                    accept_node!(ctx, kid)
                end
                replace_bytes!(ctx, " ", 0)
                push!(kids′, ws)
                accept_node!(ctx, ws)
                if space_after
                    push!(kids′, kid)
                    accept_node!(ctx, kid)
                end
            end
            state = keep_looking_for_keywords ? (:looking_for_keyword) : (:closing)
            keep_looking_for_keywords = false
            space_after = true
        else
            @assert state === :closing
            accept_node!(ctx, kid)
            any_changes && push!(kids′, kid)
        end
    end

    # Reset stream
    seek(ctx.fmt_io, pos)
    # Return
    if any_changes
        # Construct the new node
        node′ = make_node(node, kids′)
        return node′
    else
        return nothing
    end
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

function braces_around_where_rhs(ctx::Context, node::Node)
    if !(kind(node) === K"where" && !is_leaf(node))
        return nothing
    end
    kids = verified_kids(node)
    kids′ = kids
    # any_changes = false
    pos = position(ctx.fmt_io)
    where_idx = findfirst(x -> is_leaf(x) && kind(x) === K"where", kids)::Int
    rhs_idx = findnext(!JuliaSyntax.is_whitespace, kids, where_idx + 1)::Int
    rhs = kids[rhs_idx]
    if kind(rhs) === K"braces"
        return nothing
    end
    # Wrap the rhs in a braces node
    kids′ = kids[1:(rhs_idx - 1)]
    for i in 1:(rhs_idx - 1)
        accept_node!(ctx, kids[i])
    end
    opening_brace = Node(JuliaSyntax.SyntaxHead(K"{", 0), 1)
    closing_brace = Node(JuliaSyntax.SyntaxHead(K"}", 0), 1)
    rhs′ = Node(
        JuliaSyntax.SyntaxHead(K"braces", 0),
        [opening_brace, rhs, closing_brace],
    )
    push!(kids′, rhs′)
    # Write the new node
    replace_bytes!(ctx, "{", 0)
    accept_node!(ctx, opening_brace)
    accept_node!(ctx, rhs)
    replace_bytes!(ctx, "}", 0)
    accept_node!(ctx, closing_brace)
    # Accept any remaining kids
    for i in (rhs_idx + 1):length(kids)
        accept_node!(ctx, kids[i])
        push!(kids′, kids[i])
    end
    # Reset stream and return
    seek(ctx.fmt_io, pos)
    return make_node(node, kids′)
end

function parens_around_op_calls_in_colon(ctx::Context, node::Node)
    if !(is_infix_op_call(node) && kind(infix_op_call_op(node)) === K":")
        return nothing
    end

    kids = verified_kids(node)
    kids′ = kids
    any_changes = false
    pos = position(ctx.fmt_io)

    for i in eachindex(kids)
        kid = kids[i]
        if is_infix_op_call(kid)
            if kids′ === kids
                kids′ = kids[1:(i - 1)]
            end
            grandkids = verified_kids(kid)
            first_non_ws = findfirst(!JuliaSyntax.is_whitespace, grandkids)::Int
            last_non_ws = findlast(!JuliaSyntax.is_whitespace, grandkids)::Int
            # Extract whitespace grandkids to become kids
            for j in 1:(first_non_ws - 1)
                accept_node!(ctx, grandkids[j])
                push!(kids′, grandkids[j])
            end
            # Create the parens node
            opening_paren = Node(JuliaSyntax.SyntaxHead(K"(", 0), 1)
            replace_bytes!(ctx, "(", 0)
            accept_node!(ctx, opening_paren)
            parens_kids = [opening_paren]
            kid′_kids = grandkids[first_non_ws:last_non_ws]
            kid′ = make_node(kid, kid′_kids)
            accept_node!(ctx, kid′)
            push!(parens_kids, kid′)
            closing_paren = Node(JuliaSyntax.SyntaxHead(K")", 0), 1)
            replace_bytes!(ctx, ")", 0)
            accept_node!(ctx, closing_paren)
            push!(parens_kids, closing_paren)
            parens = Node(JuliaSyntax.SyntaxHead(K"parens", 0), parens_kids)
            push!(kids′, parens)
            for j in (last_non_ws + 1):length(grandkids)
                accept_node!(ctx, grandkids[j])
                push!(kids′, grandkids[j])
            end
            any_changes = true
        else
            accept_node!(ctx, kid)
            any_changes && push!(kids′, kid)
        end
    end
    # Reset stream
    seek(ctx.fmt_io, pos)
    # Rebuild node and return
    if any_changes
        node′ = make_node(node, kids′)
        return node′
    else
        return nothing
    end
end

# This function materialized all indentations marked by `insert_delete_mark_newlines`.
function four_space_indent(ctx::Context, node::Node)
    kind(node) === K"NewlineWs" || return nothing
    next_sibling_kind(ctx) === K"NewlineWs" && return
    bytes = read_bytes(ctx, node)
    @assert !in(UInt8('\r'), bytes)
    @assert bytes[1] == UInt8('\n')
    indent_level = ctx.indent_level
    # TAG_PRE_DEDENT means this is the newline just before an `end`
    if has_tag(node, TAG_PRE_DEDENT)
        indent_level -= 1
    end
    # TAG_LINE_CONT is a "soft" indentation
    if has_tag(node, TAG_LINE_CONT)
        indent_level += 1
    end
    spn′ = 1 + 4 * indent_level
    spn = span(node)
    if spn == spn′
        return nothing
    end
    resize!(bytes, spn′)
    fill!(@view(bytes[2:end]), UInt8(' '))
    replace_bytes!(ctx, bytes, spn)
    node′ = Node(head(node), spn′, (), node.tags)
    return node′
end

# This function tags the `function`/`macro` and `end` keywords as well as the trailing
# newline of the function/macro body.
function indent_function_or_macro(ctx::Context, node::Node)
    kids = verified_kids(node)
    any_kid_changed = false
    # First node is the function/macro keyword
    func_idx = 1
    func_node = kids[func_idx]
    @assert is_leaf(func_node) && kind(func_node) in KSet"function macro"
    if !has_tag(func_node, TAG_INDENT)
        kids[func_idx] = add_tag(func_node, TAG_INDENT)
        any_kid_changed = true
    end
    # Second node is the space between keyword and name
    if !is_longform_anon_function(node)
        space_idx = 2
        space_node = kids[space_idx]
        @assert is_leaf(space_node) && kind(space_node) === K"Whitespace"
    end
    # Third node is the signature (call/where/::) for standard method definitions but just
    # an Identifier for cases like `function f end`.
    sig_idx = findnext(x -> !JuliaSyntax.is_whitespace(x), kids, func_idx + 1)::Int
    if sig_idx == 2
        # Only case where no space is needed after the keyword
        @assert is_longform_anon_function(node)
    end
    sig_node = kids[sig_idx]
    if kind(sig_node) === K"Identifier"
        # Empty function definition like `function f end`.
        # TODO: Make sure the spaces around are correct
        end_idx = findnext(x -> kind(x) === K"end", kids, sig_idx + 1)::Int
        end_node = kids[end_idx]
        @assert is_leaf(end_node) && kind(end_node) === K"end"
        if !has_tag(end_node, TAG_DEDENT)
            kids[end_idx] = add_tag(end_node, TAG_DEDENT)
            any_kid_changed = true
        end
        return any_kid_changed ? node : nothing
    end
    # K"tuple" when this is an anonymous function
    @assert !is_leaf(sig_node) && kind(sig_node) in KSet"call where :: tuple"
    # Fourth node is the function/macro body block.
    block_idx = sig_idx + 1
    block_node′ = indent_block(ctx, kids[block_idx])
    if block_node′ !== nothing
        kids[block_idx] = block_node′
        any_kid_changed = true
    end
    # Fifth node is the closing end keyword
    end_idx = block_idx + 1
    end_node = kids[end_idx]
    @assert is_leaf(end_node) && kind(end_node) === K"end"
    if !has_tag(end_node, TAG_DEDENT)
        kids[end_idx] = add_tag(end_node, TAG_DEDENT)
        any_kid_changed = true
    end
    @assert verified_kids(node) === kids
    return any_kid_changed ? node : nothing
end

function indent_let(ctx::Context, node::Node)
    kids = verified_kids(node)
    any_kid_changed = false
    # First node is the let keyword
    let_idx = 1
    let_node = kids[let_idx]
    @assert is_leaf(let_node) && kind(let_node) === K"let"
    if !has_tag(let_node, TAG_INDENT)
        kids[let_idx] = add_tag(let_node, TAG_INDENT)
        any_kid_changed = true
    end
    # Second node is the variables block (will be soft-indented by the assignments pass)
    vars_idx = 2
    vars_node = kids[vars_idx]
    @assert !is_leaf(vars_node) && kind(vars_node) === K"block"
    @assert kind(last_leaf(vars_node)) !== "NewlineWs"
    # Third node is the NewlineWs before the block
    ln_idx = 3
    ln_node = kids[ln_idx]
    @assert is_leaf(ln_node) && kind(ln_node) === K"NewlineWs"
    # Fourth node is the function body block.
    block_idx = 4
    block_node = kids[block_idx]
    @assert !is_leaf(block_node) && kind(block_node) === K"block"
    block_node′ = indent_block(ctx, block_node)
    if block_node′ !== nothing
        kids[block_idx] = block_node′
        any_kid_changed = true
    end
    # Fifth node is the closing end keyword
    end_idx = 5
    @assert is_leaf(kids[end_idx]) && kind(kids[end_idx]) === K"end"
    if !has_tag(kids[end_idx], TAG_DEDENT)
        kids[end_idx] = add_tag(kids[end_idx], TAG_DEDENT)
        any_kid_changed = true
    end
    @assert verified_kids(node) === kids
    return any_kid_changed ? node : nothing
end

# TODO: Reuse indent_block?
function indent_begin(ctx::Context, node::Node, block_kind = K"begin")
    kids = verified_kids(node)
    any_kid_changed = false
    # First node is the begin keyword
    begin_idx = 1
    begin_node = kids[begin_idx]
    @assert is_leaf(begin_node) && kind(begin_node) === block_kind
    if !has_tag(begin_node, TAG_INDENT)
        kids[begin_idx] = add_tag(begin_node, TAG_INDENT)
        any_kid_changed = true
    end
    # Second node is the newline
    ln_idx = 2
    ln_node = kids[ln_idx]
    @assert is_leaf(ln_node) && kind(ln_node) === K"NewlineWs"
    # After the NewlineWs node we skip over all kids until the end
    end_idx = findlast(x -> kind(x) === K"end", kids)
    @assert end_idx == lastindex(kids) # ??
    # Tag last newline as pre-dedent
    ln_idx = end_idx - 1
    ln_node = kids[ln_idx]
    if kind(ln_node) === K"NewlineWs"
        if !has_tag(ln_node, TAG_PRE_DEDENT)
            kids[ln_idx] = add_tag(ln_node, TAG_PRE_DEDENT)
            any_kid_changed = true
        end
    end
    end_node = kids[end_idx]
    @assert is_leaf(end_node) && kind(end_node) === K"end"
    if !has_tag(end_node, TAG_DEDENT)
        kids[end_idx] = add_tag(end_node, TAG_DEDENT)
        any_kid_changed = true
    end
    @assert verified_kids(node) === kids
    return any_kid_changed ? node : nothing
end

# TODO: This needs to be reworked to handle non-standard cases like, for example, one-liners
# of the form `if x y end`. For now we only handle the standard case and ignore the rest.
function indent_block(::Context, node::Node)
    @assert kind(node) === K"block" && !is_leaf(node)
    kids = verified_kids(node)
    any_kid_changed = false
    # Expect a NewlineWs node at the end of the block (otherwise the closing `end` is not on
    # a separate line).
    trailing_idx = findlast(x -> kind(x) === K"NewlineWs", kids)
    if trailing_idx === nothing || trailing_idx != lastindex(kids)
        return nothing
    elseif !has_tag(kids[trailing_idx], TAG_PRE_DEDENT)
        kids[trailing_idx] = add_tag(kids[trailing_idx], TAG_PRE_DEDENT)
        any_kid_changed = true
    end
    # Look for a leading NewlineWs node
    leading_idx = findfirst(x -> kind(x) === K"NewlineWs", kids)
    if leading_idx !== nothing && leading_idx < trailing_idx
        # TODO: Forgot why we check for this. I think it is only necessary if we want to
        # split a one-liner into multiple lines.
        # return nothing
    end
    @assert verified_kids(node) === kids
    return any_kid_changed ? node : nothing
end

function indent_catch(ctx::Context, node::Node)
    @assert kind(node) in KSet"catch else finally"
    kids = verified_kids(node)
    any_kid_changed = false
    catch_idx = 1
    catch_node = kids[catch_idx]
    @assert is_leaf(catch_node) && kind(catch_node) in KSet"catch else finally"
    if !has_tag(catch_node, TAG_INDENT)
        kids[catch_idx] = add_tag(catch_node, TAG_INDENT)
        any_kid_changed = true
    end
    if !has_tag(catch_node, TAG_DEDENT)
        kids[catch_idx] = add_tag(catch_node, TAG_DEDENT)
        any_kid_changed = true
    end
    # Skip over the catch-identifier (if any)
    block_idx = findnext(x -> kind(x) === K"block", kids, catch_idx + 1)::Int
    @assert kind(kids[block_idx]) === K"block"
    block_node′ = indent_block(ctx, kids[block_idx])
    if block_node′ !== nothing
        kids[block_idx] = block_node′
        any_kid_changed = true
    end
    return any_kid_changed ? node : nothing
end

function indent_try(ctx::Context, node::Node)
    @assert kind(node) in KSet"try"
    @assert !is_leaf(node)
    kids = verified_kids(node)
    any_kid_changed = false
    # First node is `try`
    try_idx = 1
    try_node = kids[try_idx]
    @assert is_leaf(kids[try_idx]) && kind(try_node) in KSet"try"
    if !has_tag(try_node, TAG_INDENT)
        kids[try_idx] = add_tag(try_node, TAG_INDENT)
        any_kid_changed = true
    end
    # Second node the try-block
    try_block_idx = findnext(!JuliaSyntax.is_whitespace, kids, try_idx + 1)::Int
    try_block_node′ = indent_block(ctx, kids[try_block_idx])
    if try_block_node′ !== nothing
        kids[try_block_idx] = try_block_node′
        any_kid_changed = true
    end
    # Check for catch/finally. They can be in any order
    catch_idx = findnext(x -> kind(x) in KSet"catch finally", kids, try_block_idx + 1)::Int
    @assert !is_leaf(kids[catch_idx]) && kind(kids[catch_idx]) in KSet"catch finally"
    catch_node′ = indent_catch(ctx, kids[catch_idx])
    if catch_node′ !== nothing
        kids[catch_idx] = catch_node′
        any_kid_changed = true
    end
    # There may be an else in between catch and finally (lol)
    else_idx = findnext(x -> kind(x) === K"else", kids, catch_idx + 1)
    if else_idx !== nothing
        else_node′ = indent_catch(ctx, kids[else_idx])
        if else_node′ !== nothing
            kids[else_idx] = else_node′
            any_kid_changed = true
        end
    end
    # Check for the other one
    other_kind = kind(kids[catch_idx]) === K"catch" ? K"finally" : K"catch"
    finally_idx = findnext(
        x -> kind(x) === other_kind, kids, something(else_idx, catch_idx) + 1,
    )
    if finally_idx !== nothing
        finally_node′ = indent_catch(ctx, kids[finally_idx])
        if finally_node′ !== nothing
            kids[finally_idx] = finally_node′
            any_kid_changed = true
        end
    end
    # Check for end
    end_idx = findnext(
        x -> kind(x) === K"end", kids, something(finally_idx, else_idx, catch_idx) + 1,
    )::Int
    @assert is_leaf(kids[end_idx]) && kind(kids[end_idx]) === K"end"
    if !has_tag(kids[end_idx], TAG_DEDENT)
        kids[end_idx] = add_tag(kids[end_idx], TAG_DEDENT)
        any_kid_changed = true
    end
    @assert verified_kids(node) === kids
    return any_kid_changed ? node : nothing
end

function indent_if(ctx::Context, node::Node)
    @assert kind(node) in KSet"if elseif"
    @assert !is_leaf(node)
    kids = verified_kids(node)
    any_kid_changed = false
    # First node is either `if` or `elseif` (when called recursively)
    if_idx = 1
    if_node = kids[if_idx]
    @assert is_leaf(kids[if_idx]) && kind(if_node) in KSet"if elseif"
    if !has_tag(if_node, TAG_INDENT)
        if_node = add_tag(if_node, TAG_INDENT)
        any_kid_changed = true
    end
    if kind(node) === K"elseif" && !has_tag(if_node, TAG_DEDENT)
        if_node = add_tag(if_node, TAG_DEDENT)
        any_kid_changed = true
    end
    kids[if_idx] = if_node
    # Look for the condition node
    cond_idx = findnext(!JuliaSyntax.is_whitespace, kids, if_idx + 1)::Int
    if cond_idx != if_idx + 1
        # TODO: Trim whitespace between the keyword and the condition. It may exist as a
        # separate leaf, or hidden in the condition node.
    end
    cond_node = kids[cond_idx]
    @assert kind(last_leaf(cond_node)) !== "NewlineWs"
    # Fourth node is the body block.
    block_idx = findnext(!JuliaSyntax.is_whitespace, kids, cond_idx + 1)::Int
    @assert block_idx == cond_idx + 1
    block_node′ = indent_block(ctx, kids[block_idx])
    if block_node′ !== nothing
        kids[block_idx] = block_node′
        any_kid_changed = true
    end
    # Check for elseif
    elseif_idx = findnext(x -> kind(x) === K"elseif", kids, block_idx + 1)
    if elseif_idx !== nothing
        @assert !is_leaf(kids[elseif_idx]) && kind(kids[elseif_idx]) === K"elseif"
        elseif_node′ = indent_if(ctx, kids[elseif_idx])
        if elseif_node′ !== nothing
            kids[elseif_idx] = elseif_node′
            any_kid_changed = true
        end
    end
    # Check for else
    else_idx = findnext(x -> kind(x) === K"else", kids, something(elseif_idx, block_idx) + 1)
    if else_idx !== nothing
        @assert is_leaf(kids[else_idx]) && kind(kids[else_idx]) === K"else"
        else_node = kids[else_idx]
        if !has_tag(else_node, TAG_INDENT)
            else_node = add_tag(else_node, TAG_INDENT)
            any_kid_changed = true
        end
        if !has_tag(else_node, TAG_DEDENT)
            else_node = add_tag(else_node, TAG_DEDENT)
            any_kid_changed = true
        end
        kids[else_idx] = else_node
        else_block_idx = findnext(!JuliaSyntax.is_whitespace, kids, else_idx + 1)::Int
        @assert kind(kids[else_block_idx]) === K"block"
        else_block′ = indent_block(ctx, kids[else_block_idx])
        if else_block′ !== nothing
            kids[else_block_idx] = else_block′
            any_kid_changed = true
        end
    end
    # Check for end
    end_idx = findnext(x -> kind(x) === K"end", kids, something(else_idx, elseif_idx, block_idx) + 1)
    @assert (kind(node) === K"elseif") == (end_idx === nothing)
    if end_idx !== nothing
        @assert is_leaf(kids[end_idx]) && kind(kids[end_idx]) === K"end"
        if !has_tag(kids[end_idx], TAG_DEDENT)
            kids[end_idx] = add_tag(kids[end_idx], TAG_DEDENT)
            any_kid_changed = true
        end
    end
    @assert verified_kids(node) === kids
    return any_kid_changed ? node : nothing
end

function indent_call(ctx::Context, node::Node)
    @assert kind(node) in KSet"call dotcall"
    return indent_paren(ctx, node)
end


function indent_newlines_between_indices(
        ctx::Context, node::Node, open_idx::Int, close_idx::Int;
        indent_closing_token::Bool = false,
    )
    kids = verified_kids(node)
    any_kid_changed = false
    for i in open_idx:close_idx
        kid = kids[i]
        this_kid_changed = false
        # Skip the newline just before the closing token for e.g. (...\n)
        # (indent_closing_token = false) but not in e.g. `a+\nb` (indent_closing_token =
        # true) where the closing token is part of the expression itself.
        if !indent_closing_token && i == close_idx - 1 && kind(kid) === K"NewlineWs"
            continue
        end
        # Tag all direct NewlineWs kids
        if kind(kid) === K"NewlineWs" && !has_tag(kid, TAG_LINE_CONT)
            kid = add_tag(kid, TAG_LINE_CONT)
            this_kid_changed = true
        end
        # NewlineWs nodes can also hide as the first or last leaf of a node, tag'em.
        # Skip leading newline if this kid is the first one
        leading = i != open_idx
        # Skip trailing newline of this kid if the next token is the closing one and the
        # closing token should not be indented.
        trailing = !(i == close_idx - 1 && !indent_closing_token)
        kid′ = continue_newlines(kid; leading = leading, trailing = trailing)
        if kid′ !== nothing
            kid = kid′
            this_kid_changed = true
        end
        if this_kid_changed
            kids[i] = kid
        end
        any_kid_changed |= this_kid_changed
    end
    @assert verified_kids(node) === kids
    return any_kid_changed ? node : nothing
end

# Tags opening and closing tokens for indent/dedent and the newline just before the closing
# token as pre-dedent
function indent_listlike(
        ctx::Context, node::Node, open_idx::Int, close_idx::Int;
        indent_closing_token::Bool = false,
    )
    kids = verified_kids(node)
    kids′ = kids
    any_kid_changed = false
    # Bail early if there is just a single item
    open_idx == close_idx && return nothing
    # Check whether we expect leading/trailing newlines
    # multiline = contains_outer_newline(kids, open_idx, close_idx)
    multiline = any(y -> any_leaf(x -> kind(x) === K"NewlineWs", kids[y]), (open_idx + 1):(close_idx - 1))
    if !multiline
        # TODO: This should be fine? If there are no newlines it should be safe to just
        # don't indent anything in this node?
        return
    end
    pos = position(ctx.fmt_io)

    # Leave all initial kids the same
    for i in 1:(open_idx - 1)
        accept_node!(ctx, kids[i])
    end

    # Opening token indents
    kid = kids[open_idx]
    @assert is_leaf(kid)
    @assert kind(kid) !== K"NewlineWs"
    if !has_tag(kid, TAG_INDENT)
        kid = add_tag(kid, TAG_INDENT)
        if kids′ === kids
            kids′ = kids[1:(open_idx - 1)]
        end
        any_kid_changed = true
    end
    any_kid_changed && push!(kids′, kid)
    accept_node!(ctx, kid)
    # Next we expect the leading newline
    @assert multiline
    kid = kids[open_idx + 1]
    if kind(kid) === K"NewlineWs" ||
            kind(first_leaf(kid)) === K"NewlineWs"
        # Newline or newlinde hidden in first item
        any_kid_changed && push!(kids′, kid)
        accept_node!(ctx, kid)
    else
        # Need to insert a newline
        if kind(kid) === K"Whitespace"
            # Merge with the whitespace. It shouldn't matter if the newline is put before or
            # after the space. If put before the space will be handled by the indent pass
            # and if put after it will be handled by the trailing spaces pass.
            kid = Node(JuliaSyntax.SyntaxHead(K"NewlineWs", JuliaSyntax.TRIVIA_FLAG), span(kid) + 1)
            replace_bytes!(ctx, "\n", 0)
            if kids′ === kids
                kids′ = kids[1:(open_idx - 1)]
            end
            any_kid_changed = true
            push!(kids′, kid)
            accept_node!(ctx, kid)
        elseif kind(first_leaf(kid)) === K"Whitespace"
            grandkid = first_leaf(kid)
            grandkid = Node(JuliaSyntax.SyntaxHead(K"NewlineWs", JuliaSyntax.TRIVIA_FLAG), span(grandkid) + 1)
            replace_bytes!(ctx, "\n", 0)
            kid = replace_first_leaf(kid, grandkid)
            if kids′ === kids
                kids′ = kids[1:(open_idx - 1)]
            end
            any_kid_changed = true
            push!(kids′, kid)
            accept_node!(ctx, kid)
        else
            nlws = Node(JuliaSyntax.SyntaxHead(K"NewlineWs", JuliaSyntax.TRIVIA_FLAG), 1)
            replace_bytes!(ctx, "\n", 0)
            if kids′ === kids
                kids′ = kids[1:(open_idx - 1)]
            end
            any_kid_changed = true
            push!(kids′, nlws)
            accept_node!(ctx, nlws)
            push!(kids′, kid)
            accept_node!(ctx, kid)
        end
    end
    # Bring all kids between the opening and closing token to the new list
    for i in (open_idx + 2):(close_idx - 2)
        kid = kids[i]
        any_kid_changed && push!(kids′, kid)
        accept_node!(ctx, kid)
    end
    # Kid just before the closing token should be a newline and it should be tagged with
    # pre-dedent.
    if close_idx - 1 == open_idx + 1
        # Just a single kid which should then have both leading and trailing newline
        if any_kid_changed
            # Modify this kid again by popping from the list and backtrack the stream
            kid = pop!(kids′)
            seek(ctx.fmt_io, position(ctx.fmt_io) - span(kid))
        end
    else
        kid = kids[close_idx - 1]
    end
    if (kind(kid) === K"NewlineWs" && has_tag(kid, TAG_PRE_DEDENT)) ||
            (kind(last_leaf(kid)) === K"NewlineWs" && has_tag(last_leaf(kid), TAG_PRE_DEDENT))
        # Newline or newlinde hidden in first item with tag
        any_kid_changed && push!(kids′, kid)
        accept_node!(ctx, kid)
    elseif kind(kid) === K"NewlineWs"
        # Newline without tag
        @assert !has_tag(kid, TAG_PRE_DEDENT)
        kid = add_tag(kid, TAG_PRE_DEDENT)
        if kids′ === kids
            kids′ = kids[1:(close_idx - 2)]
        end
        any_kid_changed = true
        push!(kids′, kid)
        accept_node!(ctx, kid)
    elseif kind(last_leaf(kid)) === K"NewlineWs"
        # @assert false # Testcase?
        # Hidden newline without tag
        grandkid = last_leaf(kid)
        @assert !has_tag(grandkid, TAG_PRE_DEDENT)
        grandkid = add_tag(grandkid, TAG_PRE_DEDENT)
        kid = replace_last_leaf(kid, grandkid)
        if kids′ === kids
            kids′ = kids[1:(close_idx - 2)]
        end
        any_kid_changed = true
        push!(kids′, kid)
        accept_node!(ctx, kid)
    else
        # Need to insert a newline. Note that we tag the new newline directly since it
        # is the responsibility of this function (otherwise there would just be an extra
        # repetitive call to add it anyway).
        if kind(kid) === K"Whitespace"
            # Merge with the whitespace
            kid = Node(JuliaSyntax.SyntaxHead(K"NewlineWs", JuliaSyntax.TRIVIA_FLAG), span(kid) + 1)
            kid = add_tag(kid, TAG_PRE_DEDENT)
            replace_bytes!(ctx, "\n", 0)
            if kids′ === kids
                kids′ = kids[1:(open_idx - 1)]
            end
            any_kid_changed = true
            push!(kids′, kid)
            accept_node!(ctx, kid)
        elseif kind(last_leaf(kid)) === K"Whitespace"
            # TODO: Testcase? Need to merge here.
            @assert false
        else
            # Note that this is a trailing newline and should be put after this item
            if kids′ === kids
                kids′ = kids[1:(open_idx - 1)]
            end
            any_kid_changed = true
            push!(kids′, kid)
            accept_node!(ctx, kid)
            nlws = Node(JuliaSyntax.SyntaxHead(K"NewlineWs", JuliaSyntax.TRIVIA_FLAG), 1)
            nlws = add_tag(nlws, TAG_PRE_DEDENT)
            replace_bytes!(ctx, "\n", 0)
            push!(kids′, nlws)
            accept_node!(ctx, nlws)
        end
    end
    # Closing token dedents
    kid = kids[close_idx]
    @assert is_leaf(kid)
    if !has_tag(kid, TAG_DEDENT)
        kid = add_tag(kid, TAG_DEDENT)
        if kids′ === kids
            kids′ = kids[1:(close_idx - 1)]
        end
        any_kid_changed = true
    end
    any_kid_changed && push!(kids′, kid)
    accept_node!(ctx, kid)
    # Keep any remaining kids
    for i in (close_idx + 1):length(kids)
        kid = kids[i]
        any_kid_changed && push!(kids′, kid)
        accept_node!(ctx, kid)
    end
    # Reset stream
    seek(ctx.fmt_io, pos)
    # Make a new node and return
    return any_kid_changed ? make_node(node, kids′) : nothing
end

# Mark opening and closing parentheses, in a call or a tuple, with indent and dedent tags.
function indent_paren(ctx::Context, node::Node)
    @assert kind(node) in KSet"call dotcall tuple parens"
    kids = verified_kids(node)
    opening_paren_idx = findfirst(x -> kind(x) === K"(", kids)::Int
    closing_paren_idx = findnext(x -> kind(x) === K")", kids, opening_paren_idx + 1)::Int
    return indent_listlike(ctx, node, opening_paren_idx, closing_paren_idx)
end

function indent_braces(ctx::Context, node::Node)
    @assert kind(node) in KSet"curly braces bracescat"
    kids = verified_kids(node)
    opening_brace_idx = findfirst(x -> kind(x) === K"{", kids)::Int
    closing_brace_idx = findnext(x -> kind(x) === K"}", kids, opening_brace_idx + 1)::Int
    return indent_listlike(ctx, node, opening_brace_idx, closing_brace_idx)
end

# Insert line-continuation nodes instead of bumping the indent level.
function indent_op_call(ctx::Context, node::Node)
    kids = verified_kids(node)
    first_operand_idx = findfirst(!JuliaSyntax.is_whitespace, kids)::Int
    last_operand_idx = findlast(!JuliaSyntax.is_whitespace, kids)::Int
    return indent_newlines_between_indices(
        ctx, node, first_operand_idx, last_operand_idx; indent_closing_token = true,
    )
end

function indent_loop(ctx::Context, node::Node)
    @assert kind(node) in KSet"for while"
    kids = verified_kids(node)
    any_kid_changed = false
    for_idx = findfirst(x -> kind(x) in KSet"for while", kids)::Int
    if !has_tag(kids[for_idx], TAG_INDENT)
        kids[for_idx] = add_tag(kids[for_idx], TAG_INDENT)
        any_kid_changed = true
    end
    block_idx = findnext(x -> kind(x) === K"block", kids, for_idx + 1)::Int
    block_node′ = indent_block(ctx, kids[block_idx])
    if block_node′ !== nothing
        kids[block_idx] = block_node′
        any_kid_changed = true
    end
    end_idx = findlast(x -> kind(x) === K"end", kids)::Int
    if !has_tag(kids[end_idx], TAG_DEDENT)
        kids[end_idx] = add_tag(kids[end_idx], TAG_DEDENT)
        any_kid_changed = true
    end
    return any_kid_changed ? node : nothing
end

function indent_tuple(ctx::Context, node::Node)
    @assert kind(node) === K"tuple"
    kids = verified_kids(node)
    # Check whether this is an explicit tuple, e.g. `(a, b)`, or an implicit tuple,
    # e.g. `a, b`. Implicit tuples only show up in do-blocks(?).
    opening_paren_idx = findfirst(x -> kind(x) === K"(", kids)
    if opening_paren_idx === nothing
        # Implicit tuple: don't indent the closing token
        first_item_idx = findfirst(!JuliaSyntax.is_whitespace, kids)
        if first_item_idx === nothing
            # Empty implicit tuple can happen in e.g. a do-block without arguments
            return nothing
        end
        last_item_idx = findlast(!JuliaSyntax.is_whitespace, kids)::Int
        # TODO: Closing token indent?
        return indent_listlike(ctx, node, first_item_idx, last_item_idx)
    else
        # Explicit tuple: indent the closing token
        closing_paren_idx = findnext(x -> kind(x) === K")", kids, opening_paren_idx + 1)::Int
        @assert opening_paren_idx == firstindex(kids)
        @assert closing_paren_idx == lastindex(kids)
        return indent_listlike(ctx, node, opening_paren_idx, closing_paren_idx)
    end
end

function indent_parens(ctx::Context, node::Node)
    @assert kind(node) in KSet"parens"
    return indent_paren(ctx, node)
end

# TODO: This is not needed? NamedTuples?
function indent_parameters(ctx::Context, node::Node)
    # kids = verified_kids(node)
    # # TODO: This is always here?
    # semicolon_idx = findfirst(x -> kind(x) === K";", kids)::Int
    # last_non_ws_idx = findlast(!JuliaSyntax.is_whitespace, kids)::Int
    # return indent_newlines_between_indices(
    #     ctx, node, semicolon_idx, last_non_ws_idx; indent_closing_token = true,
    # )
end

function indent_struct(ctx::Context, node::Node)
    @assert kind(node) === K"struct"
    kids = verified_kids(node)
    any_kid_changed = false
    struct_idx = findfirst(!JuliaSyntax.is_whitespace, kids)::Int
    @assert kind(kids[struct_idx]) in KSet"mutable struct"
    if !has_tag(kids[struct_idx], TAG_INDENT)
        kids[struct_idx] = add_tag(kids[struct_idx], TAG_INDENT)
        any_kid_changed = true
    end
    block_idx = findnext(x -> kind(x) === K"block", kids, struct_idx + 1)::Int
    block_node′ = indent_block(ctx, kids[block_idx])
    if block_node′ !== nothing
        kids[block_idx] = block_node′
        any_kid_changed = true
    end
    end_idx = findlast(x -> kind(x) === K"end", kids)::Int
    if !has_tag(kids[end_idx], TAG_DEDENT)
        kids[end_idx] = add_tag(kids[end_idx], TAG_DEDENT)
        any_kid_changed = true
    end
    return any_kid_changed ? node : nothing
end

function indent_short_circuit(ctx::Context, node::Node)
    return indent_op_call(ctx, node)
end

# TODO: This function can be used for more things than just indent_using I think. Perhaps
# with a max_depth parameter.
function continue_all_newlines(
        ctx::Context, node::Node; skip_last::Bool = true, is_last::Bool = is_leaf(node),
    )
    if is_leaf(node)
        if kind(node) === K"NewlineWs" && !has_tag(node, TAG_LINE_CONT) &&
                !(skip_last && is_last)
            return add_tag(node, TAG_LINE_CONT)
        else
            return nothing
        end
    else
        any_kid_changed = false
        kids = verified_kids(node)
        for (i, kid) in pairs(kids)
            kid′ = continue_all_newlines(
                ctx, kid; skip_last = skip_last, is_last = i == lastindex(kids),
            )
            if kid′ !== nothing
                kids[i] = kid′
                any_kid_changed = true
            end
        end
        return any_kid_changed ? node : nothing
    end
end

function indent_using_import_export(ctx::Context, node::Node)
    @assert kind(node) in KSet"using import export"
    return continue_all_newlines(ctx, node)
end

function indent_ternary(ctx::Context, node::Node)
    @assert kind(node) === K"?"
    return continue_all_newlines(ctx, node)
end

function indent_assignment(ctx::Context, node::Node)
    kids = verified_kids(node)
    # Also catches for loop specifications (but at this point we have normalized `=` and `∈`
    # to `in`).
    op_idx = findfirst(x -> is_assignment(x) || kind(x) === K"in", kids)::Int
    last_non_ws_idx = findlast(!JuliaSyntax.is_whitespace, kids)::Int
    return indent_newlines_between_indices(
        ctx, node, op_idx, last_non_ws_idx; indent_closing_token = true,
    )
end

function indent_paren_block(ctx::Context, node::Node)
    @assert kind(node) === K"block"
    @assert JuliaSyntax.has_flags(node, JuliaSyntax.PARENS_FLAG)
    kids = verified_kids(node)
    opening_paren_idx = findfirst(x -> kind(x) === K"(", kids)::Int
    closing_paren_idx = findnext(x -> kind(x) === K")", kids, opening_paren_idx + 1)::Int
    return indent_listlike(ctx, node, opening_paren_idx, closing_paren_idx)
end

function indent_do(ctx::Context, node::Node)
    @assert kind(node) === K"do"
    kids = verified_kids(node)
    any_kid_changed = false
    # Skip over the call and go directly to the do-keyword
    do_idx = findfirst(x -> kind(x) === K"do", kids)::Int
    if !has_tag(kids[do_idx], TAG_INDENT)
        kids[do_idx] = add_tag(kids[do_idx], TAG_INDENT)
        any_kid_changed = true
    end
    # Find the do body block
    block_idx = findnext(x -> kind(x) === K"block", kids, do_idx + 1)::Int
    block_node′ = indent_block(ctx, kids[block_idx])
    if block_node′ !== nothing
        kids[block_idx] = block_node′
        any_kid_changed = true
    end
    # Closing `end`
    end_idx = findnext(x -> kind(x) === K"end", kids, block_idx + 1)::Int
    if !has_tag(kids[end_idx], TAG_DEDENT)
        kids[end_idx] = add_tag(kids[end_idx], TAG_DEDENT)
        any_kid_changed = true
    end
    return any_kid_changed ? node : nothing
end

function indent_quote(ctx::Context, node::Node)
    @assert kind(node) === K"quote"
    kids = verified_kids(node)
    any_kid_changed = false
    # K"quote" can be `quote ... end` or `:(...)`.
    block_form = !JuliaSyntax.has_flags(node, JuliaSyntax.COLON_QUOTE)
    if block_form
        block_idx = findfirst(x -> kind(x) === K"block", kids)
        if block_idx === nothing
            # `bar` in `foo.bar` is a quote block...
            return nothing
        end
        block_node′ = indent_begin(ctx, kids[block_idx], K"quote")
        if block_node′ !== nothing
            kids[block_idx] = block_node′
            any_kid_changed = true
        end
        return any_kid_changed ? node : nothing
    else
        # The short form can be ignored since the inside (K"block", K"tuple", or
        # K"Identifier") of the quote will be handled by other passes.
        return nothing
    end
end

# Literal array nodes and also ref-nodes (which can be either a typed-array or a getindex)
function indent_array(ctx::Context, node::Node)
    @assert kind(node) in KSet"vect vcat typed_vcat ncat ref"
    kids = verified_kids(node)
    opening_bracket_idx = findfirst(x -> kind(x) === K"[", kids)::Int
    closing_bracket_idx = findnext(x -> kind(x) === K"]", kids, opening_bracket_idx + 1)::Int
    return indent_listlike(ctx, node, opening_bracket_idx, closing_bracket_idx)
end

# TODO: can a row be multiline?
function indent_array_row(ctx::Context, node::Node)
    # @assert kind(node) === K"row"
    # return continue_all_newlines(ctx, node)
end

function indent_comparison(ctx::Context, node::Node)
    @assert kind(node) === K"comparison"
    return continue_all_newlines(ctx, node)
end

# Indent a nested module
function indent_module(ctx::Context, node::Node)
    kids = verified_kids(node)
    any_kid_changed = false
    # First node is the module keyword
    mod_idx = 1
    mod_node = kids[mod_idx]
    @assert is_leaf(mod_node) && kind(mod_node) in KSet"module baremodule"
    if !has_tag(mod_node, TAG_INDENT)
        kids[mod_idx] = add_tag(mod_node, TAG_INDENT)
        any_kid_changed = true
    end
    # Second node is the space between keyword and name
    # TODO: Make sure there is just a single space
    space_idx = 2
    space_node = kids[space_idx]
    @assert is_leaf(space_node) && kind(space_node) === K"Whitespace"
    # Third node is the module identifier
    id_idx = 3
    id_node = kids[id_idx]
    @assert kind(id_node) === K"Identifier"
    # Fourth node is the module body block.
    block_idx = 4
    block_node′ = indent_block(ctx, kids[block_idx])
    if block_node′ !== nothing
        kids[block_idx] = block_node′
        any_kid_changed = true
    end
    # Fifth node is the closing end keyword
    end_idx = 5
    end_node = kids[end_idx]
    @assert is_leaf(end_node) && kind(end_node) === K"end"
    if !has_tag(end_node, TAG_DEDENT)
        kids[end_idx] = add_tag(end_node, TAG_DEDENT)
        any_kid_changed = true
    end
    @assert verified_kids(node) === kids
    return any_kid_changed ? node : nothing
end

# The only thing at top level that we need to indent are modules which don't occupy the full
# top level expression, for example a file with an inner module followed by some code.
function indent_toplevel(ctx::Context, node::Node)
    @assert kind(node) === K"toplevel"
    kids = verified_kids(node)
    mod_idx = findfirst(x -> kind(x) === K"module", kids)
    if mod_idx === nothing || count(!JuliaSyntax.is_whitespace, kids) == 1
        # No module or module that is the only top level expression
        return nothing
    end
    any_kid_changed = false
    while mod_idx !== nothing
        mod_node = kids[mod_idx]
        mod_node′ = indent_module(ctx, mod_node)
        if mod_node′ !== nothing
            kids[mod_idx] = mod_node′
            any_kid_changed = true
        end
        mod_idx = findnext(x -> kind(x) === K"module", kids, mod_idx + 1)
    end
    return any_kid_changed ? node : nothing
end

function insert_delete_mark_newlines(ctx::Context, node::Node)
    if is_leaf(node)
        return nothing
    elseif kind(node) in KSet"function macro"
        return indent_function_or_macro(ctx, node)
    elseif kind(node) === K"if"
        return indent_if(ctx, node)
    elseif kind(node) === K"let"
        return indent_let(ctx, node)
    elseif is_begin_block(node)
        return indent_begin(ctx, node)
    elseif kind(node) in KSet"call dotcall" &&
            flags(node) == 0 # Flag check rules out op-calls
        return indent_call(ctx, node)
    elseif is_infix_op_call(node)
        return indent_op_call(ctx, node)
    elseif kind(node) in KSet"for while"
        return indent_loop(ctx, node)
    elseif kind(node) === K"tuple"
        return indent_tuple(ctx, node)
    elseif kind(node) === K"struct"
        return indent_struct(ctx, node)
    elseif kind(node) === K"parens"
        return indent_parens(ctx, node)
    elseif kind(node) in KSet"curly braces bracescat"
        return indent_braces(ctx, node)
    elseif kind(node) in KSet"|| &&"
        return indent_short_circuit(ctx, node)
    elseif kind(node) in KSet"using import export"
        return indent_using_import_export(ctx, node)
    elseif is_assignment(node)
        return indent_assignment(ctx, node)
    elseif kind(node) === K"parameters"
        return indent_parameters(ctx, node)
    elseif kind(node) === K"?"
        return indent_ternary(ctx, node)
    elseif kind(node) === K"try"
        return indent_try(ctx, node)
    elseif kind(node) === K"quote"
        return indent_quote(ctx, node)
    elseif kind(node) === K"do"
        return indent_do(ctx, node)
    elseif is_paren_block(node)
        return indent_paren_block(ctx, node)
    elseif kind(node) in KSet"vect vcat typed_vcat ncat ref"
        return indent_array(ctx, node)
    elseif kind(node) in KSet"row"
        return indent_array_row(ctx, node)
    elseif kind(node) === K"comparison"
        return indent_comparison(ctx, node)
    elseif kind(node) === K"toplevel"
        return indent_toplevel(ctx, node)
    elseif kind(node) === K"module" &&
            findlast(x -> x === K"module", ctx.lineage_kinds) !== nothing
        return indent_module(ctx, node)
    end
    return nothing
end

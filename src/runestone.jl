# SPDX-License-Identifier: MIT

# This is the runestone where all the formatting transformations are implemented.

function trim_trailing_whitespace(ctx::Context, node::Node)
    kind(node) in KSet"NewlineWs Comment" || return nothing
    @assert is_leaf(node)
    str = String(read_bytes(ctx, node))
    local str′::String
    if kind(node) === K"NewlineWs"
        # Strip all whitespace up until the newline while normalizing line endings to \njK:w
        str′ = replace(str, r"\h*(\r\n|\r|\n)" => '\n')
        # If the next sibling is also a NewlineWs we can trim trailing
        # whitespace from this node too
        next_kind = next_sibling_kind(ctx)
        if next_kind === K"NewlineWs"
            # str′ = replace(str′, r"(\r\n|\r|\n)\h*" => '\n')
            str′ = replace(str′, r"\n\h*" => '\n')
        end
    else
        @assert kind(node) === K"Comment"
        # Strip trailing spaces and tabs from comments
        str′ = rstrip(str, (' ', '\t'))
    end
    if str == str′
        return nothing
    end
    # Write new bytes and reset the stream
    nb = replace_bytes!(ctx, str′, span(node))
    @assert nb != span(node)
    # Create new node and return it
    return make_node(node, nb)
end

function replace_tabs_with_four_spaces(ctx::Context, node::Node)
    kind(node) in KSet"Whitespace NewlineWs" || return nothing
    @assert is_leaf(node)
    bytes = read_bytes(ctx, node)
    tabidx = findfirst(x -> x == UInt8('\t'), bytes)
    tabidx === nothing && return nothing
    while tabidx !== nothing
        bytes[tabidx] = UInt8(' ')
        for _ in 1:3
            insert!(bytes, tabidx, UInt8(' '))
        end
        tabidx = findnext(x -> x == UInt8('\t'), bytes, tabidx + 4)
    end
    nb = replace_bytes!(ctx, bytes, span(node))
    return make_node(node, nb)
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
    (?:[ef][+-]?(?:[1-9]\d*|0))?
    $
    """x
    if occursin(r, str)
        return nothing
    end
    if occursin('_', str) || occursin("0x", str)
        # TODO: Hex floats and floats with underscores are ignored
        return nothing
    end
    # Split up the pieces
    r = r"^(?<sgn>[+-])?(?<int>\d*)(?:\.?(?<frac>\d*))?(?:(?<epm>[eEf][+-]?)(?<exp>\d+))?$"
    m = match(r, str)::RegexMatch
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
                if kind(kid) === K"Comment"
                    # Keep the state
                elseif looking_for_x
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
            if kind(kid) === K"Comment"
                # Just skip through and keep the state?
            elseif looking_for_x
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
            kind(node) in KSet"tuple parameters curly braces bracescat vect ref parens" ||
                (kind(node) === K"call" && flags(node) == 0) || # Flag check rules out op-calls
                (kind(node) === K"dotcall" && flags(node) == 0) ||
                (kind(node) === K"macrocall" && JuliaSyntax.has_flags(node, JuliaSyntax.PARENS_FLAG)) ||
                is_paren_block(node)
        )
        return nothing
    end
    if kind(node) === K"parameters"
        # Note that some of these are not valid Julia syntax but still parse
        @assert ctx.lineage_kinds[end] in KSet"tuple call dotcall macrocall curly vect ref braces"
    end

    @assert !is_leaf(node)
    kids = verified_kids(node)
    kids′ = kids

    peek(i) = i < length(kids) ? kind(kids[i + 1]) : nothing

    ws = Node(JuliaSyntax.SyntaxHead(K"Whitespace", JuliaSyntax.TRIVIA_FLAG), 1)
    comma = Node(JuliaSyntax.SyntaxHead(K",", JuliaSyntax.TRIVIA_FLAG), 1)

    # Find the opening and closing leafs
    implicit_tuple = false
    if kind(node) in KSet"tuple call dotcall parens macrocall" || is_paren_block(node)
        opening_leaf_idx = findfirst(x -> kind(x) === K"(", kids)
        if opening_leaf_idx === nothing
            # Implicit tuple without (), for example arguments in a do-block
            implicit_tuple = true
            opening_leaf_idx = findfirst(!JuliaSyntax.is_whitespace, kids)
            if opening_leaf_idx === nothing
                # All whitespace... return?
                return nothing
            else
                closing_leaf_idx = findlast(!JuliaSyntax.is_whitespace, kids)::Int
                opening_leaf_idx == closing_leaf_idx && return nothing # empty
                opening_leaf_idx -= 1
                closing_leaf_idx += 1
            end
            @assert findnext(x -> kind(x) === K")", kids, opening_leaf_idx + 1) === nothing
        else
            closing_leaf_idx = findnext(x -> kind(x) === K")", kids, opening_leaf_idx + 1)::Int
            closing_leaf_idx == opening_leaf_idx + 1 && return nothing # empty
        end
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
        x -> !(JuliaSyntax.is_whitespace(x) || kind(x) in KSet", ;"),
        @view(kids[(opening_leaf_idx + 1):(closing_leaf_idx - 1)])
    )
    first_item_idx = findnext(x -> !(JuliaSyntax.is_whitespace(x) || kind(x) in KSet", ;"), kids, opening_leaf_idx + 1)
    if first_item_idx !== nothing && first_item_idx >= closing_leaf_idx
        first_item_idx = nothing
    end
    last_item_idx = findprev(x -> !(JuliaSyntax.is_whitespace(x) || kind(x) in KSet", ;"), kids, closing_leaf_idx - 1)
    if last_item_idx !== nothing && last_item_idx <= opening_leaf_idx
        last_item_idx = nothing
    end

    # Multiline lists require leading and trailing newline
    # multiline = contains_outer_newline(kids, opening_leaf_idx, closing_leaf_idx)
    # multiline = any(y -> any_leaf(x -> kind(x) === K"NewlineWs", kids[y]), (opening_leaf_idx + 1):(closing_leaf_idx - 1))
    multiline = is_multiline_between_idxs(ctx, node, opening_leaf_idx, closing_leaf_idx)

    is_named_tuple = kind(node) === K"tuple" && n_items == 1 && kind(kids[first_item_idx]) === K"parameters"

    # A trailing comma is required if
    #  - node is a single item tuple which is not from an anonymous fn (Julia-requirement)
    #  - the closing token is not on the same line as the last item (Runic-requirement)
    require_trailing_comma = false
    allow_trailing_semi = false
    allow_trailing_comma = multiline
    if kind(node) in KSet"call dotcall macrocall"
        require_trailing_comma = false
    elseif implicit_tuple
        require_trailing_comma = false
    elseif kind(node) === K"tuple" && n_items == 1 && ctx.lineage_kinds[end] !== K"function" &&
            kind(kids[first_item_idx::Int]) !== K"parameters"
        # TODO: May also have to check for K"where" and K"::" in the lineage above
        require_trailing_comma = true
    elseif kind(node) in KSet"bracescat parens"
        require_trailing_comma = false # Leads to parser error
    elseif kind(node) in KSet"block"
        require_trailing_comma = false
        allow_trailing_semi = n_items < 2
    elseif kind(node) === K"parameters"
        # For parameters the trailing comma is configured from the parent
        require_trailing_comma = has_tag(node, TAG_TRAILING_COMMA)
        allow_trailing_comma = has_tag(node, TAG_TRAILING_COMMA_OPT)
    elseif n_items > 0 && kind(kids[last_item_idx]) === K"macrocall" &&
            !JuliaSyntax.has_flags(kids[last_item_idx], JuliaSyntax.PARENS_FLAG) &&
            !is_string_macro(kids[last_item_idx])
        require_trailing_comma = false
    elseif multiline
        require_trailing_comma = true
    elseif n_items > 0
        require_trailing_comma = any(
            x -> kind(x) === K"NewlineWs", @view(kids[(last_item_idx + 1):(closing_leaf_idx - 1)])
        ) || has_newline_after_non_whitespace(kids[last_item_idx])
    end

    # Helper to compute the new state after a given item
    function state_after_item(i, last_item_idx, require_trailing_comma)
        @assert i <= last_item_idx
        if i < last_item_idx
            return :expect_comma
        elseif i == last_item_idx && require_trailing_comma
            if kind(kids[last_item_idx]) === K"parameters"
                # If the last kid is K"parameters" it will handle a trailing comma
                return :expect_closing
            else
                return :expect_comma
            end
        else
            return :expect_closing
        end
    end

    # Keep track of the state
    state = if kind(node) === K"parameters" && n_items > 0
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
        first_item_in_implicit_tuple = implicit_tuple && i == opening_leaf_idx + 1
        if state === :expect_item
            if first_item_in_implicit_tuple && kind(kid′) === K"Whitespace" && peek(i) !== K"Comment"
                # Not allowed to touch this one I think?
                accept_node!(ctx, kid′)
                any_kid_changed && push!(kids′, kid′)
            elseif kind(kid′) === K"Whitespace" && peek(i) !== K"Comment"
                @assert !first_item_in_implicit_tuple # Unreachable?
                # Delete whitespace unless followed by a comment
                replace_bytes!(ctx, "", span(kid′))
                this_kid_changed = true
                if kids′ === kids
                    kids′ = kids[1:(i - 1)]
                end
            elseif kind(kid′) === K"NewlineWs" ||
                    (kind(kid′) === K"Whitespace" && peek(i) === K"Comment")
                @assert !first_item_in_implicit_tuple # Unreachable?
                # Newline here can happen if this kid is just after the opening leaf or if
                # there is an empty line between items. No state change.
                accept_node!(ctx, kid′)
                any_kid_changed && push!(kids′, kid′)
            elseif kind(kid′) === K"Comment"
                @assert !first_item_in_implicit_tuple # Unreachable?
                accept_node!(ctx, kid′)
                any_kid_changed && push!(kids′, kid′)
                state = :expect_space # To ensure space after the comment
            else
                # This is an item (probably?).
                # Make sure it doesn't have leading or trailing whitespace.
                if kind(first_leaf(kid′)) === K"Whitespace" && kind(second_leaf(kid′)) !== K"Comment" && !first_item_in_implicit_tuple
                    # Delete the whitespace leaf
                    kid_ws = first_leaf(kid′)
                    replace_bytes!(ctx, "", span(kid_ws))
                    kid′ = replace_first_leaf(kid′, nullnode)
                    this_kid_changed = true
                end
                if kind(last_leaf(kid′)) === K"Whitespace"
                    # Delete the whitespace leaf
                    kid_ws = last_leaf(kid′)
                    let pos = position(ctx.fmt_io)
                        seek(ctx.fmt_io, pos + span(kid′) - span(kid_ws))
                        replace_bytes!(ctx, "", span(kid_ws))
                        seek(ctx.fmt_io, pos)
                    end
                    kid′ = replace_last_leaf(kid′, nullnode)
                    this_kid_changed = true
                end
                if kind(kid′) === K"parameters" && require_trailing_comma &&
                        i == last_item_idx && !has_tag(kid′, TAG_TRAILING_COMMA)
                    # Tag the node to require a trailing comma
                    kid′ = add_tag(kid′, TAG_TRAILING_COMMA)
                    this_kid_changed = true
                end
                if kind(kid′) === K"parameters" && allow_trailing_comma &&
                        i == last_item_idx && !has_tag(kid′, TAG_TRAILING_COMMA_OPT)
                    # Tag the node to optionally have a trailing comma
                    kid′ = add_tag(kid′, TAG_TRAILING_COMMA_OPT)
                    this_kid_changed = true
                end
                if kind(kid′) === K"parameters" && !require_trailing_comma && !is_named_tuple &&
                        count(
                        x -> !(JuliaSyntax.is_whitespace(x) || kind(x) in KSet", ;"),
                        verified_kids(kid′)
                    ) == 0
                    # If kid is K"parameters" without items and we don't want a trailing
                    # comma/semicolon we need to eat any whitespace kids (e.g. comments)
                    grandkids = verified_kids(kid′)
                    semi_idx = 1
                    @assert kind(grandkids[semi_idx]) === K";"
                    ws_idx = something(findnext(x -> kind(x) !== K"Whitespace", grandkids, semi_idx + 1), lastindex(grandkids) + 1)
                    any_kid_changed = true
                    if kids′ === kids
                        kids′ = kids[1:(i - 1)]
                    end
                    replace_bytes!(ctx, "", mapreduce(span, +, grandkids[1:(ws_idx - 1)]; init = 0))
                    for j in ws_idx:length(grandkids)
                        grandkid = grandkids[j]
                        accept_node!(ctx, grandkid)
                        push!(kids′, grandkid)
                    end
                else
                    # Kid is now acceptable
                    any_kid_changed |= this_kid_changed
                    if any_kid_changed
                        if kids′ === kids
                            kids′ = kids[1:(i - 1)]
                        end
                        push!(kids′, kid′)
                    end
                    accept_node!(ctx, kid′)
                end
                # Transition to the next state
                state = state_after_item(i, last_item_idx, require_trailing_comma)
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
                    !JuliaSyntax.is_whitespace, @view(kids[1:(closing_leaf_idx - 1)]), i + 1
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
                # TODO: Why is this needed?
                # if kind(kid′) === K"NewlineWs"
                #     state = :expect_closing
                # end
                any_kid_changed |= this_kid_changed
                # Accept the newline
                accept_node!(ctx, kid′)
                any_kid_changed && push!(kids′, kid′)
            elseif kind(kid′) === K"parameters"
                # Note that some of these are not valid Julia syntax still parse
                @assert kind(node) in KSet"call dotcall macrocall curly tuple vect ref braces"
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
                if allow_trailing_comma && !has_tag(kid′, TAG_TRAILING_COMMA_OPT)
                    # Tag the parameters node to optionally allow a trailing comma
                    kid′ = add_tag(kid′, TAG_TRAILING_COMMA_OPT)
                    this_kid_changed = true
                end
                if !require_trailing_comma &&
                        count(
                        x -> !(JuliaSyntax.is_whitespace(x) || kind(x) in KSet", ;"),
                        verified_kids(kid′)
                    ) == 0
                    # If kid is K"parameters" without items and we don't want a trailing
                    # comma/semicolon we need to eat any whitespace kids (e.g. comments)
                    grandkids = verified_kids(kid′)
                    semi_idx = 1
                    @assert kind(grandkids[semi_idx]) === K";"
                    ws_idx = findnext(x -> kind(x) !== K"Whitespace", grandkids, semi_idx + 1)
                    if ws_idx !== nothing
                        any_kid_changed = true
                        if kids′ === kids
                            kids′ = kids[1:(i - 1)]
                        end
                        replace_bytes!(ctx, "", mapreduce(span, +, grandkids[1:(ws_idx - 1)]; init = 0))
                        for j in ws_idx:length(grandkids)
                            grandkid = grandkids[j]
                            accept_node!(ctx, grandkid)
                            push!(kids′, grandkid)
                        end
                    else
                        # Nothing in the parameter node needed, overwrite it fully
                        any_kid_changed = true
                        replace_bytes!(ctx, "", span(kid′))
                        if any_kid_changed
                            if kids′ === kids
                                kids′ = kids[1:(i - 1)]
                            end
                        end
                    end
                else
                    # TODO: Tag for requiring trailing comma.
                    any_kid_changed |= this_kid_changed
                    accept_node!(ctx, kid′)
                    if any_kid_changed
                        if kids′ === kids
                            kids′ = kids[1:(i - 1)]
                        end
                        push!(kids′, kid′)
                    end
                end
                # K"parameter" is always the last item in valid Julia code but we need to
                # handle all expression that parses and there might be multiple
                # K"parameters"...
                state = i == last_item_idx ? (:expect_closing) : (:expect_item)
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
                    state = state_after_item(i, last_item_idx, require_trailing_comma)
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
                    state = state_after_item(i, last_item_idx, require_trailing_comma)
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
                    state = state_after_item(i, last_item_idx, require_trailing_comma)
                end
            end
        else
            @assert state === :expect_closing
            if (kind(kid′) === K"," && !allow_trailing_comma) ||
                    (kind(kid′) === K";" && !allow_trailing_semi) ||
                    (kind(kid′) === K"Whitespace" && peek(i) !== K"Comment")
                # Trailing comma (when not wanted) and space not followed by a comment are
                # removed
                this_kid_changed = true
                if kids′ === kids
                    kids′ = kids[1:(i - 1)]
                end
                replace_bytes!(ctx, "", span(kid′))
            elseif kind(node) === K"block" && kind(kid′) === K";" && allow_trailing_semi ||
                    (kind(kid′) === K"," && allow_trailing_comma) ||
                    (kind(kid′) === K"Whitespace" && peek(i) !== K"Comment")
                allow_trailing_semi = n_items == 0 # Only one semicolon allowed
                allow_trailing_comma = false # Just one please
                accept_node!(ctx, kid′)
                any_kid_changed && push!(kids′, kid′)
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
            # K"parameters" should aleays handle the trailing comma and got to
            # :expect_closing directly
            @assert kind(kids[last_item_idx]) !== K"parameters"
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
    if !(is_assignment(node) && !is_leaf(node))
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

function spaces_in_export_public(ctx::Context, node::Node)
    is_leaf(node) && return nothing
    if !(kind(node) in KSet"export public" || is_global_local_list(node))
        return nothing
    end
    kids = verified_kids(node)
    kids′ = kids
    any_changes = false
    pos = position(ctx.fmt_io)

    spacenode = Node(JuliaSyntax.SyntaxHead(K"Whitespace", JuliaSyntax.TRIVIA_FLAG), 1)

    @assert is_leaf(kids[1]) && kind(kids[1]) in KSet"export public global local"
    accept_node!(ctx, kids[1])

    # space -> identifier -> comma
    state = :expect_space
    i = 2
    while i <= length(kids)
        kid = kids[i]
        if state === :expect_space
            if kind(kid) === K"NewlineWs" || (kind(kid) === K"Whitespace" && span(kid) == 1)
                any_changes && push!(kids′, kid)
                accept_node!(ctx, kid)
            elseif kind(kid) === K"Whitespace"
                kid′ = replace_first_leaf(kid, spacenode)
                replace_bytes!(ctx, " ", span(first_leaf(kid)))
                any_changes = true
                if kids′ === kids
                    kids′ = kids[1:(i - 1)]
                end
                accept_node!(ctx, kid′)
                push!(kids′, kid′)
            else
                @assert kind(first_leaf(kid)) !== K"Whitespace"
                # Insert a space
                any_changes = true
                if kids′ === kids
                    kids′ = kids[1:(i - 1)]
                end
                replace_bytes!(ctx, " ", 0)
                push!(kids′, spacenode)
                accept_node!(ctx, spacenode)
                state = :expect_identifier
                continue # Skip increment of i
            end
            state = :expect_identifier
        elseif state === :expect_identifier
            state = :expect_comma
            if kind(kid) in KSet"Identifier @ MacroName $ var" || JuliaSyntax.is_operator(kid)
                any_changes && push!(kids′, kid)
                accept_node!(ctx, kid)
                if kind(kid) === K"@"
                    state = :expect_identifier
                end
                if kind(kid) === K"$"
                    @assert findlast(x -> x in KSet"quote macrocall", ctx.lineage_kinds) !== nothing
                end
            elseif kind(kid) === K"parens"
                # Parenthesized symbol gives a warning in JuliaSyntax but is allowed
                # TODO: Runic could remove them...
                @assert kind(first_leaf(kid)) !== K"Whitespace"
                any_changes && push!(kids′, kid)
                accept_node!(ctx, kid)
            elseif kind(kid) in KSet"Comment NewlineWs"
                any_changes && push!(kids′, kid)
                accept_node!(ctx, kid)
                state = :expect_space
            else
                @assert false
            end
        else
            @assert state === :expect_comma
            state = :expect_space
            if kind(kid) === K","
                any_changes && push!(kids′, kid)
                accept_node!(ctx, kid)
            elseif kind(kid) === K"Whitespace"
                # Drop this node
                any_changes = true
                replace_bytes!(ctx, "", span(kid))
                if kids′ === kids
                    kids′ = kids[1:(i - 1)]
                end
                state = :expect_comma
            else
                @assert false
            end
        end
        i += 1
    end
    # Reset stream
    seek(ctx.fmt_io, pos)
    return any_changes ? make_node(node, kids′) : nothing
end

# Used in spaces_in_import_using. Well formatted importpath should have a single leading
# space or a newline.
function format_importpath(ctx::Context, node::Node)
    @assert kind(node) === K"importpath"
    pos = position(ctx.fmt_io)
    spacebar = Node(JuliaSyntax.SyntaxHead(K"Whitespace", JuliaSyntax.TRIVIA_FLAG), 1)
    if kind(first_leaf(node)) === K"NewlineWs" ||
            (kind(first_leaf(node)) === K"Whitespace" && span(first_leaf(node)) == 1)
        # Newline or whitespace with correct span
        node′ = nothing
    elseif kind(first_leaf(node)) === K"Whitespace"
        # Whitespace with incorrect span; replace with a single space
        replace_bytes!(ctx, " ", span(first_leaf(node)))
        node′ = replace_first_leaf(node, spacebar)
    else
        # No whitespace, insert
        @assert kind(first_leaf(node)) in KSet"Identifier @ Comment" ||
            JuliaSyntax.is_operator(first_leaf(node))
        kids′ = copy(verified_kids(node))
        pushfirst!(kids′, spacebar)
        replace_bytes!(ctx, " ", 0)
        node′ = make_node(node, kids′)
    end
    # Reset stream
    seek(ctx.fmt_io, pos)
    return node′
end

# Used in spaces_in_import_using.
function format_as(ctx::Context, node::Node)
    @assert kind(node) === K"as"
    kids = verified_kids(node)
    kids′ = kids
    any_changes = false
    pos = position(ctx.fmt_io)
    spacebar = Node(JuliaSyntax.SyntaxHead(K"Whitespace", JuliaSyntax.TRIVIA_FLAG), 1)
    # First the importpath (LHS of the `as`)
    idx = 1
    kid′ = kids[idx]
    @assert kind(kid′) === K"importpath"
    kid′′ = format_importpath(ctx, kid′)
    if kid′′ !== nothing
        any_changes = true
        kid′ = kid′′
        kids′ = [kid′]
    end
    accept_node!(ctx, kid′)
    # space before `as`
    idx += 1
    kid = kids[idx]
    @assert kind(kid) === K"Whitespace"
    if span(kid) == 1
        # Correct span
        accept_node!(ctx, kid)
        any_changes && push!(kids′, kid)
    else
        # Incorrect span
        replace_bytes!(ctx, " ", span(kid))
        any_changes = true
        if kids′ === kids
            kids′ = kids[1:(idx - 1)]
        end
        accept_node!(ctx, spacebar)
        push!(kids′, spacebar)
    end
    # `as`
    idx += 1
    kid = kids[idx]
    @assert kind(kid) === K"as"
    accept_node!(ctx, kid)
    any_changes && push!(kids′, kid)
    # space after `as`
    idx += 1
    kid = kids[idx]
    @assert kind(kid) === K"Whitespace"
    if span(kid) == 1
        # Correct span
        accept_node!(ctx, kid)
        any_changes && push!(kids′, kid)
    else
        # Incorrect span
        replace_bytes!(ctx, " ", span(kid))
        any_changes = true
        if kids′ === kids
            kids′ = kids[1:(idx - 1)]
        end
        accept_node!(ctx, spacebar)
        push!(kids′, spacebar)
    end
    # Alias-identifier
    idx += 1
    kid = kids[idx]
    @assert kind(kid) in KSet"Identifier $ @"
    if !is_leaf(kid)
        @assert kind(first_leaf(kid)) !== K"Whitespace"
    end
    if kind(kid) === K"$"
        @assert findlast(x -> x in KSet"quote macrocall", ctx.lineage_kinds) !== nothing
    end
    accept_node!(ctx, kid)
    any_changes && push!(kids′, kid)
    if kind(kid) === K"@"
        idx += 1
        kid = kids[idx]
        @assert kind(kid) === K"MacroName"
        accept_node!(ctx, kid)
        any_changes && push!(kids′, kid)
    end
    # Reset stream
    seek(ctx.fmt_io, pos)
    return any_changes ? make_node(node, kids′) : nothing
end

function spaces_in_import_using(ctx::Context, node::Node)
    if !(kind(node) in KSet"import using" && !is_leaf(node))
        return nothing
    end
    kids = verified_kids(node)
    kids′ = kids
    any_changes = false
    pos = position(ctx.fmt_io)

    colon_list = kind(first(kids)) === K":"
    if colon_list
        colon_node = first(kids)
        @assert length(kids) == 1
        kids = verified_kids(colon_node)
        kids′ = kids
    end

    @assert kind(kids[1]) in KSet"import using"
    accept_node!(ctx, kids[1])

    state = :expect_item
    i = 2
    while i <= length(kids)
        kid = kids[i]
        if state === :expect_item
            @assert kind(kid) in KSet"importpath as"
            if kind(kid) === K"importpath"
                kid′ = format_importpath(ctx, kid)
            else
                @assert kind(kid) === K"as"
                kid′ = format_as(ctx, kid)
            end
            if kid′ !== nothing
                any_changes = true
                if kids′ === kids
                    kids′ = kids[1:(i - 1)]
                end
                accept_node!(ctx, kid′)
                push!(kids′, kid′)
            else
                accept_node!(ctx, kid)
                any_changes && push!(kids′, kid)
            end
            state = :expect_comma
        else
            @assert state === :expect_comma
            if kind(kid) === K"Whitespace"
                # Drop this node
                any_changes = true
                replace_bytes!(ctx, "", span(kid))
                if kids′ === kids
                    kids′ = kids[1:(i - 1)]
                end
            else
                @assert kind(kid) in KSet": ,"
                accept_node!(ctx, kid)
                any_changes && push!(kids′, kid)
                state = :expect_item
            end
        end
        i += 1
    end
    # Reset stream
    seek(ctx.fmt_io, pos)
    if any_changes
        # Create new node and return it
        if colon_list
            colon_node′ = make_node(colon_node, kids′)
            return make_node(node, [colon_node′])
        else
            return make_node(node, kids′)
        end
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
# TODO: local, const
function spaces_around_keywords(ctx::Context, node::Node)
    is_leaf(node) && return nothing
    keyword_set = KSet"where do mutable struct abstract primitive type function if elseif catch while"
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
                    if !any(x -> !(JuliaSyntax.is_whitespace(x) || kind(x) === K";"), verified_kids(nkid))
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
                # Reachable in e.g. `T where{T}`, `if(`, ... insert space
                @assert kind(node) in KSet"where if elseif while do function"
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
    pos = position(ctx.fmt_io)
    vars_index = findfirst(!JuliaSyntax.is_whitespace, kids)::Int
    # TODO: Need to insert whitespaces around `in` when replacing e.g. `i=I` with `iinI`.
    # However, at the moment it looks like the whitespace around operator pass does it's
    # thing first? I don't really know how though, because the for loop pass should be
    # happening before...
    in_index = findnext(!JuliaSyntax.is_whitespace, kids, vars_index + 1)
    in_node = kids[in_index]
    if kind(in_node) === K"in"
        @assert JuliaSyntax.is_trivia(in_node)
        @assert is_leaf(in_node)
        @assert position(ctx.fmt_io) == pos
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
        JuliaSyntax.SyntaxHead(K"in", JuliaSyntax.TRIVIA_FLAG), nb
    )
    accept_node!(ctx, in_node′)
    kids′ = copy(kids)
    kids′[in_index] = in_node′
    seek(ctx.fmt_io, pos)
    return make_node(node, kids′)
end

function replace_with_in_filter(ctx::Context, node::Node)
    @assert kind(node) === K"filter" && !is_leaf(node)
    pos = position(ctx.fmt_io)
    kids = verified_kids(node)
    idx = findfirst(x -> kind(x) in KSet"= cartesian_iterator" && !is_leaf(x), kids)::Int
    for i in 1:(idx - 1)
        accept_node!(ctx, kids[i])
    end
    kid = kids[idx]
    if kind(kid) === K"="
        kid′ = replace_with_in(ctx, kid)
    else
        kid′ = replace_with_in_cartesian(ctx, kid)
    end
    # Reset stream
    seek(ctx.fmt_io, pos)
    if kid′ === nothing
        return nothing
    else
        kids = copy(kids)
        kids[idx] = kid′
        return make_node(node, kids)
    end
end

function replace_with_in_cartesian(ctx::Context, node::Node)
    @assert kind(node) === K"cartesian_iterator" && !is_leaf(node)
    kids = verified_kids(node)
    kids′ = kids
    pos = position(ctx.fmt_io)
    for (i, kid) in pairs(kids)
        if kind(kid) === K"="
            kid′ = replace_with_in(ctx, kid)
            if kid′ !== nothing
                if kids′ === kids
                    kids′ = copy(kids)
                end
                kids′[i] = kid′
                accept_node!(ctx, kid′)
            else
                kids′[i] = kid
                accept_node!(ctx, kid)
            end
        else
            kids′[i] = kid
            accept_node!(ctx, kid)
        end
    end
    # Reset stream
    seek(ctx.fmt_io, pos)
    if kids === kids′
        return nothing
    end
    return make_node(node, kids′)
end

# replace `=` and `∈` with `in` in for-loops
function for_loop_use_in(ctx::Context, node::Node)
    if !(
            (kind(node) === K"for" && !is_leaf(node) && meta_nargs(node) == 4) ||
                kind(node) === K"generator"
        )
        return nothing
    end
    pos = position(ctx.fmt_io)
    kids = verified_kids(node)
    kids′ = kids
    for_index = findfirst(c -> kind(c) === K"for" && is_leaf(c), kids)::Int
    next_index = 1
    any_for_changed = false
    # generator can have multiple for nodes
    while for_index !== nothing
        for_node = kids[for_index]
        @assert kind(for_node) === K"for" && span(for_node) == 3 &&
            is_leaf(for_node) && JuliaSyntax.is_trivia(for_node)
        for i in next_index:for_index
            accept_node!(ctx, kids[i])
        end
        # The for loop specification node can be either K"=" or K"cartesian_iterator"
        for_spec_index = for_index + 1
        for_spec_node = kids[for_spec_index]
        @assert kind(for_spec_node) in KSet"= cartesian_iterator filter"
        if kind(for_spec_node) === K"="
            for_spec_node′ = replace_with_in(ctx, for_spec_node)
        elseif kind(for_spec_node) === K"filter"
            for_spec_node′ = replace_with_in_filter(ctx, for_spec_node)
        else
            @assert kind(for_spec_node) === K"cartesian_iterator"
            for_spec_node′ = replace_with_in_cartesian(ctx, for_spec_node)
        end
        if for_spec_node′ !== nothing
            any_for_changed = true
            # Insert the new for spec node
            if kids′ === kids
                kids′ = copy(kids)
            end
            kids′[for_spec_index] = for_spec_node′
            accept_node!(ctx, for_spec_node′)
        else
            accept_node!(ctx, for_spec_node)
        end
        for_index = findnext(c -> kind(c) === K"for" && is_leaf(c), kids, for_spec_index + 1)
        if for_index !== nothing
            @assert kind(node) === K"generator"
        end
        next_index = for_spec_index + 1
    end
    if !any_for_changed
        seek(ctx.fmt_io, pos)
        return nothing
    end
    # At this point the eq nodes are done, just accept any remaining nodes
    # TODO: Don't need to do this...
    for i in next_index:length(kids′)
        accept_node!(ctx, kids′[i])
    end
    # Construct the full node and return
    node′ = make_node(node, kids′)
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
        [opening_brace, rhs, closing_brace]
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

# No newline at the beginning and single newline at the end of the file
function no_leading_and_single_trailing_newline(ctx::Context, node::Node)
    if !(ctx.filemode && length(ctx.lineage_kinds) == 0)
        return nothing
    end
    @assert kind(node) === K"toplevel"
    @assert !is_leaf(node)
    @assert position(ctx.fmt_io) == 0
    changed = false
    # Remove leading newlines and whitespace
    while (l = first_leaf(node); l !== nothing && kind(l) in KSet"NewlineWs Whitespace" && length(verified_kids(node)) > 1)
        changed = true
        replace_bytes!(ctx, "", span(l))
        node = replace_first_leaf(node, nullnode)
    end
    accept_node!(ctx, node)
    # Remove trailing newlines
    l = last_leaf(node)
    if l === nothing || kind(l) !== K"NewlineWs"
        kids′ = copy(verified_kids(node))
        push!(kids′, Node(JuliaSyntax.SyntaxHead(K"NewlineWs", JuliaSyntax.TRIVIA_FLAG), 1))
        replace_bytes!(ctx, "\n", 0)
        changed = true
        node = make_node(node, kids′)
    else
        ll = second_last_leaf(node)
        while ll !== nothing && kind(l) === kind(ll) === K"NewlineWs"
            changed = true
            seek(ctx.fmt_io, position(ctx.fmt_io) - span(l))
            # replace_bytes!(ctx, "", span(l))
            node = replace_last_leaf(node, nullnode)
            @assert last_leaf(node) === ll
            l = ll
            ll = second_last_leaf(node)
        end
    end
    if changed
        return node
    else
        seek(ctx.fmt_io, 0)
        return nothing
    end
end

# Remove more than three newlines in a row
function max_three_consecutive_newlines(ctx::Context, node::Node)
    is_leaf(node) && return nothing
    kids = verified_kids(node)
    idx = findfirst(x -> kind(x) === K"NewlineWs", kids)
    while idx !== nothing
        if idx + 3 <= length(kids) &&
                (kind(kids[idx + 1]) == kind(kids[idx + 2]) == kind(kids[idx + 3]) == K"NewlineWs")
            kids′ = Vector{Node}(undef, length(kids) - 1)
            for (i, kid) in pairs(kids)
                if i == idx
                    replace_bytes!(ctx, "", span(kids[idx]))
                else
                    accept_node!(ctx, kid)
                    kids′[i < idx ? i : (i - 1)] = kid
                end
            end
            return make_node(node, kids′)
        end
        idx = findnext(x -> kind(x) === K"NewlineWs", kids, idx + 1)
    end
    return nothing
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
    if !(is_longform_anon_function(node) || is_longform_functor(node))
        space_idx = 2
        space_node = kids[space_idx]
        @assert is_leaf(space_node) && kind(space_node) === K"Whitespace"
    end
    # Third node is the signature (call/where/::) for standard method definitions but just
    # an Identifier for cases like `function f end`.
    sig_idx = findnext(x -> !JuliaSyntax.is_whitespace(x), kids, func_idx + 1)::Int
    if sig_idx == 2
        # Only case where no space is needed after the keyword
        @assert is_longform_anon_function(node) || is_longform_functor(node)
    end
    sig_node = kids[sig_idx]
    # Identifier for regular names but "not function call" for empty functions with Unicode
    # symbols??
    if kind(sig_node) === K"Identifier" || !(kind(sig_node) in KSet"call where :: tuple parens")
        # Empty function definition like `function f end`.
        # TODO: Make sure the spaces around are correct
        end_idx = findnext(x -> kind(x) === K"end", kids, sig_idx + 1)::Int
        end_node = kids[end_idx]
        @assert is_leaf(end_node) && kind(end_node) === K"end"
        if !has_tag(end_node, TAG_DEDENT)
            kids[end_idx] = add_tag(end_node, TAG_DEDENT)
            any_kid_changed = true
        end
        return any_kid_changed ? make_node(node, kids) : nothing
    end
    # K"tuple" when this is an anonymous function
    @assert !is_leaf(sig_node) && kind(sig_node) in KSet"call where :: tuple parens"
    # Fourth node is the function/macro body block.
    block_idx = sig_idx + 1
    let p = position(ctx.fmt_io)
        for i in 1:(block_idx - 1)
            accept_node!(ctx, kids[i])
        end
        block_node′ = indent_block(ctx, kids[block_idx])
        if block_node′ !== nothing
            kids[block_idx] = block_node′
            any_kid_changed = true
        end
        seek(ctx.fmt_io, p)
    end
    # Fifth node is the closing end keyword
    end_idx = findnext(x -> kind(x) === K"end", kids, block_idx + 1)::Int
    end_node = kids[end_idx]
    @assert is_leaf(end_node) && kind(end_node) === K"end"
    if !has_tag(end_node, TAG_DEDENT)
        kids[end_idx] = add_tag(end_node, TAG_DEDENT)
        any_kid_changed = true
    end
    @assert verified_kids(node) === kids
    return any_kid_changed ? make_node(node, kids) : nothing
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
    if span(vars_node) > 0 && length(verified_kids(vars_node)) > 0
        @assert kind(last_leaf(vars_node)) !== "NewlineWs"
    end
    # # Third node is the NewlineWs before the block
    # ln_idx = 3
    # ln_node = kids[ln_idx]
    # @assert is_leaf(ln_node) && kind(ln_node) === K"NewlineWs"
    # Fourth node is the function body block.
    block_idx = findnext(x -> kind(x) === K"block", kids, vars_idx + 1)::Int
    let p = position(ctx.fmt_io)
        for i in 1:(block_idx - 1)
            accept_node!(ctx, kids[i])
        end
        block_node = kids[block_idx]
        @assert !is_leaf(block_node) && kind(block_node) === K"block"
        block_node′ = indent_block(ctx, block_node)
        if block_node′ !== nothing
            kids[block_idx] = block_node′
            any_kid_changed = true
        end
        seek(ctx.fmt_io, p)
    end
    # Look for the end node
    end_idx = findnext(x -> kind(x) === K"end", kids, block_idx + 1)::Int
    @assert is_leaf(kids[end_idx]) && kind(kids[end_idx]) === K"end"
    if !has_tag(kids[end_idx], TAG_DEDENT)
        kids[end_idx] = add_tag(kids[end_idx], TAG_DEDENT)
        any_kid_changed = true
    end
    @assert verified_kids(node) === kids
    return any_kid_changed ? make_node(node, kids) : nothing
end

function indent_begin(ctx::Context, node::Node, block_kind = K"begin")
    @assert kind(node) === K"block"
    pos = position(ctx.fmt_io)
    node′ = indent_block(ctx, node)
    if node′ !== nothing
        node = node′
        any_kid_changed = false
    end
    kids = verified_kids(node)
    any_kid_changed = false
    # First node is the begin/quote keyword
    begin_idx = 1
    begin_node = kids[begin_idx]
    @assert is_leaf(begin_node) && kind(begin_node) === block_kind
    if !has_tag(begin_node, TAG_INDENT)
        kids[begin_idx] = add_tag(begin_node, TAG_INDENT)
        any_kid_changed = true
    end
    # Second node is the newline
    # TODO: Require newline?
    # ln_idx = 2
    # ln_node = kids[ln_idx]
    # @assert is_leaf(ln_node) && kind(ln_node) === K"NewlineWs"
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
    # Reset stream
    seek(ctx.fmt_io, pos)
    return any_kid_changed ? make_node(node, kids) : nothing
end

function indent_block(
        ctx::Context, node::Node; allow_empty::Bool = true, do_indent::Bool = true
    )
    @assert kind(node) === K"block" && !is_leaf(node)
    @assert !JuliaSyntax.has_flags(node, JuliaSyntax.PARENS_FLAG)
    kids = verified_kids(node)
    pos = position(ctx.fmt_io)
    any_kid_changed = false

    # begin-end and quote-end have their respective keywords inside the block...
    is_begin_end = length(kids) > 2 && kind(kids[1]) in KSet"begin quote" &&
        kind(kids[end]) === K"end"
    begin # TODO: let-block if is_begin_end is boxed
        function make_view(x)
            if is_begin_end
                return @view(x[2:(end - 1)])
            else
                return @view(x[:])
            end
        end
        function popatview!(x, idx)
            local p = parent(x)
            if is_begin_end
                item = popat!(p, idx + 1)
            else
                item = popat!(p, idx)
            end
            return make_view(p), item
        end
        function popview!(x)
            return popatview!(x, lastindex(x))
        end
        function insertview!(x, idx, item)
            local p = parent(x)
            if is_begin_end
                insert!(p, idx + 1, item)
            else
                insert!(p, idx, item)
            end
            return make_view(p)
        end
        function pushview!(x, item)
            return insertview!(x, lastindex(x) + 1, item)
        end
    end
    kids′ = make_view(kids)
    if is_begin_end
        accept_node!(ctx, kids[1])
    end

    # If the block is empty and contain no newlines, and empty blocks are allowed, we just
    # return
    if allow_empty && findfirst(!JuliaSyntax.is_whitespace, kids′) === nothing &&
            findfirst(x -> kind(x) === K"NewlineWs", kids′) === nothing
        return nothing
    end

    # Ensure a NewlineWs node at the end of the block (otherwise the closing
    # `end/else/catch/...` is not on a separate line).
    trailing_idx = findlast(x -> kind(x) === K"NewlineWs", kids′)
    if trailing_idx === nothing || trailing_idx != lastindex(kids′)
        # Missing NewlineWs node, insert.
        kids′ = make_view(copy(kids))
        p = position(ctx.fmt_io)
        for k in kids′
            accept_node!(ctx, k)
        end
        # If the previous node is a K"Whitespace" node we just overwrite it instead of
        # merging becuase this whitespace will end up as trailing/leading whitespace anyway.
        if length(kids′) > 0 && kind(kids′[end]) === K"Whitespace"
            spn = span(kids′[end])
            seek(ctx.fmt_io, position(ctx.fmt_io) - spn)
            replace_bytes!(ctx, "", spn)
            kids′, _ = popview!(kids′)
        end
        # Insert a NewlineWs node in the tree and stream
        replace_bytes!(ctx, "\n", 0)
        k = Node(JuliaSyntax.SyntaxHead(K"NewlineWs", JuliaSyntax.TRIVIA_FLAG), 1)
        if do_indent
            k = add_tag(k, TAG_PRE_DEDENT)
        end
        kids′ = pushview!(kids′, k)
        seek(ctx.fmt_io, p)
        any_kid_changed = true
    elseif do_indent && !has_tag(kids′[trailing_idx], TAG_PRE_DEDENT)
        kids′ = make_view(copy(kids))
        kids′[trailing_idx] = add_tag(kids′[trailing_idx], TAG_PRE_DEDENT)
        any_kid_changed = true
    end
    trailing_idx = findlast(x -> kind(x) === K"NewlineWs", kids′)::Int
    @assert trailing_idx == lastindex(kids′)

    # Ensure a NewlineWs node at the beginning of the block (otherwise the opening
    # `begin/try/...` is not on a separate line).
    # Note: Currently a block is allowed to have space + comment before the newline to
    # support trailing comments on the same line as the keyword, e.g.
    # ```
    # let x = 1 # comment
    #     y = x + 1
    # end
    # ```
    # TODO: Perhaps only certain blocks should allow this? E.g. `let` to support comments
    # for the variables (the last comment would end up inside the block)?
    acceptable_newline =
        kmatch(kids′, KSet"NewlineWs") ||
        kmatch(kids′, KSet"; NewlineWs") ||
        kmatch(kids′, KSet"Whitespace ; NewlineWs") ||
        kmatch(kids′, KSet"Comment NewlineWs") ||
        kmatch(kids′, KSet"Whitespace Comment NewlineWs") ||
        kmatch(kids′, KSet"; Comment NewlineWs") ||
        kmatch(kids′, KSet"; Whitespace Comment NewlineWs") ||
        kmatch(kids′, KSet"Whitespace ; Comment NewlineWs") ||
        kmatch(kids′, KSet"Whitespace ; Whitespace Comment NewlineWs")

    if !acceptable_newline
        insert_idx = 1
        if kmatch(kids′, KSet"Whitespace ; Whitespace Comment")
            insert_idx = 5
        elseif kmatch(kids′, KSet"; Whitespace Comment") ||
                kmatch(kids′, KSet"Whitespace ; Comment")
            insert_idx = 4
        elseif kmatch(kids′, KSet"Whitespace ;") ||
                kmatch(kids′, KSet"Whitespace Comment") ||
                kmatch(kids′, KSet"; Comment")
            insert_idx = 3
        elseif kmatch(kids′, KSet";") ||
                kmatch(kids′, KSet"Comment")
            insert_idx = 2
        end
        if kids === parent(kids′)
            kids′ = make_view(copy(kids))
        end
        # If the node is a Whitespace we just overwrite it with a `\n    ` node.
        wsspn = 0
        if kind(kids′[insert_idx]) === K"Whitespace"
            kids′, ws = popatview!(kids′, insert_idx)
            wsspn = span(ws)
        end
        # If we end up in this code path we are most likely splitting a single line block
        # into multiples lines. This means that we haven't yet updated the indent level for
        # the keyword just before this block so in most cases we save a roundtrip by
        # increasing the indent level with 1 here.
        nl = "\n" * " "^(4 * (ctx.indent_level + 1))
        # Skip past whitespace + comment
        for i in 1:(insert_idx - 1)
            accept_node!(ctx, kids′[i])
        end
        replace_bytes!(ctx, nl, wsspn)
        k = Node(JuliaSyntax.SyntaxHead(K"NewlineWs", JuliaSyntax.TRIVIA_FLAG), sizeof(nl))
        kids′ = insertview!(kids′, insert_idx, k)
        any_kid_changed = true
    end
    # Reset stream
    seek(ctx.fmt_io, pos)
    return any_kid_changed ? make_node(node, parent(kids′)) : nothing
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
    let p = position(ctx.fmt_io)
        for i in 1:(block_idx - 1)
            accept_node!(ctx, kids[i])
        end
        block_node′ = indent_block(ctx, kids[block_idx])
        if block_node′ !== nothing
            kids[block_idx] = block_node′
            any_kid_changed = true
        end
        seek(ctx.fmt_io, p)
    end
    return any_kid_changed ? make_node(node, kids) : nothing
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
    let p = position(ctx.fmt_io)
        for i in 1:(try_block_idx - 1)
            accept_node!(ctx, kids[i])
        end
        try_block_node′ = indent_block(ctx, kids[try_block_idx])
        if try_block_node′ !== nothing
            kids[try_block_idx] = try_block_node′
            any_kid_changed = true
        end
        seek(ctx.fmt_io, p)
    end
    # Check for catch/finally. They can be in any order
    catch_idx = findnext(x -> kind(x) in KSet"catch finally", kids, try_block_idx + 1)::Int
    @assert !is_leaf(kids[catch_idx]) && kind(kids[catch_idx]) in KSet"catch finally"
    let p = position(ctx.fmt_io)
        for i in 1:(catch_idx - 1)
            accept_node!(ctx, kids[i])
        end
        catch_node′ = indent_catch(ctx, kids[catch_idx])
        if catch_node′ !== nothing
            kids[catch_idx] = catch_node′
            any_kid_changed = true
        end
        seek(ctx.fmt_io, p)
    end
    # There may be an else in between catch and finally (lol)
    else_idx = findnext(x -> kind(x) === K"else", kids, catch_idx + 1)
    if else_idx !== nothing
        let p = position(ctx.fmt_io)
            for i in 1:(else_idx - 1)
                accept_node!(ctx, kids[i])
            end
            else_node′ = indent_catch(ctx, kids[else_idx])
            if else_node′ !== nothing
                kids[else_idx] = else_node′
                any_kid_changed = true
            end
            seek(ctx.fmt_io, p)
        end
    end
    # Check for the other one
    other_kind = kind(kids[catch_idx]) === K"catch" ? K"finally" : K"catch"
    finally_idx = findnext(
        x -> kind(x) === other_kind, kids, something(else_idx, catch_idx) + 1
    )
    if finally_idx !== nothing
        let p = position(ctx.fmt_io)
            for i in 1:(finally_idx - 1)
                accept_node!(ctx, kids[i])
            end
            finally_node′ = indent_catch(ctx, kids[finally_idx])
            if finally_node′ !== nothing
                kids[finally_idx] = finally_node′
                any_kid_changed = true
            end
            seek(ctx.fmt_io, p)
        end
    end
    # Check for end
    end_idx = findnext(
        x -> kind(x) === K"end", kids, something(finally_idx, else_idx, catch_idx) + 1
    )::Int
    @assert is_leaf(kids[end_idx]) && kind(kids[end_idx]) === K"end"
    if !has_tag(kids[end_idx], TAG_DEDENT)
        kids[end_idx] = add_tag(kids[end_idx], TAG_DEDENT)
        any_kid_changed = true
    end
    @assert verified_kids(node) === kids
    return any_kid_changed ? make_node(node, kids) : nothing
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
    let p = position(ctx.fmt_io)
        for i in 1:(block_idx - 1)
            accept_node!(ctx, kids[i])
        end
        block_node′ = indent_block(ctx, kids[block_idx])
        if block_node′ !== nothing
            kids[block_idx] = block_node′
            any_kid_changed = true
        end
        seek(ctx.fmt_io, p)
    end
    # Check for elseif
    elseif_idx = findnext(x -> kind(x) === K"elseif", kids, block_idx + 1)
    if elseif_idx !== nothing
        @assert !is_leaf(kids[elseif_idx]) && kind(kids[elseif_idx]) === K"elseif"
        let p = position(ctx.fmt_io)
            for i in 1:(elseif_idx - 1)
                accept_node!(ctx, kids[i])
            end
            elseif_node′ = indent_if(ctx, kids[elseif_idx])
            if elseif_node′ !== nothing
                kids[elseif_idx] = elseif_node′
                any_kid_changed = true
            end
            seek(ctx.fmt_io, p)
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
        let p = position(ctx.fmt_io)
            for i in 1:(else_block_idx - 1)
                accept_node!(ctx, kids[i])
            end
            else_block′ = indent_block(ctx, kids[else_block_idx])
            if else_block′ !== nothing
                kids[else_block_idx] = else_block′
                any_kid_changed = true
            end
            seek(ctx.fmt_io, p)
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
    return any_kid_changed ? make_node(node, kids) : nothing
end

function indent_call(ctx::Context, node::Node)
    @assert kind(node) in KSet"call dotcall"
    return indent_paren(ctx, node)
end


# TODO: I feel like this function can be removed. It is only used in `indent_op_call`
function indent_newlines_between_indices(
        ctx::Context, node::Node, open_idx::Int, close_idx::Int;
        indent_closing_token::Bool = false
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
        if kind(kid) === K"NewlineWs" && !has_tag(kid, TAG_LINE_CONT)
            # Tag all direct NewlineWs kids
            kid = add_tag(kid, TAG_LINE_CONT)
            this_kid_changed = true
        elseif is_triple_thing(kid) && (i != open_idx || has_tag(node, TAG_LINE_CONT))
            # TODO: Might be too course to use the tag on the node here...
            # Tag triple strings and triple string macros
            kid′ = indent_triple_thing(ctx, kid)
            if kid′ !== nothing
                kid = kid′
                this_kid_changed = true
            end
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
    return any_kid_changed ? make_node(node, kids) : nothing
end

# Tags opening and closing tokens for indent/dedent and the newline just before the closing
# token as pre-dedent
function indent_listlike(
        ctx::Context, node::Node, open_idx::Int, close_idx::Int;
        indent_closing_token::Bool = false
    )
    kids = verified_kids(node)
    kids′ = kids
    any_kid_changed = false
    # Bail early if there is just a single item
    open_idx == close_idx && return nothing
    # Check whether we expect leading/trailing newlines
    # multiline = contains_outer_newline(kids, open_idx, close_idx)
    # multiline = any(y -> any_leaf(x -> kind(x) === K"NewlineWs", kids[y]), (open_idx + 1):(close_idx - 1))
    multiline = is_multiline_between_idxs(ctx, node, open_idx, close_idx)
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
        elseif kind(kid) === K"parameters"
            # For parameters we want the newline after the semi-colon
            grandkids = verified_kids(kid)
            semi_idx = findfirst(x -> kind(x) === K";", grandkids)::Int
            next_idx = semi_idx + 1
            if kind(grandkids[next_idx]) === K"NewlineWs"
                # Nothing to do
                any_kid_changed && push!(kids′, kid)
                accept_node!(ctx, kid)
            elseif kind(grandkids[next_idx]) === K"Whitespace"
                # Merge with the newline
                let pos = position(ctx.fmt_io)
                    for k in 1:(next_idx - 1)
                        accept_node!(ctx, grandkids[k])
                    end
                    replace_bytes!(ctx, "\n", 0)
                    seek(ctx.fmt_io, pos)
                end
                grandkid = grandkids[next_idx]
                grandkid′ = Node(JuliaSyntax.SyntaxHead(K"NewlineWs", JuliaSyntax.TRIVIA_FLAG), 1 + span(grandkid))
                grandkids′ = copy(grandkids)
                grandkids′[next_idx] = grandkid′
                kid = make_node(kid, grandkids′)
                if kids′ === kids
                    kids′ = kids[1:(open_idx - 1)]
                end
                any_kid_changed = true
                push!(kids′, kid)
                accept_node!(ctx, kid)
            else
                # Insert a newline as the first grandchild
                let pos = position(ctx.fmt_io)
                    for k in 1:semi_idx
                        accept_node!(ctx, grandkids[k])
                    end
                    replace_bytes!(ctx, "\n", 0)
                    seek(ctx.fmt_io, pos)
                end
                grandkids′ = copy(grandkids)
                insert!(grandkids′, next_idx, Node(JuliaSyntax.SyntaxHead(K"NewlineWs", JuliaSyntax.TRIVIA_FLAG), 1))
                kid = make_node(kid, grandkids′)
                if kids′ === kids
                    kids′ = kids[1:(open_idx - 1)]
                end
                any_kid_changed = true
                push!(kids′, kid)
                accept_node!(ctx, kid)
            end
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
            # TODO: I believe this is only reachable because whitespace isn't trimmed from
            #       vcat nodes like function calls and tuples etc.
            @assert kind(node) in KSet"vcat typed_vcat"
            # Since the whitespace is inside the kid node we assume that a newlinews would
            # have ended up inside too. We also remove all existing whitespace since it
            # would otherwise have to be cleaned up by the trailing whitespace pass.
            accept_node!(ctx, kid)
            seek(ctx.fmt_io, position(ctx.fmt_io) - span(last_leaf(kid)))
            replace_bytes!(ctx, "\n", span(last_leaf(kid)))
            nlws = Node(JuliaSyntax.SyntaxHead(K"NewlineWs", JuliaSyntax.TRIVIA_FLAG), 1)
            nlws = add_tag(nlws, TAG_PRE_DEDENT)
            accept_node!(ctx, nlws)
            kid′ = replace_last_leaf(kid, nlws)
            if kids′ === kids
                kids′ = kids[1:(open_idx - 1)]
            end
            any_kid_changed = true
            push!(kids′, kid′)
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
    @assert kind(node) in KSet"call dotcall tuple parens macrocall"
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
        ctx, node, first_operand_idx, last_operand_idx; indent_closing_token = true
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
    # findlast because the condition can also be a block
    block_idx = findlast(x -> kind(x) === K"block", kids)::Int
    let p = position(ctx.fmt_io)
        for i in 1:(block_idx - 1)
            accept_node!(ctx, kids[i])
        end
        block_node′ = indent_block(ctx, kids[block_idx])
        if block_node′ !== nothing
            kids[block_idx] = block_node′
            any_kid_changed = true
        end
        seek(ctx.fmt_io, p)
    end
    end_idx = findlast(x -> kind(x) === K"end", kids)::Int
    if !has_tag(kids[end_idx], TAG_DEDENT)
        kids[end_idx] = add_tag(kids[end_idx], TAG_DEDENT)
        any_kid_changed = true
    end
    return any_kid_changed ? make_node(node, kids) : nothing
end

function indent_implicit_tuple(ctx::Context, node::Node)
    # TODO: This should probably be hard indent?
    @assert kind(node) === K"tuple"
    return continue_all_newlines(ctx, node)
end

function indent_tuple(ctx::Context, node::Node)
    @assert kind(node) === K"tuple"
    kids = verified_kids(node)
    # Check whether this is an explicit tuple, e.g. `(a, b)`, or an implicit tuple,
    # e.g. `a, b`. Implicit tuples only show up in do-blocks(?).
    opening_paren_idx = findfirst(x -> kind(x) === K"(", kids)
    if opening_paren_idx === nothing
        return indent_implicit_tuple(ctx, node)
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
    let p = position(ctx.fmt_io)
        for i in 1:(block_idx - 1)
            accept_node!(ctx, kids[i])
        end
        block_node′ = indent_block(ctx, kids[block_idx])
        if block_node′ !== nothing
            kids[block_idx] = block_node′
            any_kid_changed = true
        end
        seek(ctx.fmt_io, p)
    end
    end_idx = findlast(x -> kind(x) === K"end", kids)::Int
    if !has_tag(kids[end_idx], TAG_DEDENT)
        kids[end_idx] = add_tag(kids[end_idx], TAG_DEDENT)
        any_kid_changed = true
    end
    return any_kid_changed ? make_node(node, kids) : nothing
end

function indent_short_circuit(ctx::Context, node::Node)
    return indent_op_call(ctx, node)
end

function indent_triple_thing(ctx::Context, node::Node)
    @assert is_triple_thing(node)
    if is_triple_string(node)
        return has_tag(node, TAG_LINE_CONT) ? nothing : add_tag(node, TAG_LINE_CONT)
    elseif is_triple_string_macro(node)
        kids = verified_kids(node)
        @assert is_triple_string(kids[2])
        kid′ = indent_triple_thing(ctx, kids[2])
        kid′ === nothing && return nothing
        kids′ = copy(kids)
        kids′[2] = kid′
        return make_node(node, kids′)
    else
        @assert kind(node) === K"juxtapose" && is_triple_string_macro(verified_kids(node)[1])
        kids = verified_kids(node)
        kid′ = indent_triple_thing(ctx, kids[1])
        kid′ === nothing && return nothing
        kids′ = copy(kids)
        kids′[1] = kid′
        return make_node(node, kids′)
    end
end

# TODO: This function can be used for more things than just indent_using I think. Perhaps
# with a max_depth parameter.
function continue_all_newlines(
        ctx::Context, node::Node; skip_last::Bool = true, is_last::Bool = is_leaf(node),
        skip_first::Bool = true, is_first::Bool = true
    )
    # Not sure these need to arguments since they should always(?) be `true`.
    @assert skip_last
    @assert skip_first
    if is_leaf(node)
        if kind(node) === K"NewlineWs" && !has_tag(node, TAG_LINE_CONT) &&
                !((skip_last && is_last) || (skip_first && is_first))
            return add_tag(node, TAG_LINE_CONT)
        else
            return nothing
        end
    elseif is_triple_thing(node)
        # Check skip_first inside to break the recursion and considier triple strings leafs
        if !(skip_first && is_first)
            return indent_triple_thing(ctx, node)
        else
            return nothing
        end
    else
        any_kid_changed = false
        kids = verified_kids(node)
        for (i, kid) in pairs(kids)
            kid′ = continue_all_newlines(
                ctx, kid; skip_last = skip_last, is_last = i == lastindex(kids),
                skip_first = skip_first, is_first = is_first && i == firstindex(kids)
            )
            if kid′ !== nothing
                kids[i] = kid′
                any_kid_changed = true
            end
        end
        return any_kid_changed ? make_node(node, kids) : nothing
    end
end

function indent_using_import_export_public(ctx::Context, node::Node)
    @assert kind(node) in KSet"using import export public global local"
    return continue_all_newlines(ctx, node)
end

function indent_ternary(ctx::Context, node::Node)
    @assert kind(node) === K"?"
    return continue_all_newlines(ctx, node)
end

function indent_iterator(ctx::Context, node::Node)
    @assert kind(node) in KSet"cartesian_iterator generator"
    return continue_all_newlines(ctx, node)
end

function indent_assignment(ctx::Context, node::Node)
    @assert !is_leaf(node)
    @assert is_variable_assignment(ctx, node)
    kids = verified_kids(node)
    lhsidx = findfirst(!JuliaSyntax.is_whitespace, kids)::Int
    eqidx = findnext(!JuliaSyntax.is_whitespace, kids, lhsidx + 1)::Int
    @assert kind(node) === kind(kids[eqidx])
    @assert length(kids) > eqidx
    rhsidx = findnext(!JuliaSyntax.is_whitespace, kids, eqidx + 1)::Int
    r = (eqidx + 1):(rhsidx - 1)
    length(r) == 0 && return nothing
    rhs = kids[rhsidx]
    # Some right hand sides have more "inertia" towards indentation. This is so that we
    # will end up with e.g.
    # ```
    # x =
    # if cond
    # end
    # ```
    # instead of
    # ```
    # x =
    #     if cond
    # end
    # ```
    # TODO: Remove newlines inbetween the `=` and the rhs to end up with
    # ```
    # x = if cond
    # end
    # ```
    blocklike = kind(rhs) in KSet"if try function let" || is_triple_thing(rhs)
    blocklike && return nothing # TODO: Perhaps delete superfluous newlines?
    # Continue all newlines between the `=` and the rhs
    kids′ = kids
    changed = false
    for i in r
        kid = kids[i]
        if kind(kid) === K"NewlineWs" && !has_tag(kid, TAG_LINE_CONT)
            kid′ = add_tag(kid, TAG_LINE_CONT)
            changed = true
        else
            kid′ = kid
        end
        if changed
            if kids′ === kids
                kids′ = kids[1:(i - 1)]
            end
            push!(kids′, kid′)
        end
    end
    # Mark the rhs for line continuation
    if !has_tag(rhs, TAG_LINE_CONT)
        rhs = add_tag(rhs, TAG_LINE_CONT)
        changed = true
        if kids′ === kids
            kids′ = kids[1:(rhsidx - 1)]
        end
        push!(kids′, rhs)
    else
        changed && push!(kids′, rhs)
    end
    if changed
        @assert kids !== kids′
        for i in (rhsidx + 1):length(kids)
            push!(kids′, kids[i])
        end
        return make_node(node, kids′)
    else
        return nothing
    end
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
    let p = position(ctx.fmt_io)
        for i in 1:(block_idx - 1)
            accept_node!(ctx, kids[i])
        end
        block_node′ = indent_block(ctx, kids[block_idx])
        if block_node′ !== nothing
            kids[block_idx] = block_node′
            any_kid_changed = true
        end
        seek(ctx.fmt_io, p)
    end
    # Closing `end`
    end_idx = findnext(x -> kind(x) === K"end", kids, block_idx + 1)::Int
    if !has_tag(kids[end_idx], TAG_DEDENT)
        kids[end_idx] = add_tag(kids[end_idx], TAG_DEDENT)
        any_kid_changed = true
    end
    return any_kid_changed ? make_node(node, kids) : nothing
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
        @assert block_idx == 1 # Otherwise need to seek the stream
        block_node′ = indent_begin(ctx, kids[block_idx], K"quote")
        if block_node′ !== nothing
            kids[block_idx] = block_node′
            any_kid_changed = true
        end
        return any_kid_changed ? make_node(node, kids) : nothing
    else
        # The short form can be ignored since the inside (K"block", K"tuple", or
        # K"Identifier") of the quote will be handled by other passes.
        return nothing
    end
end

# Literal array nodes and also ref-nodes (which can be either a typed-array or a getindex)
function indent_array(ctx::Context, node::Node)
    @assert kind(node) in KSet"vect vcat typed_vcat ncat ref comprehension typed_comprehension"
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
function indent_module(ctx::Context, node::Node; do_indent::Bool = true)
    @assert kind(node) === K"module"
    kids = verified_kids(node)
    any_kid_changed = false
    pos = position(ctx.fmt_io)
    # First node is the module keyword
    mod_idx = 1
    mod_node = kids[mod_idx]
    @assert is_leaf(mod_node) && kind(mod_node) in KSet"module baremodule"
    if do_indent && !has_tag(mod_node, TAG_INDENT)
        kids[mod_idx] = add_tag(mod_node, TAG_INDENT)
        any_kid_changed = true
    end
    # Next we expect whitespace + identifier, but can also be expression with whitespace
    # hidden inside...
    space_idx = 2
    space_node = kids[space_idx]
    if kind(space_node) === K"Whitespace"
        # Now we need an identifier or var"
        id_idx = 3
        id_node = kids[id_idx]
        @assert kind(id_node) in KSet"Identifier var"
        block_idx = 4
    else
        # This can be reached if the module name is interpolated or parenthesized, for
        # example.
        @assert kind(first_leaf(space_node)) in KSet"Whitespace ("
        @assert !JuliaSyntax.is_whitespace(space_node)
        block_idx = 3
    end
    # Next node is the module body block.
    let p = position(ctx.fmt_io)
        for i in 1:(block_idx - 1)
            accept_node!(ctx, kids[i])
        end
        block_node′ = indent_block(ctx, kids[block_idx]; do_indent = do_indent)
        if block_node′ !== nothing
            kids[block_idx] = block_node′
            any_kid_changed = true
        end
        seek(ctx.fmt_io, p)
    end
    # Skip until the closing end keyword
    end_idx = findnext(x -> kind(x) === K"end", kids, block_idx + 1)
    end_node = kids[end_idx]
    @assert is_leaf(end_node) && kind(end_node) === K"end"
    if do_indent && !has_tag(end_node, TAG_DEDENT)
        kids[end_idx] = add_tag(end_node, TAG_DEDENT)
        any_kid_changed = true
    end
    @assert verified_kids(node) === kids
    # Reset the stream
    seek(ctx.fmt_io, pos)
    return any_kid_changed ? make_node(node, kids) : nothing
end

# The only thing at top level that we need to indent are modules which don't occupy the full
# top level expression, for example a file with an inner module followed by some code.
function indent_toplevel(ctx::Context, node::Node)
    @assert kind(node) === K"toplevel"
    kids = verified_kids(node)
    mod_idx = findfirst(x -> kind(x) === K"module", kids)
    if mod_idx === nothing
        # No module here
        return nothing
    end
    # If the only top level expression is a module we don't indent it
    do_indent = count(!JuliaSyntax.is_whitespace, kids) > 1
    any_kid_changed = false
    while mod_idx !== nothing
        let p = position(ctx.fmt_io)
            for i in 1:(mod_idx - 1)
                accept_node!(ctx, kids[i])
            end
            mod_node′ = indent_module(ctx, kids[mod_idx]; do_indent = do_indent)
            if mod_node′ !== nothing
                kids[mod_idx] = mod_node′
                any_kid_changed = true
            end
            seek(ctx.fmt_io, p)
        end
        mod_idx = findnext(x -> kind(x) === K"module", kids, mod_idx + 1)
    end
    return any_kid_changed ? make_node(node, kids) : nothing
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
    elseif kind(node) === K"macrocall" &&
            JuliaSyntax.has_flags(node, JuliaSyntax.PARENS_FLAG)
        return indent_paren(ctx, node)
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
    elseif kind(node) in KSet"using import export public" || is_global_local_list(node)
        return indent_using_import_export_public(ctx, node)
    elseif is_variable_assignment(ctx, node)
        return indent_assignment(ctx, node)
    elseif kind(node) === K"parameters"
        return indent_parameters(ctx, node)
    elseif kind(node) === K"?"
        return indent_ternary(ctx, node)
    elseif kind(node) in KSet"cartesian_iterator generator"
        return indent_iterator(ctx, node)
    elseif kind(node) === K"try"
        return indent_try(ctx, node)
    elseif kind(node) === K"quote"
        return indent_quote(ctx, node)
    elseif kind(node) === K"do"
        return indent_do(ctx, node)
    elseif is_paren_block(node)
        return indent_paren_block(ctx, node)
    elseif kind(node) in KSet"vect vcat typed_vcat ncat ref comprehension typed_comprehension"
        return indent_array(ctx, node)
    elseif kind(node) in KSet"row"
        return indent_array_row(ctx, node)
    elseif kind(node) === K"comparison"
        return indent_comparison(ctx, node)
    elseif kind(node) === K"toplevel"
        return indent_toplevel(ctx, node)
    elseif kind(node) === K"module"
        do_indent = findlast(x -> x === K"module", ctx.lineage_kinds) !== nothing
        return indent_module(ctx, node; do_indent = do_indent)
    end
    return nothing
end

function indent_multiline_strings(ctx::Context, node::Node)
    if !is_triple_string(node)
        return nothing
    end
    triplekind = kind(node) === K"string" ? K"\"\"\"" : K"```"
    itemkind = kind(node) === K"string" ? K"String" : K"CmdString"
    indent_span = 4 * ctx.indent_level
    if has_tag(node, TAG_LINE_CONT)
        indent_span += 4
    end
    indented = indent_span > 0

    pos = position(ctx.fmt_io)
    kids = verified_kids(node)
    kids′ = kids
    any_changes = false

    # Fastpath for the common case of top level multiline strings like e.g. docstrings
    if !indented && findfirst(x -> kind(x) === K"Whitespace", kids) === nothing
        return nothing
    end

    # Opening triple quote
    open_idx = findfirst(x -> kind(x) === triplekind, kids)::Int
    close_idx = findlast(x -> kind(x) === triplekind, kids)::Int
    @assert close_idx == length(kids) # ?
    open_kid = kids[open_idx]
    @assert kind(open_kid) === triplekind
    accept_node!(ctx, open_kid)

    # Loop over the lines/expressions
    idx = open_idx + 1
    state = :expect_something
    while idx < close_idx
        kid = kids[idx]
        if state === :expect_something
            if kind(kid) === itemkind
                if indented && span(kid) > 0 && read_bytes(ctx, kid)[end] == UInt8('\n')
                    state = :expect_indent_ws
                end
                accept_node!(ctx, kid)
                any_changes && push!(kids′, kid)
            elseif kind(kid) === K"Whitespace"
                bytes = read_bytes(ctx, kid)
                # Multiline strings with trailing \ will have non-space characters in the
                # Whitespace node. These should be preserved.
                # TODO: Maybe this should be continue-indent to highlight the continuation?
                if length(bytes) == 2 + indent_span && bytes[1] === UInt8('\\') && bytes[2] === UInt8('\n')
                    @assert all(x -> x in (UInt8(' '), UInt8('\t')), @view(bytes[3:end]))
                    # This node is correct
                    accept_node!(ctx, kid)
                    any_changes && push!(kids′, kid)
                elseif length(bytes) >= 2 && bytes[1] === UInt8('\\') && bytes[2] === UInt8('\n')
                    @assert all(x -> x in (UInt8(' '), UInt8('\t')), @view(bytes[3:end]))
                    if length(bytes) < 2 + indent_span
                        # Insert the missing spaces
                        while length(bytes) < 2 + indent_span
                            push!(bytes, UInt8(' '))
                        end
                    else
                        @assert length(bytes) > 2 + indent_span
                        # Truncate spaces
                        resize!(bytes, 2 + indent_span)
                    end
                    replace_bytes!(ctx, bytes, span(kid))
                    if kids′ === kids
                        kids′ = kids[1:(idx - 1)]
                    end
                    kid′ = Node(JuliaSyntax.SyntaxHead(K"Whitespace", JuliaSyntax.TRIVIA_FLAG), length(bytes))
                    accept_node!(ctx, kid′)
                    push!(kids′, kid′)
                    any_changes = true
                else
                    # Delete this node completely
                    @assert all(x -> x in (UInt8(' '), UInt8('\t')), bytes)
                    replace_bytes!(ctx, "", span(kid))
                    if kids′ === kids
                        kids′ = kids[1:(idx - 1)]
                    end
                    any_changes = true
                end
            else
                accept_node!(ctx, kid)
                any_changes && push!(kids′, kid)
            end
        else
            @assert state === :expect_indent_ws
            state = :expect_something
            if kind(kid) === itemkind && span(kid) == 1 && peek(ctx.fmt_io) == UInt8('\n')
                # If this line is empty there shouldn't be a whitespace node. Switch the
                # state and loop around with the same idx.
                state = :expect_something
                continue # Skip the index increment
            elseif begin
                    cond = false
                    if kind(kid) === K"Whitespace" && idx + 1 < close_idx &&
                            kind(kids[idx + 1]) === itemkind && span(kids[idx + 1]) == 1
                        peekpos = position(ctx.fmt_io)
                        accept_node!(ctx, kid)
                        accept_node!(ctx, kids[idx + 1])
                        seek(ctx.fmt_io, position(ctx.fmt_io) - 1)
                        cond = peek(ctx.fmt_io) == UInt8('\n')
                        seek(ctx.fmt_io, peekpos)
                    end
                    cond
                end
                # If this whitespace is followed by an empty string it should be deleted
                state = :expect_something
                continue # Skip the index increment
            elseif kind(kid) === K"Whitespace" && span(kid) == indent_span
                @assert all(x -> x === UInt8(' '), read_bytes(ctx, kid))
                accept_node!(ctx, kid)
                any_changes && push!(kids′, kid)
            elseif kind(kid) === K"Whitespace"
                replace_bytes!(ctx, " "^indent_span, span(kid))
                if kids′ === kids
                    kids′ = kids[1:(idx - 1)]
                end
                kid′ = Node(head(kid), indent_span, tags(kid))
                any_changes = true
                push!(kids′, kid′)
                accept_node!(ctx, kid′)
            else
                replace_bytes!(ctx, " "^indent_span, 0)
                if kids′ === kids
                    kids′ = kids[1:(idx - 1)]
                end
                kid′ = Node(JuliaSyntax.SyntaxHead(K"Whitespace", JuliaSyntax.TRIVIA_FLAG), indent_span)
                any_changes = true
                push!(kids′, kid′)
                accept_node!(ctx, kid′)
                continue # Skip the index increment
            end
        end
        idx += 1
    end
    # Make sure to add indent before the closing triple quote
    if state === :expect_indent_ws
        replace_bytes!(ctx, " "^indent_span, 0)
        if kids′ === kids
            kids′ = kids[1:(idx - 1)]
        end
        kid′ = Node(JuliaSyntax.SyntaxHead(K"Whitespace", JuliaSyntax.TRIVIA_FLAG), indent_span)
        any_changes = true
        push!(kids′, kid′)
        accept_node!(ctx, kid′)
    end
    @assert idx == close_idx
    # Closing triple quote
    close_kid = kids[close_idx]
    @assert kind(close_kid) === triplekind
    accept_node!(ctx, close_kid)
    any_changes && push!(kids′, close_kid)
    # Reset stream
    seek(ctx.fmt_io, pos)
    return any_changes ? make_node(node, kids′) : nothing
end

# Pattern matching for "bad" semicolons:
#  - `\s*;\n` -> `\n`
#  - `\s*;\s*#\n` -> `\s* \s*#\n`
function remove_trailing_semicolon_block(ctx::Context, node::Node)
    kind(node) === K"block" || return nothing
    @assert !is_leaf(node)
    pos = position(ctx.fmt_io)
    kids = verified_kids(node)
    kids′ = kids
    dealias() = kids′ === kids ? copy(kids) : kids′
    semi_idx = findfirst(x -> kind(x) === K";", kids′)
    while semi_idx !== nothing
        search_index = semi_idx + 1
        if kmatch(kids′, KSet"; NewlineWs", semi_idx)
            # `\s*;\n` -> `\n`
            kids′ = dealias()
            space_before = kmatch(kids′, KSet"Whitespace ;", semi_idx - 1)
            if space_before
                span_overwrite = span(kids′[semi_idx - 1]) + span(kids′[semi_idx])
                nodes_to_skip_over = semi_idx - 2
                deleteat!(kids′, semi_idx)
                deleteat!(kids′, semi_idx - 1)
                search_index = semi_idx - 1
            else
                span_overwrite = span(kids′[semi_idx])
                nodes_to_skip_over = semi_idx - 1
                deleteat!(kids′, semi_idx)
                search_index = semi_idx
            end
            let p = position(ctx.fmt_io)
                for i in 1:nodes_to_skip_over
                    accept_node!(ctx, kids′[i])
                end
                replace_bytes!(ctx, "", span_overwrite)
                seek(ctx.fmt_io, p)
            end
        elseif kmatch(kids′, KSet"; Comment NewlineWs", semi_idx) ||
                kmatch(kids′, KSet"; Whitespace Comment NewlineWs", semi_idx)
            # `\s*;\s*#\n` -> `\s* \s*#\n`
            # The `;` is replaced by ` ` here in case comments are aligned
            kids′ = dealias()
            ws_span = span(kids′[semi_idx])
            @assert ws_span == 1
            space_before = kmatch(kids′, KSet"Whitespace ;", semi_idx - 1)
            if space_before
                ws_span += span(kids′[semi_idx - 1])
            end
            space_after = kmatch(kids′, KSet"; Whitespace", semi_idx)
            if space_after
                ws_span += span(kids′[semi_idx + 1])
            end
            let p = position(ctx.fmt_io)
                for i in 1:(semi_idx - 1)
                    accept_node!(ctx, kids′[i])
                end
                replace_bytes!(ctx, " ", span(kids′[semi_idx]))
                seek(ctx.fmt_io, p)
            end
            # Insert new node
            @assert kind(kids′[semi_idx]) === K";"
            ws = Node(JuliaSyntax.SyntaxHead(K"Whitespace", JuliaSyntax.TRIVIA_FLAG), ws_span)
            kids′[semi_idx] = ws
            # Delete the consumed whitespace nodes
            space_after && deleteat!(kids′, semi_idx + 1)
            space_before && deleteat!(kids′, semi_idx - 1)
        end
        # Compute next index
        semi_idx = findnext(x -> kind(x) === K";", kids′, search_index)
    end
    # Reset the stream and return
    seek(ctx.fmt_io, pos)
    return kids′ === kids ? nothing : make_node(node, kids′)
end

function remove_trailing_semicolon(ctx::Context, node::Node)
    if is_begin_block(node)
        r = remove_trailing_semicolon_block(ctx, node)
        return r
    end
    if !(!is_leaf(node) && kind(node) in KSet"if elseif quote function for let while macro try catch finally else do struct")
        return nothing
    end
    if kind(node) === K"quote" && JuliaSyntax.has_flags(node, JuliaSyntax.COLON_QUOTE)
        # This node is `:(...)` and not `quote...end`
        return nothing
    end
    pos = position(ctx.fmt_io)
    kids = verified_kids(node)
    kids′ = kids
    block_predicate = function(x)
        return kind(x) === K"block" && !JuliaSyntax.has_flags(x, JuliaSyntax.PARENS_FLAG)
    end
    block_idx = findfirst(block_predicate, kids′)
    if kind(node) === K"let"
        # The first block of let is the variables
        block_idx = findnext(block_predicate, kids′, block_idx + 1)
    end
    any_changed = false
    while block_idx !== nothing
        let p = position(ctx.fmt_io)
            for i in 1:(block_idx - 1)
                accept_node!(ctx, kids′[i])
            end
            block′ = remove_trailing_semicolon_block(ctx, kids′[block_idx])
            if block′ !== nothing
                any_changed = true
                if kids′ === kids
                    kids′ = copy(kids)
                end
                kids′[block_idx] = block′
            end
            seek(ctx.fmt_io, p)
        end
        block_idx = findnext(block_predicate, kids′, block_idx + 1)
    end
    # Reset the stream and return
    seek(ctx.fmt_io, pos)
    return any_changed ? make_node(node, kids′) : nothing
end

function spaces_around_comments(ctx::Context, node::Node)
    is_leaf(node) && return
    pos = position(ctx.fmt_io)
    kids = verified_kids(node)
    kids′ = kids
    # We assume that the previous node ends with ws, which should be true since the same
    # pass here adds it if the first kid is a comment.
    prev_kid_ends_with_ws = true
    ws = Node(JuliaSyntax.SyntaxHead(K"Whitespace", JuliaSyntax.TRIVIA_FLAG), 1)
    for (i, kid) in pairs(kids)
        if kind(kid) === K"Comment" ||
                (fl = first_leaf(kid); fl !== nothing && kind(fl) === K"Comment")
            # TODO: In the case where the comment is found within the kid the whitespace
            # should maybe be added right before the comment in the tree (which is how
            # JuliaSyntax would have parsed the source if the space was already there). I
            # don't know if this really matters though since it is already pretty random
            # where whitespace ends up.
            if !prev_kid_ends_with_ws
                kids′ = kids′ === kids ? kids[1:(i - 1)] : kids′
                push!(kids′, ws)
                replace_bytes!(ctx, " ", 0)
                accept_node!(ctx, ws)
            end
        end
        if kids′ !== kids
            push!(kids′, kid)
        end
        accept_node!(ctx, kid)
        prev_kid_ends_with_ws = kind(kid) in KSet"Whitespace NewlineWs" ||
            (ll = last_leaf(kid); ll !== nothing && kind(ll) in KSet"Whitespace NewlineWs")
    end
    # Reset the stream and return
    seek(ctx.fmt_io, pos)
    if kids === kids′
        return nothing
    else
        return make_node(node, kids′)
    end
end

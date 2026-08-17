# SPDX-License-Identifier: MIT

# This is the runestone where all the formatting transformations are implemented.

###########################################################
# NodeBuilder: copy-on-write emitter for rebuilding kids  #
###########################################################

# Builder for rules that rebuild the kids vector of a node while keeping the output
# stream (`ctx.fmt_io`) in sync. It encapsulates the copy-on-write protocol: the output
# aliases the original kids until the first change, at which point the already-emitted
# prefix is copied. The number of emitted kids is tracked internally, which makes it
# impossible to copy the wrong prefix (a bug class that has occurred when call sites
# computed the prefix index by hand).
mutable struct NodeBuilder
    const ctx::Context
    const kids::Vector{Node}             # original kids; lookahead (e.g. kmatch) unaffected
    kids′::Union{Vector{Node}, Nothing}  # output; `nothing` while it aliases kids[1:n_accepted]
    n_accepted::Int
    const pos::Int                       # stream position at the start of the parent node
end

function NodeBuilder(ctx::Context, node::Node)
    return NodeBuilder(ctx, verified_kids(node), nothing, 0, position(ctx.fmt_io))
end

function NodeBuilder(ctx::Context, kids::Vector{Node})
    return NodeBuilder(ctx, kids, nothing, 0, position(ctx.fmt_io))
end

# Copy the already-emitted prefix on the first change.
function materialize!(b::NodeBuilder)
    if b.kids′ === nothing
        b.kids′ = b.kids[1:b.n_accepted]
    end
    return b.kids′::Vector{Node}
end

# Pass an original kid through unchanged.
function accept!(b::NodeBuilder, kid::Node)
    if b.kids′ === nothing
        @assert kid === b.kids[b.n_accepted + 1]
        b.n_accepted += 1
    else
        push!(b.kids′::Vector{Node}, kid)
    end
    accept_node!(b.ctx, kid)
    return
end

# Emit a changed or new node whose bytes in the stream are already correct.
function emit!(b::NodeBuilder, kid′::Node)
    push!(materialize!(b), kid′)
    accept_node!(b.ctx, kid′)
    return
end

# Emit a node together with a byte edit: `bytes` replaces the `old_span` bytes at the
# current stream position (`old_span == 0` means pure insertion).
function emit!(
        b::NodeBuilder, kid′::Node, bytes::Union{String, AbstractVector{UInt8}},
        old_span::Integer
    )
    materialize!(b)
    replace_bytes!(b.ctx, bytes, old_span)
    push!(b.kids′::Vector{Node}, kid′)
    accept_node!(b.ctx, kid′)
    return
end

# Drop an original kid: delete its bytes and emit nothing.
function skip_kid!(b::NodeBuilder, kid::Node)
    materialize!(b)
    replace_bytes!(b.ctx, "", span(kid))
    return
end

# Take back the most recently emitted kid: remove it from the output and rewind the
# stream over it.
function unemit!(b::NodeBuilder)
    local kid
    if b.kids′ === nothing
        kid = b.kids[b.n_accepted]
        b.n_accepted -= 1
    else
        kid = pop!(b.kids′::Vector{Node})
    end
    seek(b.ctx.fmt_io, position(b.ctx.fmt_io) - span(kid))
    return kid
end

# Reset the stream to the parent start and return the replacement node, or `nothing` if
# no kid changed (the unit-rule contract).
function finish!(b::NodeBuilder, node::Node)
    seek(b.ctx.fmt_io, b.pos)
    kids′ = b.kids′
    return kids′ === nothing ? nothing : make_node(node, kids′)
end

function trim_trailing_whitespace(ctx::Context, node::Node)
    kind(node) in KSet"NewlineWs Comment" || return nothing
    @assert is_leaf(node)
    str = String(read_bytes(ctx, node))
    local str′::String
    if kind(node) === K"NewlineWs"
        # Strip all whitespace up until the newline while normalizing line endings to \n
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
    @assert nb == sizeof(str′)
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
    # Ignore literals with underscores for now (same as for floats)
    if findfirst(==('_' % UInt8), bytes) !== nothing
        return nothing
    end
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
    # Note that Julia considers '−' (Unicode U+2212) to be a synonym to the normally used
    # '-' (ASCII/Unicode U+002D) so we need to check for the former in this regex, and
    # normalize it to '-' when writing it out.
    r = r"^(?<sgn>[+\-−])?(?<int>\d*)(?:\.?(?<frac>\d*))?(?:(?<epm>[eEf][+\-−]?)(?<exp>\d+))?$"
    m = match(r, str)::RegexMatch
    io = IOBuffer() # TODO: Could be reused?
    # Write the sign part
    if (sgn = m[:sgn]; sgn !== nothing)
        write(io, replace(sgn, "−" => "-")) # \u2212 => \u002D
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
    if (epm = m[:epm]; epm !== nothing)
        write(io, replace(epm, "E" => "e", "−" => "-")) # \u2212 => \u002D
        @assert m[:exp] !== nothing
        # Strip leading zeros from integral part
        exp_part = replace(m[:exp]::AbstractString, r"^0*((?:[1-9]\d*)|0)$" => s"\1")
        write(io, exp_part)
    end
    str′ = String(take!(io))
    # The happy path regex above is an optimization and not a termination guarantee: if the
    # rebuilt literal is identical to the input the node must be accepted here, otherwise
    # the driver would re-run this function until its iteration guard trips.
    if str′ == str
        return nothing
    end
    nb = replace_bytes!(ctx, str′, span(node))
    @assert nb == sizeof(str′)
    # Create new node and return it
    node′ = Node(head(node), nb)
    return node′
end

# Insert space around `x`, where `x` can be operators, assignments, etc. with the pattern:
# `<something><space><x><space><something>`, for example the spaces around `+` and `=` in
# `a = x + y`.
function spaces_around_x(ctx::Context, node::Node, n_leaves_per_x::Int = 1)
    @assert !is_leaf(node)

    kids = verified_kids(node)
    ws = ws_node(1)

    # Toggle for whether we are currently looking for whitespace or not
    looking_for_whitespace = false
    looking_for_x = false
    n_x_leaves_visited = 0

    peek(kids, i) = i < length(kids) ? kind(kids[i + 1]) : nothing

    b = NodeBuilder(ctx, node)

    for (i, kid) in pairs(kids)
        if kind(kid) === K"NewlineWs" ||
                (i == 1 && kind(kid) === K"Whitespace")
            # NewlineWs are accepted as is by this pass.
            # Whitespace is accepted as is if this is the first kid even if the span is
            # larger than we expect since we don't look backwards. It should be cleaned up
            # by some other pass.
            accept!(b, kid)
            looking_for_whitespace = false
        elseif looking_for_whitespace && kind(kid) === K"Whitespace"
            if span(kid) == 1 ||
                    (peek(kids, i) === K"Comment" && peek(kids, i + 1) === K"NewlineWs")
                # All good, just advance the IO
                accept!(b, kid)
            else
                # Whitespace node but replace since not single space
                emit!(b, ws, " ", span(kid))
            end
            looking_for_whitespace = false
        else
            if looking_for_whitespace
                # Not a whitespace node, insert one before handling the node itself
                @assert !(first_leaf(kid) in KSet"Whitespace NewlineWs")
                emit!(b, ws, " ", 0)
            else
                # We end up here if we look for x, or the things in between x's
                @assert kind(kid) !== K"Whitespace" # This would be weird, I think?
            end
            accept!(b, kid)
            looking_for_whitespace = kind(last_leaf(kid)) !== K"Whitespace"
            if kind(kid) === K"Comment"
                # Just skip through and keep the state?
            elseif looking_for_x
                # We are looking for x, check we have them all otherwise keep looking
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
    return finish!(b, node)
end

# Insert space after comma and semicolon in list-like expressions. Aim for the form
# `<nospace><item><comma><space><item><comma><space>...<item><nospace>`.
# TODO: Why did this function become sooo complicated?
# A kid of a listlike node that is an actual item, i.e. not whitespace or a separator.
is_list_item(x::Node) = !(JuliaSyntax.is_whitespace(x) || kind(x) in KSet", ;")

# Compute the trailing comma/semicolon policy for a listlike node in spaces_in_listlike.
# Return (require_trailing_comma, allow_trailing_comma, allow_trailing_semi).
function trailing_comma_policy(
        ctx::Context, node::Node, kids::Vector{Node}, n_items::Int,
        first_item_idx::Union{Int, Nothing}, last_item_idx::Union{Int, Nothing},
        closing_leaf_idx::Int, implicit_tuple::Bool, multiline::Bool,
        is_named_tuple::Bool, last_item_frozen::Bool,
    )
    # A trailing comma is required if
    #  - node is a single item tuple which is not from an anonymous fn (Julia-requirement)
    #  - the closing token is not on the same line as the last item (Runic-requirement)
    require_trailing_comma = false
    allow_trailing_semi = false
    allow_trailing_comma = multiline
    if kind(node) in KSet"call dotcall macrocall" ||
            (
            kind(node) === K"tuple" && ctx.lineage_kinds[end] === K"->" && ctx.next_sibling !== nothing &&
                !(n_items == 1 && kind(kids[first_item_idx::Int]) === K"tuple")
        )
        # For calls and anonymous function argument tuples, trailing commas are never required
        # (and only allowed if multiline, same as regular function definitions).
        # Exception: `((a, b),) -> body` — when the single item is itself a tuple the outer
        # trailing comma must be kept, otherwise `((a, b))` is parsed as `(a, b)` (2 args).
        require_trailing_comma = false
    elseif n_items > 0 && kind(kids[last_item_idx::Int]) === K"generator"
        # https://github.com/fredrikekre/Runic.jl/issues/151
        require_trailing_comma = false
    elseif implicit_tuple
        # Trailing commas in implicit tuples in the LHS of an assignment, e.g. `x, = 1, 2`,
        # is required for single item tuples and allowed for multiple items (to allow e.g.
        # `x, y, = z` signaling that z contains more items).
        # Note that an implicit single item tuple can never(?) end up on the RHS so there is
        # no need to make sure this is the LHS node.
        require_trailing_comma = n_items == 1 && ctx.lineage_kinds[end] === K"="
        allow_trailing_comma = ctx.lineage_kinds[end] === K"="
    elseif kind(node) === K"tuple" && n_items == 1 && ctx.lineage_kinds[end] === K"function" &&
            kind(kids[first_item_idx::Int]) === K"macrocall" &&
            JuliaSyntax.has_flags(kids[first_item_idx], JuliaSyntax.PARENS_FLAG)
        # `function (@a(b),) body end` style: require trailing comma to preserve 1-tuple argument.
        require_trailing_comma = true
    elseif kind(node) === K"tuple" && n_items == 1 &&
            !(ctx.lineage_kinds[end] === K"function" && ctx.next_sibling !== nothing) &&
            kind(kids[first_item_idx::Int]) !== K"parameters"
        # TODO: May also have to check for K"where" and K"::" in the lineage above
        # The K"function" + next_sibling guard excludes argument tuples of anonymous
        # functions (`function (args) body end`) which are not the last child, while
        # still requiring trailing commas for body tuples of short-form definitions
        # (`f() = (a,)`) which are the last child.
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
    elseif n_items > 0 && predicate_contains(kids[last_item_idx]) do nd
            return kind(nd) === K"macrocall" &&
                !JuliaSyntax.has_flags(nd, JuliaSyntax.PARENS_FLAG) && !is_string_macro(nd)
        end
        # Unparenthesized macrocalls are scary even if hidden deep in the tree
        require_trailing_comma = false
    elseif multiline
        require_trailing_comma = true
    elseif n_items > 0
        require_trailing_comma = any(
            x -> kind(x) === K"NewlineWs", @view(kids[(last_item_idx + 1):(closing_leaf_idx - 1)])
        )
    end
    # If the last item is inside a `# runic: off` region we can't insert the trailing comma
    # after it without touching the region. Trailing commas that are required by the parser
    # (single item tuples etc.) are always already present in the source, so the only thing
    # given up here is the Runic-mandated trailing comma of multiline lists.
    if require_trailing_comma && last_item_frozen
        require_trailing_comma = false
    end
    return require_trailing_comma, allow_trailing_comma, allow_trailing_semi
end

# Handle a K"parameters" kid in spaces_in_listlike: apply trailing comma tags and drop
# the node if it is empty and not needed for a trailing semicolon. Tagging is controlled
# by `require_tag`/`allow_tag` and dropping by `drop_if_empty`; the caller pre-combines
# these with the freeze status since a frozen kid is never modified.
function handle_parameters_kid!(
        b::NodeBuilder, kid::Node, require_tag::Bool, allow_tag::Bool, drop_if_empty::Bool
    )
    changed = false
    if require_tag && !has_tag(kid, TAG_TRAILING_COMMA)
        # Tag the node to require a trailing comma
        kid = add_tag(kid, TAG_TRAILING_COMMA)
        changed = true
    end
    if allow_tag && !has_tag(kid, TAG_TRAILING_COMMA_OPT)
        # Tag the node to optionally have a trailing comma
        kid = add_tag(kid, TAG_TRAILING_COMMA_OPT)
        changed = true
    end
    if drop_if_empty && count(is_list_item, verified_kids(kid)) == 0
        # If kid is K"parameters" without items and we don't want the K"parameters" node
        # for the trailing semicolon we drop the entire node
        grandkids = verified_kids(kid)
        @assert length(grandkids) == 1 && kind(grandkids[1]) === K";"
        skip_kid!(b, kid)
    elseif changed
        emit!(b, kid)
    else
        accept!(b, kid)
    end
    return
end

function spaces_in_listlike(ctx::Context, node::Node)
    if !(
            kind(node) in KSet"tuple parameters curly braces bracescat vect ref parens" ||
                (kind(node) in KSet"call dotcall" && !is_any_op_call(node)) ||
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

    peek(i) = i < length(kids) ? kind(kids[i + 1]) : nothing

    ws = ws_node(1)
    comma = Node(JuliaSyntax.SyntaxHead(K",", JuliaSyntax.TRIVIA_FLAG), 1)

    # Find the opening and closing leafs
    implicit_tuple = false
    if kind(node) in KSet"tuple call dotcall parens macrocall" || is_paren_block(node)
        opening_leaf_idx = findfirst(x -> kind(x) === K"(", kids)
        if opening_leaf_idx === nothing
            # Implicit tuple without (), for example arguments in a do-block
            implicit_tuple = true
            @assert kind(node) === K"tuple"
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

    n_items = count(is_list_item, @view(kids[(opening_leaf_idx + 1):(closing_leaf_idx - 1)]))
    first_item_idx = findnext(is_list_item, kids, opening_leaf_idx + 1)
    if first_item_idx !== nothing && first_item_idx >= closing_leaf_idx
        first_item_idx = nothing
    end
    last_item_idx = findprev(is_list_item, kids, closing_leaf_idx - 1)
    if last_item_idx !== nothing && last_item_idx <= opening_leaf_idx
        last_item_idx = nothing
    end

    # Pre-scan for `# runic: off` / `# runic: on` toggle pairs among the kids. Kids inside
    # such a region are "frozen": they are accepted verbatim instead of having their
    # whitespace rewritten. Note that the state machine below still transitions as usual
    # for frozen kids, only the byte mutations are suppressed.
    toggle_ranges = find_format_toggle_ranges(ctx, kids)
    is_frozen(i) = toggle_ranges !== nothing && any(r -> i in r, toggle_ranges)

    # Multiline lists require leading and trailing newline
    multiline = is_multiline_between_idxs(ctx, node, opening_leaf_idx, closing_leaf_idx)

    is_named_tuple = kind(node) === K"tuple" && n_items == 1 && kind(kids[first_item_idx]) === K"parameters"

    require_trailing_comma, allow_trailing_comma, allow_trailing_semi = trailing_comma_policy(
        ctx, node, kids, n_items, first_item_idx, last_item_idx, closing_leaf_idx,
        implicit_tuple, multiline, is_named_tuple,
        last_item_idx !== nothing && is_frozen(last_item_idx),
    )

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
        :expect_space
    elseif n_items > 0
        :expect_item
    else
        :expect_closing
    end

    b = NodeBuilder(ctx, node)

    # Accept kids up until the opening leaf
    for i in 1:opening_leaf_idx
        accept!(b, kids[i])
    end

    # Loop over the kids between the opening/closing tokens.
    for i in (opening_leaf_idx + 1):(closing_leaf_idx - 1)
        kid′ = kids[i]
        # Kids inside a `# runic: off` region are accepted verbatim
        frozen = is_frozen(i)
        if state === :expect_item
            if kind(kid′) === K"Whitespace" && peek(i) !== K"Comment" && !frozen
                # Delete whitespace unless followed by a comment
                skip_kid!(b, kid′)
            elseif kind(kid′) === K"NewlineWs" || kind(kid′) === K"Whitespace"
                # Newline here can happen if this kid is just after the opening leaf or if
                # there is an empty line between items. Whitespace reaching this branch is
                # either followed by a comment or frozen. No state change.
                accept!(b, kid′)
            elseif kind(kid′) === K"Comment"
                accept!(b, kid′)
                state = :expect_space # To ensure space after the comment
            else
                # This is an item (probably?).
                @assert !JuliaSyntax.is_whitespace(first_leaf(kid′))
                @assert !JuliaSyntax.is_whitespace(last_leaf(kid′))
                if kind(kid′) === K"parameters"
                    handle_parameters_kid!(
                        b, kid′,
                        !frozen && require_trailing_comma && i == last_item_idx,
                        !frozen && allow_trailing_comma && i == last_item_idx,
                        !frozen && !require_trailing_comma && !is_named_tuple,
                    )
                else
                    accept!(b, kid′)
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
                    accept!(b, kid′)
                else
                    unreachable()
                end
                # Transition to the next state
                state = before_last_item ? (:expect_space) : (:expect_closing)
            elseif kind(kid′) === K"Whitespace" && peek(i) !== K"Comment" && !frozen
                # Delete space (unless followed by a comment) and hope next is still comma
                # (no state change)
                skip_kid!(b, kid′)
            elseif kind(kid′) === K"NewlineWs" ||
                    kind(kid′) === K"Whitespace" ||
                    kind(kid′) === K"Comment"
                # This branch can be reached if:
                #  - we have passed the last item and there is no trailing comma
                #  - there is a comma coming but it is on the next line (weird)
                #  - there is a comment with no space before it
                next_non_ws_idx = findnext(
                    !JuliaSyntax.is_whitespace, @view(kids[1:(closing_leaf_idx - 1)]), i + 1
                )
                next_kind = next_non_ws_idx === nothing ? nothing : kind(kids[next_non_ws_idx])
                # Insert a comma if there isn't one coming (never inside a frozen region,
                # the comma would end up inside the `# runic: on` comment)
                if trailing && next_kind !== K"," && !frozen
                    @assert require_trailing_comma
                    emit!(b, comma, ",", 0)
                    state = :expect_closing
                end
                # Accept the newline
                accept!(b, kid′)
            elseif kind(kid′) === K"parameters"
                # Note that some of these are not valid Julia syntax but still parse
                @assert kind(node) in KSet"call dotcall macrocall curly tuple vect ref braces"
                @assert !JuliaSyntax.is_whitespace(first_leaf(kid′))
                handle_parameters_kid!(
                    b, kid′,
                    !frozen && require_trailing_comma,
                    !frozen && allow_trailing_comma,
                    !frozen && !require_trailing_comma,
                )
                # K"parameters" is always the last item in valid Julia code but we need to
                # handle all expressions that parse and there might be multiple
                # K"parameters"...
                state = i == last_item_idx ? (:expect_closing) : (:expect_item)
            else
                unreachable()
            end
        elseif state === :expect_space
            if kind(kid′) === K"Whitespace" &&
                    (span(kid′) == 1 || peek(i) === K"Comment" || frozen)
                # Whitespace with correct span
                # Whitespace before a comment
                # Frozen whitespace, kept as is
                accept!(b, kid′)
                state = :expect_item
            elseif kind(kid′) === K"Whitespace"
                # Wrong span, replace it
                emit!(b, ws, " ", span(kid′))
                # Transition to the next state
                state = :expect_item
            elseif kind(kid′) === K"NewlineWs"
                # NewlineWs are accepted and accounts for a space
                accept!(b, kid′)
                state = :expect_item
            elseif frozen
                # Item inside a frozen region: accept it without inserting a space
                @assert !(kind(kid′) in KSet", ;")
                accept!(b, kid′)
                state = state_after_item(i, last_item_idx, require_trailing_comma)
            else
                # Probably a list item, insert a space before it
                @assert !(kind(kid′) in KSet", ;")
                @assert !JuliaSyntax.is_whitespace(first_leaf(kid′))
                emit!(b, ws, " ", 0)
                accept!(b, kid′)
                # Here we inserted a space and consumed the next item, moving on to comma
                state = state_after_item(i, last_item_idx, require_trailing_comma)
            end
        else
            @assert state === :expect_closing
            if !frozen && (
                    (kind(kid′) === K"," && !allow_trailing_comma) ||
                        (kind(kid′) === K";" && !allow_trailing_semi) ||
                        (kind(kid′) === K"Whitespace" && peek(i) !== K"Comment")
                )
                # Trailing comma (when not wanted) and space not followed by a comment are
                # removed
                skip_kid!(b, kid′)
            elseif frozen
                # Frozen region: accept the kid verbatim, but keep the trailing
                # comma/semicolon bookkeeping in sync
                if kind(kid′) === K";"
                    allow_trailing_semi = n_items == 0
                elseif kind(kid′) === K","
                    allow_trailing_comma = false
                end
                accept!(b, kid′)
            elseif kind(node) === K"block" && kind(kid′) === K";" && allow_trailing_semi ||
                    (kind(kid′) === K"," && allow_trailing_comma) ||
                    (kind(kid′) === K"Whitespace" && peek(i) !== K"Comment")
                allow_trailing_semi = n_items == 0 # Only one semicolon allowed
                allow_trailing_comma = false # Just one please
                accept!(b, kid′)
            elseif kind(kid′) === K"NewlineWs" ||
                    (kind(kid′) === K"Whitespace" && peek(i) === K"Comment") ||
                    kind(kid′) === K"Comment"
                # Newlines, whitespace followed by comment, and comments are accepted.
                accept!(b, kid′)
            else
                unreachable()
            end
        end # if-state
    end
    if state !== :expect_closing
        if state === :expect_comma
            # K"parameters" should always handle the trailing comma and go to
            # :expect_closing directly
            @assert kind(kids[last_item_idx]) !== K"parameters"
            # Need to add a trailing comma if it is expected
            @assert require_trailing_comma
            emit!(b, comma, ",", 0)
            state = :expect_closing
        else
            unreachable()
        end
    end
    @assert state === :expect_closing
    # Accept kids after the closing leaf
    for i in closing_leaf_idx:length(kids)
        accept!(b, kids[i])
    end
    return finish!(b, node)
end

# This pass handles spaces around infix operator calls, comparison chains, and
# <: and >: operators.
function spaces_around_operators(ctx::Context, node::Node)
    if !(
            (is_infix_op_call(node) && !(infix_op_call_op(ctx, node) in KSet": ^")) ||
                (kind(node) in KSet"<: >:" && meta_nargs(node) == 3) ||
                (kind(node) === K"comparison" && !JuliaSyntax.is_trivia(node)) ||
                kind(node) === K"in" && !is_leaf(node)
        )
        return nothing
    end
    @assert kind(node) in KSet"call dotcall comparison <: >: in"
    n_leaves_per_x = kind(node) === K"dotcall" ? 2 : 1
    return spaces_around_x(ctx, node, n_leaves_per_x)
end

function spaces_around_assignments(ctx::Context, node::Node)
    if !(
            (is_assignment(node) && !is_leaf(node)) ||
                is_short_form_function_definition(node)
        )
        return nothing
    end
    # The operator spans multiple leaves for op= (`+` `=`), .= (`.` `=`), and
    # .op= (`.` `+` `=`) nodes
    n_leaves_per_x = 1
    if kind(node) in KSet"op= .op= .="
        n_leaves_per_x += kind(node) === K".op=" ? 2 : 1
    end
    return spaces_around_x(ctx, node, n_leaves_per_x)
end

function spaces_around_anonymous_function(ctx::Context, node::Node)
    if !(kind(node) === K"->" && !is_leaf(node))
        return nothing
    end
    return spaces_around_x(ctx, node)
end

function spaces_around_ternary(ctx::Context, node::Node)
    if !(kind(node) === K"?" && !is_leaf(node))
        return nothing
    end
    return spaces_around_x(ctx, node)
end

# Opposite of `spaces_around_x`: remove spaces around `x`
function no_spaces_around_x(ctx::Context, node::Node)
    @assert !is_leaf(node)
    # TODO: Can't handle NewlineWs and comments here right now
    if any(kind(c) in KSet"NewlineWs Comment" for c in verified_kids(node))
        return nothing
    end

    kids = verified_kids(node)
    b = NodeBuilder(ctx, node)
    for (i, kid) in pairs(kids)
        if kind(kid) === K"Whitespace"
            # Leading and trailing whitespace should not be dropped but normalization
            # should make sure these never exist.
            @assert 1 < i < length(kids)
            skip_kid!(b, kid)
        else
            @assert !JuliaSyntax.is_whitespace(kid) # Filtered out above
            accept!(b, kid)
        end
    end
    return finish!(b, node)
end

function spaces_in_export_public(ctx::Context, node::Node)
    is_leaf(node) && return nothing
    if !(kind(node) in KSet"export public" || is_global_local_list(node))
        return nothing
    end
    kids = verified_kids(node)
    spacenode = ws_node(1)

    b = NodeBuilder(ctx, node)
    @assert is_leaf(kids[1]) && kind(kids[1]) in KSet"export public global local"
    accept!(b, kids[1])

    # space -> identifier -> comma
    state = :expect_space
    i = 2
    while i <= length(kids)
        kid = kids[i]
        if state === :expect_space
            state = :expect_identifier
            if kind(kid) === K"NewlineWs" || (kind(kid) === K"Whitespace" && span(kid) == 1)
                accept!(b, kid)
            elseif kind(kid) === K"Whitespace"
                emit!(b, spacenode, " ", span(kid))
            elseif kind(kid) === K"Comment"
                accept!(b, kid)
                state = :expect_space
            else
                @assert kind(first_leaf(kid)) !== K"Whitespace"
                # Insert a space
                emit!(b, spacenode, " ", 0)
                continue # Skip increment of i
            end
        elseif state === :expect_identifier
            state = :expect_comma
            if kind(kid) in KSet"Identifier macro_name $ var" || JuliaSyntax.is_operator(kid)
                accept!(b, kid)
                if kind(kid) === K"$"
                    @assert findlast(x -> x in KSet"quote macrocall", ctx.lineage_kinds) !== nothing
                end
            elseif kind(kid) === K"parens"
                # Parenthesized symbol gives a warning in JuliaSyntax but is allowed
                # TODO: Runic could remove them...
                @assert kind(first_leaf(kid)) !== K"Whitespace"
                accept!(b, kid)
            elseif kind(kid) in KSet"Comment NewlineWs"
                accept!(b, kid)
                state = kind(kid) === K"Comment" ? (:expect_space) : (:expect_identifier)
            else
                unreachable()
            end
        else
            @assert state === :expect_comma
            state = :expect_space
            if kind(kid) === K","
                accept!(b, kid)
            elseif kind(kid) === K"Whitespace"
                # Drop this node
                skip_kid!(b, kid)
                state = :expect_comma
            else
                unreachable()
            end
        end
        i += 1
    end
    return finish!(b, node)
end

function spaces_in_let(ctx::Context, node::Node)
    if kind(node) !== K"let" || is_leaf(node)
        return nothing
    end
    let_kids = verified_kids(node)
    let_leaf = let_kids[1]
    @assert kind(let_leaf) === K"let" && is_leaf(let_leaf)
    vars_idx = 2
    vars_node = let_kids[vars_idx]
    @assert !is_leaf(vars_node) && kind(vars_node) === K"block"
    kids = verified_kids(vars_node)
    if length(kids) == 0
        @assert span(vars_node) == 0
        return nothing
    end
    pos = position(ctx.fmt_io)
    accept_node!(ctx, let_leaf)
    # The builder rebuilds the vars block; the stream is now at its start
    b = NodeBuilder(ctx, vars_node)
    # First node *must* be a space (?)
    @assert kind(kids[1]) === K"Whitespace"
    # Second node must be a variable or assignment (at least non-whitespace)
    idx = findnext(x -> !JuliaSyntax.is_whitespace(x), kids, 2)
    for i in 1:idx
        accept!(b, kids[i])
    end
    # Now we expect comma -> space -> variable -> comma
    state = :expect_comma
    idx += 1
    space = ws_node(1)
    while idx <= length(kids)
        kid′ = kids[idx]
        if state === :expect_comma
            state = :expect_space
            if kind(kid′) === K","
                accept!(b, kid′)
            elseif kind(kid′) === K"Comment" || kmatch(kids, KSet"Whitespace Comment", idx)
                state = :expect_comma
                accept!(b, kid′)
            elseif kind(kid′) === K"Whitespace"
                @assert !kmatch(kids, KSet"Comment", idx + 1)
                # Delete this space and keep looking for comma
                state = :expect_comma
                skip_kid!(b, kid′)
            else
                unreachable()
            end
        elseif state === :expect_space
            state = :expect_var
            if kind(kid′) === K"NewlineWs" ||
                    (kind(kid′) === K"Whitespace" && span(kid′) == 1) ||
                    kmatch(kids, KSet"Whitespace Comment", idx)
                accept!(b, kid′)
            elseif kind(kid′) === K"Comment"
                accept!(b, kid′)
                state = :expect_space
            elseif kind(kid′) === K"Whitespace"
                emit!(b, space, " ", span(kid′))
            else
                @assert !JuliaSyntax.is_whitespace(kid′)
                emit!(b, space, " ", 0)
                continue # Skip the idx increment
            end
        elseif state === :expect_var
            state = :expect_comma
            if kind(kid′) in KSet"Comment NewlineWs"
                accept!(b, kid′)
                state = kind(kid′) === K"Comment" ? (:expect_space) : (:expect_var)
            else
                @assert !JuliaSyntax.is_whitespace(kid′)
                accept!(b, kid′)
            end
        else
            unreachable()
        end
        idx += 1
    end
    vars_node′ = finish!(b, vars_node)
    seek(ctx.fmt_io, pos)
    if vars_node′ === nothing
        return nothing
    end
    let_kids′ = copy(let_kids)
    let_kids′[vars_idx] = vars_node′
    return make_node(node, let_kids′)
end

# Used in `spaces_in_import_using` and `format_as`
function format_importpath(ctx::Context, node::Node)
    @assert kind(node) === K"importpath"
    @assert !JuliaSyntax.is_whitespace(first_leaf(node))
    return nothing
end

# Used in `spaces_in_import_using`
function format_as(ctx::Context, node::Node)
    @assert kind(node) === K"as"
    kids = verified_kids(node)
    # Comments and newlines may occur on either side of `as`. Leave these layouts to the
    # generic comment, whitespace, and indentation passes instead of assuming the compact
    # five-node layout below.
    if any(x -> kind(x) in KSet"Comment NewlineWs", kids)
        return nothing
    end
    spacebar = ws_node(1)
    b = NodeBuilder(ctx, node)
    # First the importpath (LHS of the `as`)
    idx = 1
    kid = kids[idx]
    @assert kind(kid) === K"importpath"
    @assert format_importpath(ctx, kid) === nothing
    accept!(b, kid)
    # space before `as`
    idx += 1
    kid = kids[idx]
    @assert kind(kid) === K"Whitespace"
    if span(kid) == 1
        accept!(b, kid)
    else
        emit!(b, spacebar, " ", span(kid))
    end
    # `as`
    idx += 1
    kid = kids[idx]
    @assert kind(kid) === K"as"
    accept!(b, kid)
    # space after `as`
    idx += 1
    kid = kids[idx]
    @assert kind(kid) === K"Whitespace"
    if span(kid) == 1
        accept!(b, kid)
    else
        emit!(b, spacebar, " ", span(kid))
    end
    # Alias-identifier (RHS of the `as`)
    idx += 1
    kid = kids[idx]
    @assert kind(kid) in KSet"Identifier $ macro_name"
    if !is_leaf(kid)
        @assert kind(first_leaf(kid)) !== K"Whitespace"
    end
    if kind(kid) === K"$"
        @assert findlast(x -> x in KSet"quote macrocall", ctx.lineage_kinds) !== nothing
    end
    accept!(b, kid)
    return finish!(b, node)
end

# TODO: This method is very similar to `spaces_in_export_public`
function spaces_in_import_using(ctx::Context, node::Node)
    if !(kind(node) in KSet"import using" && !is_leaf(node))
        return nothing
    end
    kids = verified_kids(node)

    colon_list = kind(first(kids)) === K":"
    if colon_list
        colon_node = first(kids)
        @assert length(kids) == 1
        kids = verified_kids(colon_node)
    end

    # The builder rebuilds either the node itself or, for colon lists, the colon node
    # (which starts at the same stream position since it is the only kid).
    b = NodeBuilder(ctx, kids)

    @assert kind(kids[1]) in KSet"import using"
    accept!(b, kids[1])

    spacebar = ws_node(1)

    state = :expect_space
    i = 2
    while i <= length(kids)
        kid = kids[i]
        if state === :expect_item
            state = :expect_comma
            if kind(kid) === K"importpath"
                # format_importpath only validates assumptions and never changes the node
                @assert format_importpath(ctx, kid) === nothing
                accept!(b, kid)
            elseif kind(kid) === K"as"
                kid′ = format_as(ctx, kid)
                if kid′ === nothing
                    accept!(b, kid)
                else
                    emit!(b, kid′)
                end
            elseif kind(kid) in KSet"Comment NewlineWs"
                accept!(b, kid)
                state = kind(kid) === K"Comment" ? (:expect_space) : (:expect_item)
            else
                unreachable()
            end
        elseif state === :expect_comma
            state = :expect_space
            if kind(kid) === K"Whitespace"
                # Drop this node
                skip_kid!(b, kid)
                state = :expect_comma
            else
                @assert kind(kid) in KSet": ,"
                accept!(b, kid)
            end
        else
            @assert state === :expect_space
            state = :expect_item
            if kind(kid) === K"NewlineWs" || (kind(kid) === K"Whitespace" && span(kid) == 1)
                # Newline or whitespace with correct span
                accept!(b, kid)
            elseif kind(kid) === K"Whitespace"
                # Whitespace with incorrect span; replace with a single space
                emit!(b, spacebar, " ", span(kid))
            elseif kind(kid) === K"Comment"
                accept!(b, kid)
                state = :expect_space
            else
                # No whitespace, insert
                @assert kind(kid) in KSet"Identifier importpath as"
                @assert !JuliaSyntax.is_whitespace(first_leaf(kid))
                emit!(b, spacebar, " ", 0)
                continue # Skip increment of i
            end
        end
        i += 1
    end
    if colon_list
        colon_node′ = finish!(b, colon_node)
        colon_node′ === nothing && return nothing
        return make_node(node, [colon_node′])
    else
        return finish!(b, node)
    end
end

# no spaces around `:`, `^`, and `::`
function no_spaces_around_colon_etc(ctx::Context, node::Node)
    if !(
            (is_infix_op_call(node) && infix_op_call_op(ctx, node) in KSet": ^") ||
                (kind(node) === K"::" && !is_leaf(node)) ||
                (kind(node) in KSet"<: >:" && meta_nargs(node) == 2)
        )
        return nothing
    end
    @assert kind(node) in KSet"call :: <: >:"
    return no_spaces_around_x(ctx, node)
end

function space_before_do(ctx::Context, node::Node)
    @assert kind(node) in KSet"call dotcall" && !is_leaf(node)
    kids = verified_kids(node)
    last_idx = length(kids)
    if !(last_idx >= 2 && kind(kids[last_idx]) === K"do")
        return nothing
    end
    ws_idx = last_idx - 1
    if kind(kids[ws_idx]) === K"Whitespace" && span(kids[ws_idx]) == 1
        return nothing
    end
    b = NodeBuilder(ctx, node)
    if kind(kids[ws_idx]) === K"Whitespace"
        # Replace the multi-space whitespace with a single space
        for j in 1:(ws_idx - 1)
            accept!(b, kids[j])
        end
        emit!(b, ws_node(1), " ", span(kids[ws_idx]))
    else
        # No whitespace before the do, insert one
        for j in 1:ws_idx
            accept!(b, kids[j])
        end
        emit!(b, ws_node(1), " ", 0)
    end
    accept!(b, kids[last_idx])
    return finish!(b, node)
end

function space_after_for(ctx::Context, node::Node)
    @assert kind(node) === K"for" && !is_leaf(node)
    p = position(ctx.fmt_io)
    kids = verified_kids(node)
    for_leaf = kids[1]
    @assert kind(for_leaf) === K"for" && is_leaf(for_leaf)
    ws = ws_node(1)
    if kind(kids[2]) in KSet"Whitespace NewlineWs"
        # In some tree configurations the whitespace after `for` is already a
        # direct child of the K"for" node (e.g. after another pass has
        # restructured it). For a plain K"Whitespace" normalise the span if
        # needed; for K"NewlineWs" leave it alone.
        ws_kid = kids[2]
        if kind(ws_kid) === K"NewlineWs" || span(ws_kid) == 1
            seek(ctx.fmt_io, p)
            return nothing
        end
        accept_node!(ctx, for_leaf)
        replace_bytes!(ctx, " ", span(ws_kid))
        kids′ = copy(kids)
        kids′[2] = ws
        seek(ctx.fmt_io, p)
        return make_node(node, kids′)
    end
    # In JuliaSyntax v1 the space after `for` is the first child of the first
    # in/∈ subnode inside the iteration node (kids[2]).
    iter_idx = 2
    iter_node = kids[iter_idx]
    @assert kind(iter_node) === K"iteration"
    iter_kids = verified_kids(iter_node)
    first_in = iter_kids[1]
    in_kids = verified_kids(first_in)
    ws_kid = in_kids[1]
    if kind(ws_kid) !== K"Whitespace" || span(ws_kid) == 1
        seek(ctx.fmt_io, p)
        return nothing
    end
    accept_node!(ctx, for_leaf)
    replace_bytes!(ctx, " ", span(ws_kid))
    in_kids′ = copy(in_kids)
    in_kids′[1] = ws
    first_in′ = make_node(first_in, in_kids′)
    iter_kids′ = copy(iter_kids)
    iter_kids′[1] = first_in′
    iter_node′ = make_node(iter_node, iter_kids′)
    kids′ = copy(kids)
    kids′[iter_idx] = iter_node′
    seek(ctx.fmt_io, p)
    return make_node(node, kids′)
end

function space_after_let(ctx, node)
    @assert kind(node) === K"let" && !is_leaf(node)
    p = position(ctx.fmt_io)
    kids = verified_kids(node)
    let_node = kids[1]
    @assert kind(let_node) === K"let"
    accept_node!(ctx, let_node)
    vars_idx = 2
    vars_node = kids[vars_idx]
    @assert kind(vars_node) === K"block"
    vars_kids = verified_kids(vars_node)
    if length(vars_kids) == 0
        @assert span(vars_node) == 0
        seek(ctx.fmt_io, p)
        # Empty block, but where are spaces and comments?
        return nothing
    end
    # First node *must* be a space (?)
    vars_kid = vars_kids[1]
    @assert kind(vars_kid) === K"Whitespace"
    if span(vars_kid) == 1
        seek(ctx.fmt_io, p)
        return nothing
    else
        replace_bytes!(ctx, " ", span(vars_kid))
        ws = ws_node(1)
        vars_kids′ = copy(vars_kids)
        vars_kids′[1] = ws
        vars_node′ = make_node(vars_node, vars_kids′)
        kids′ = copy(kids)
        kids′[vars_idx] = vars_node′
        seek(ctx.fmt_io, p)
        return make_node(node, kids′)
    end
end

# Single space around keywords:
# Both sides of: `where`, `do` (if followed by arguments)
# Right hand side of: `mutable`, `struct`, `abstract`, `primitive`, `type`, `function` (if
# named function), `if`, `elseif`, `catch` and `return` (if followed by something), local,
# global, const
function spaces_around_keywords(ctx::Context, node::Node)
    is_leaf(node) && return nothing
    if kind(node) === K"for"
        return space_after_for(ctx, node)
    end
    if kind(node) === K"let"
        return space_after_let(ctx, node)
    end
    if kind(node) in KSet"call dotcall"
        return space_before_do(ctx, node)
    end
    keyword_set = KSet"""
    where do mutable struct abstract primitive type function if elseif catch while return
    local global const module baremodule
    """
    if !(kind(node) in keyword_set)
        return nothing
    end
    kids = verified_kids(node)
    kids′ = kids
    any_changes = false
    pos = position(ctx.fmt_io)
    ws = ws_node(1)

    peek_kinds = KSet"where"
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
                    tupleidx = findnext(x -> kind(x) === K"tuple", kids, i + 1)::Int
                    tuple = kids[tupleidx]
                    if !any(x -> !(JuliaSyntax.is_whitespace(x) || kind(x) === K";"), verified_kids(tuple))
                        state = :closing
                    end
                end
                # `catch` should only be followed by space if the error is caught in a var
                if kind(node) === K"catch"
                    nkid = kids[i + 1]
                    if kind(nkid) === K"Placeholder" && span(nkid) == 0
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
                    @assert kind(node) in KSet"where local global const"
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
            else
                @assert kind(first_leaf(kid)) !== K"Whitespace"
                # Reachable in e.g. `T where{T}`, `if(`, ... insert space
                @assert kind(node) in KSet"where if elseif while do function return local global module baremodule"
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

# Replace `=` and `∈` with `in` in for-loops and generators
function for_loop_use_in(ctx::Context, node::Node)
    if !(kind(node) === K"in" && !is_leaf(node))
        return nothing
    end
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

function braces_around_where_rhs(ctx::Context, node::Node)
    if !(kind(node) === K"where" && !is_leaf(node))
        return nothing
    end
    kids = verified_kids(node)
    where_idx = findfirst(x -> is_leaf(x) && kind(x) === K"where", kids)::Int
    rhs_idx = findnext(!JuliaSyntax.is_whitespace, kids, where_idx + 1)::Int
    @assert rhs_idx == lastindex(kids)
    rhs = kids[rhs_idx]
    if kind(rhs) === K"braces"
        return nothing
    end
    # Wrap the rhs in a braces node
    b = NodeBuilder(ctx, node)
    for i in 1:(rhs_idx - 1)
        accept!(b, kids[i])
    end
    opening_brace = Node(JuliaSyntax.SyntaxHead(K"{", 0), 1)
    closing_brace = Node(JuliaSyntax.SyntaxHead(K"}", 0), 1)
    rhs′ = Node(
        JuliaSyntax.SyntaxHead(K"braces", 0),
        [opening_brace, rhs, closing_brace]
    )
    # Write the braces: insert "{" before and "}" after the rhs bytes
    let p = position(ctx.fmt_io)
        replace_bytes!(ctx, "{", 0)
        seek(ctx.fmt_io, p + 1 + span(rhs))
        replace_bytes!(ctx, "}", 0)
        seek(ctx.fmt_io, p)
    end
    emit!(b, rhs′)
    return finish!(b, node)
end

function parens_around_op_calls_in_colon(ctx::Context, node::Node)
    if !(is_infix_op_call(node) && infix_op_call_op(ctx, node) === K":")
        return nothing
    end
    kids = verified_kids(node)
    b = NodeBuilder(ctx, node)
    for kid in kids
        if is_infix_op_call(kid)
            grandkids = verified_kids(kid)
            @assert findfirst(!JuliaSyntax.is_whitespace, grandkids) == firstindex(grandkids)
            @assert findlast(!JuliaSyntax.is_whitespace, grandkids) == lastindex(grandkids)
            # Create the parens node and write the paren bytes around the kid
            opening_paren = Node(JuliaSyntax.SyntaxHead(K"(", 0), 1)
            closing_paren = Node(JuliaSyntax.SyntaxHead(K")", 0), 1)
            parens = Node(JuliaSyntax.SyntaxHead(K"parens", 0), [opening_paren, kid, closing_paren])
            let p = position(ctx.fmt_io)
                replace_bytes!(ctx, "(", 0)
                seek(ctx.fmt_io, p + 1 + span(kid))
                replace_bytes!(ctx, ")", 0)
                seek(ctx.fmt_io, p)
            end
            emit!(b, parens)
        else
            accept!(b, kid)
        end
    end
    return finish!(b, node)
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
        push!(kids′, nlws_node(1))
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
                (kind(kids[idx + 1]) === kind(kids[idx + 2]) === kind(kids[idx + 3]) === K"NewlineWs")
            # Delete the first of the four newline nodes. The caller re-runs this rule so
            # any remaining excess newlines are removed in later passes.
            b = NodeBuilder(ctx, node)
            for i in 1:(idx - 1)
                accept!(b, kids[i])
            end
            skip_kid!(b, kids[idx])
            for i in (idx + 1):length(kids)
                accept!(b, kids[i])
            end
            return finish!(b, node)
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
    node′ = Node(head(node), spn′, nothing, node.tags)
    return node′
end

# This function tags the `function`/`macro` and `end` keywords as well as the trailing
# newline of the function/macro body.
function indent_function_or_macro(ctx::Context, node::Node)
    kids = verified_kids(node)
    # First node is the function/macro keyword
    func_idx = 1
    @assert is_leaf(kids[func_idx]) && kind(kids[func_idx]) in KSet"function macro"
    any_kid_changed = tag_kid!(kids, func_idx, TAG_INDENT)
    # The signature is the next non-whitespace node. It is a (call/where/::) for standard
    # method definitions but just an Identifier for cases like `function f end`.
    sig_idx = findnext(x -> !JuliaSyntax.is_whitespace(x), kids, func_idx + 1)::Int
    sig_node = kids[sig_idx]
    # Identifier for regular names but "not function call" for empty functions with Unicode
    # symbols??
    if kind(sig_node) === K"Identifier" || !(kind(sig_node) in KSet"call where :: tuple parens macrocall")
        # Empty function definition like `function f end`.
        # TODO: Make sure the spaces around are correct
        end_idx = findnext(x -> kind(x) === K"end", kids, sig_idx + 1)::Int
        @assert is_leaf(kids[end_idx]) && kind(kids[end_idx]) === K"end"
        any_kid_changed |= tag_kid!(kids, end_idx, TAG_DEDENT)
        return any_kid_changed ? make_node(node, kids) : nothing
    end
    # K"tuple" when this is an anonymous function
    # K"macrocall" when this is `function @main(args)`
    @assert !is_leaf(sig_node) && kind(sig_node) in KSet"call where :: tuple parens macrocall"
    # Next node is the function/macro body block.
    block_idx = sig_idx + 1
    any_kid_changed |= apply_at_kid!(indent_block, ctx, kids, block_idx)
    # Last node is the closing end keyword
    end_idx = findnext(x -> kind(x) === K"end", kids, block_idx + 1)::Int
    @assert is_leaf(kids[end_idx]) && kind(kids[end_idx]) === K"end"
    any_kid_changed |= tag_kid!(kids, end_idx, TAG_DEDENT)
    @assert verified_kids(node) === kids
    return any_kid_changed ? make_node(node, kids) : nothing
end

# Soft-indentation between the variables
function indent_let_varblock(ctx::Context, node::Node)
    @assert kind(node) === K"block" && !is_leaf(node)
    kids = verified_kids(node)
    changed = false
    if length(kids) == 0
        @assert span(node) == 0
        # Empty block, but where are spaces and comments?
        return nothing
    end
    # First node *must* be a space (?)
    i = 1
    @assert kind(kids[i]) === K"Whitespace"
    i = findnext(x -> !JuliaSyntax.is_whitespace(x), kids, i + 1)
    i === nothing && return nothing
    @assert kind(kids[i]) in KSet"Identifier = $ macrocall function" # This is a bit unnecessary
    while (i = findnext(x -> kind(x) === K"NewlineWs", kids, i + 1); i !== nothing)
        changed |= tag_kid!(kids, i, TAG_LINE_CONT)
    end
    return changed ? make_node(node, kids) : nothing
end

function indent_let(ctx::Context, node::Node)
    kids = verified_kids(node)
    # First node is the let keyword
    let_idx = 1
    @assert is_leaf(kids[let_idx]) && kind(kids[let_idx]) === K"let"
    any_kid_changed = tag_kid!(kids, let_idx, TAG_INDENT)
    # Second node is the variables block
    vars_idx = 2
    vars_node = kids[vars_idx]
    @assert !is_leaf(vars_node) && kind(vars_node) === K"block"
    if span(vars_node) > 0 && length(verified_kids(vars_node)) > 0
        @assert kind(last_leaf(vars_node)) !== K"NewlineWs"
    end
    any_kid_changed |= apply_at_kid!(indent_let_varblock, ctx, kids, vars_idx)
    # Next node is the body block.
    block_idx = findnext(x -> kind(x) === K"block", kids, vars_idx + 1)::Int
    @assert !is_leaf(kids[block_idx]) && kind(kids[block_idx]) === K"block"
    any_kid_changed |= apply_at_kid!(indent_block, ctx, kids, block_idx)
    # Look for the end node
    end_idx = findnext(x -> kind(x) === K"end", kids, block_idx + 1)::Int
    @assert is_leaf(kids[end_idx]) && kind(kids[end_idx]) === K"end"
    any_kid_changed |= tag_kid!(kids, end_idx, TAG_DEDENT)
    @assert verified_kids(node) === kids
    return any_kid_changed ? make_node(node, kids) : nothing
end

function indent_begin(ctx::Context, node::Node, block_kind = K"begin")
    @assert kind(node) === K"block"
    # Note: indent_block may advance the stream even when it returns nothing (for
    # begin/quote blocks it accepts the keyword before bailing out on empty blocks) so
    # the position must be restored before returning.
    pos = position(ctx.fmt_io)
    node′ = indent_block(ctx, node)
    any_kid_changed = false
    if node′ !== nothing
        node = node′
        any_kid_changed = true
    end
    kids = verified_kids(node)
    # First node is the begin/quote keyword
    begin_idx = 1
    @assert is_leaf(kids[begin_idx]) && kind(kids[begin_idx]) === block_kind
    any_kid_changed |= tag_kid!(kids, begin_idx, TAG_INDENT)
    # Last node is the end keyword
    end_idx = findlast(x -> kind(x) === K"end", kids)::Int
    @assert end_idx == lastindex(kids) # ??
    @assert is_leaf(kids[end_idx]) && kind(kids[end_idx]) === K"end"
    # Tag the newline just before the end keyword as pre-dedent
    if kind(kids[end_idx - 1]) === K"NewlineWs"
        any_kid_changed |= tag_kid!(kids, end_idx - 1, TAG_PRE_DEDENT)
    end
    any_kid_changed |= tag_kid!(kids, end_idx, TAG_DEDENT)
    # Reset stream
    seek(ctx.fmt_io, pos)
    return any_kid_changed ? make_node(node, kids) : nothing
end

# Find the opening and closing delimiter leafs and defer to indent_listlike.
function indent_listlike_between(
        ctx::Context, node::Node, open_kind::JuliaSyntax.Kind, close_kind::JuliaSyntax.Kind
    )
    kids = verified_kids(node)
    open_idx = findfirst(x -> kind(x) === open_kind, kids)::Int
    close_idx = findnext(x -> kind(x) === close_kind, kids, open_idx + 1)::Int
    return indent_listlike(ctx, node, open_idx, close_idx)
end

# Shared skeleton for `<keyword> ... <block> ... end` nodes: tag the keyword with
# indent, indent the block, and tag the closing `end` with dedent.
function indent_keyword_block_end!(
        ctx::Context, node::Node, kw_idx::Int, block_idx::Int, end_idx::Int;
        do_indent::Bool = true
    )
    kids = verified_kids(node)
    @assert is_leaf(kids[kw_idx])
    @assert kind(kids[block_idx]) === K"block"
    @assert is_leaf(kids[end_idx]) && kind(kids[end_idx]) === K"end"
    changed = do_indent && tag_kid!(kids, kw_idx, TAG_INDENT)
    changed |= apply_at_kid!(
        (ctx, kid) -> indent_block(ctx, kid; do_indent = do_indent), ctx, kids, block_idx
    )
    changed |= do_indent && tag_kid!(kids, end_idx, TAG_DEDENT)
    return changed ? make_node(node, kids) : nothing
end

# This function ensures that the block start, and ends, with a newline, and make sure that
# the trailing newline is tagged with TAG_PRE_DEDENT.
function indent_block(
        ctx::Context, node::Node; allow_empty::Bool = true, do_indent::Bool = true
    )
    @assert kind(node) === K"block" && !is_leaf(node)
    @assert !JuliaSyntax.has_flags(node, JuliaSyntax.PARENS_FLAG)
    kids = verified_kids(node)
    kids′ = kids
    pos = position(ctx.fmt_io)
    any_kid_changed = false

    # begin-end and quote-end have their respective keywords inside the block. `off` is
    # the offset between indices of the block content and indices in the kids vector.
    is_begin_end = length(kids) > 2 && kind(kids[1]) in KSet"begin quote" &&
        kind(kids[end]) === K"end"
    off = is_begin_end ? 1 : 0
    inner = (1 + off):(length(kids) - off)
    if is_begin_end
        accept_node!(ctx, kids[1])
    end

    # If the block is empty and contain no newlines, and empty blocks are allowed, we just
    # return
    if allow_empty && findfirst(!JuliaSyntax.is_whitespace, @view(kids[inner])) === nothing &&
            findfirst(x -> kind(x) === K"NewlineWs", @view(kids[inner])) === nothing
        return nothing
    end

    # Ensure a NewlineWs node at the end of the block (otherwise the closing
    # `end/else/catch/...` is not on a separate line).
    trailing_idx = findlast(x -> kind(x) === K"NewlineWs", @view(kids[inner]))
    if trailing_idx === nothing || trailing_idx != length(inner)
        # Missing NewlineWs node, insert.
        kids′ = copy(kids)
        let p = position(ctx.fmt_io)
            for i in inner
                accept_node!(ctx, kids′[i])
            end
            insert_at = last(inner) + 1
            # If the previous node is a K"Whitespace" node we just overwrite it instead of
            # merging becuase this whitespace will end up as trailing/leading whitespace
            # anyway.
            if length(inner) > 0 && kind(kids′[last(inner)]) === K"Whitespace"
                spn = span(kids′[last(inner)])
                seek(ctx.fmt_io, position(ctx.fmt_io) - spn)
                replace_bytes!(ctx, "", spn)
                popat!(kids′, last(inner))
                insert_at -= 1
            end
            # Insert a NewlineWs node in the tree and stream
            replace_bytes!(ctx, "\n", 0)
            k = nlws_node(1)
            if do_indent
                k = add_tag(k, TAG_PRE_DEDENT)
            end
            insert!(kids′, insert_at, k)
            seek(ctx.fmt_io, p)
        end
        any_kid_changed = true
    elseif do_indent && !has_tag(kids[trailing_idx + off], TAG_PRE_DEDENT)
        kids′ = copy(kids)
        kids′[trailing_idx + off] = add_tag(kids′[trailing_idx + off], TAG_PRE_DEDENT)
        any_kid_changed = true
    end
    # The block content must now end with a NewlineWs node
    @assert findlast(
        x -> kind(x) === K"NewlineWs", @view(kids′[(1 + off):(length(kids′) - off)])
    ) == length(kids′) - 2 * off

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
    # The accepted leading trivia matches the grammar
    # `[Whitespace] [;] [Whitespace] [Comment] NewlineWs`. Scan past any trivia; the
    # newline must come directly after it and a missing newline is inserted at the scan
    # position.
    i = 1 + off
    if kmatch(kids′, KSet"Whitespace ;", i)
        i += 2
    elseif kmatch(kids′, KSet";", i)
        i += 1
    end
    if kmatch(kids′, KSet"Whitespace Comment", i)
        i += 2
    elseif kmatch(kids′, KSet"Comment", i)
        i += 1
    end
    if !kmatch(kids′, KSet"NewlineWs", i)
        if kids′ === kids
            kids′ = copy(kids)
        end
        # If the node at the insertion point is a Whitespace we just overwrite it with a
        # `\n    ` node.
        wsspn = 0
        if kind(kids′[i]) === K"Whitespace"
            wsspn = span(popat!(kids′, i))
        end
        # If we end up in this code path we are most likely splitting a single line block
        # into multiples lines. This means that we haven't yet updated the indent level for
        # the keyword just before this block so in most cases we save a roundtrip by
        # increasing the indent level with 1 here.
        nl = "\n" * repeat(" ", 4 * (ctx.indent_level + 1))
        # Skip past the leading trivia
        for j in (1 + off):(i - 1)
            accept_node!(ctx, kids′[j])
        end
        replace_bytes!(ctx, nl, wsspn)
        insert!(kids′, i, nlws_node(sizeof(nl)))
        any_kid_changed = true
    end
    # Reset stream
    seek(ctx.fmt_io, pos)
    return any_kid_changed ? make_node(node, kids′) : nothing
end

function indent_catch(ctx::Context, node::Node)
    @assert kind(node) in KSet"catch else finally"
    kids = verified_kids(node)
    catch_idx = 1
    @assert is_leaf(kids[catch_idx]) && kind(kids[catch_idx]) in KSet"catch else finally"
    any_kid_changed = tag_kid!(kids, catch_idx, TAG_INDENT | TAG_DEDENT)
    # Skip over the catch-identifier (if any)
    block_idx = findnext(x -> kind(x) === K"block", kids, catch_idx + 1)::Int
    @assert kind(kids[block_idx]) === K"block"
    any_kid_changed |= apply_at_kid!(indent_block, ctx, kids, block_idx)
    return any_kid_changed ? make_node(node, kids) : nothing
end

function indent_try(ctx::Context, node::Node)
    @assert kind(node) in KSet"try"
    @assert !is_leaf(node)
    kids = verified_kids(node)
    # First node is `try`
    try_idx = 1
    @assert is_leaf(kids[try_idx]) && kind(kids[try_idx]) in KSet"try"
    any_kid_changed = tag_kid!(kids, try_idx, TAG_INDENT)
    # Second node is the try-block
    try_block_idx = findnext(!JuliaSyntax.is_whitespace, kids, try_idx + 1)::Int
    any_kid_changed |= apply_at_kid!(indent_block, ctx, kids, try_block_idx)
    # Loop over the catch/else/finally clauses. They can come in any order and are all
    # handled uniformly by indent_catch.
    clause_idx = findnext(x -> kind(x) in KSet"catch else finally", kids, try_block_idx + 1)
    @assert clause_idx !== nothing # At least one clause must exist
    last_clause_idx = clause_idx::Int
    while clause_idx !== nothing
        @assert !is_leaf(kids[clause_idx]) && kind(kids[clause_idx]) in KSet"catch else finally"
        any_kid_changed |= apply_at_kid!(indent_catch, ctx, kids, clause_idx)
        last_clause_idx = clause_idx
        clause_idx = findnext(x -> kind(x) in KSet"catch else finally", kids, clause_idx + 1)
    end
    # Check for end
    end_idx = findnext(x -> kind(x) === K"end", kids, last_clause_idx + 1)::Int
    @assert is_leaf(kids[end_idx]) && kind(kids[end_idx]) === K"end"
    any_kid_changed |= tag_kid!(kids, end_idx, TAG_DEDENT)
    @assert verified_kids(node) === kids
    return any_kid_changed ? make_node(node, kids) : nothing
end

function indent_if(ctx::Context, node::Node)
    @assert kind(node) in KSet"if elseif"
    @assert !is_leaf(node)
    kids = verified_kids(node)
    # First node is either `if` or `elseif` (when called recursively); an elseif both
    # dedents (it closes the previous branch) and indents (it opens its own).
    if_idx = 1
    @assert is_leaf(kids[if_idx]) && kind(kids[if_idx]) in KSet"if elseif"
    if_tag = kind(node) === K"elseif" ? (TAG_INDENT | TAG_DEDENT) : TAG_INDENT
    any_kid_changed = tag_kid!(kids, if_idx, if_tag)
    # Look for the condition node
    cond_idx = findnext(!JuliaSyntax.is_whitespace, kids, if_idx + 1)::Int
    @assert kind(last_leaf(kids[cond_idx])) !== K"NewlineWs"
    # Next node is the body block.
    block_idx = findnext(!JuliaSyntax.is_whitespace, kids, cond_idx + 1)::Int
    @assert block_idx == cond_idx + 1
    any_kid_changed |= apply_at_kid!(indent_block, ctx, kids, block_idx)
    # Check for elseif
    elseif_idx = findnext(x -> kind(x) === K"elseif", kids, block_idx + 1)
    if elseif_idx !== nothing
        @assert !is_leaf(kids[elseif_idx]) && kind(kids[elseif_idx]) === K"elseif"
        any_kid_changed |= apply_at_kid!(indent_if, ctx, kids, elseif_idx)
    end
    # Check for else
    else_idx = findnext(x -> kind(x) === K"else", kids, something(elseif_idx, block_idx) + 1)
    if else_idx !== nothing
        @assert is_leaf(kids[else_idx]) && kind(kids[else_idx]) === K"else"
        any_kid_changed |= tag_kid!(kids, else_idx, TAG_INDENT | TAG_DEDENT)
        else_block_idx = findnext(!JuliaSyntax.is_whitespace, kids, else_idx + 1)::Int
        @assert kind(kids[else_block_idx]) === K"block"
        any_kid_changed |= apply_at_kid!(indent_block, ctx, kids, else_block_idx)
    end
    # Check for end
    end_idx = findnext(x -> kind(x) === K"end", kids, something(else_idx, elseif_idx, block_idx) + 1)
    @assert (kind(node) === K"elseif") == (end_idx === nothing)
    if end_idx !== nothing
        @assert is_leaf(kids[end_idx]) && kind(kids[end_idx]) === K"end"
        any_kid_changed |= tag_kid!(kids, end_idx, TAG_DEDENT)
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
# Insert a newline after the semicolon of a K"parameters" node. This is used by
# indent_listlike when the parameters node is the first item after the opening token
# since we then want the newline after the semicolon instead of before it. The stream
# must be positioned at the start of `node` and is restored before returning. Return the
# new node, or `nothing` if there already is a newline after the semicolon. Bytes for
# the new newline are written to the stream.
function insert_newline_after_parameters_semi(ctx::Context, node::Node)
    @assert kind(node) === K"parameters"
    grandkids = verified_kids(node)
    semi_idx = findfirst(x -> kind(x) === K";", grandkids)::Int
    next_idx = semi_idx + 1
    next = grandkids[next_idx]
    kind(next) === K"NewlineWs" && return nothing
    # Write the newline directly after the semicolon
    let pos = position(ctx.fmt_io)
        for k in 1:semi_idx
            accept_node!(ctx, grandkids[k])
        end
        replace_bytes!(ctx, "\n", 0)
        seek(ctx.fmt_io, pos)
    end
    grandkids′ = copy(grandkids)
    if kind(next) === K"Whitespace"
        # Merge the newline with the whitespace
        grandkids′[next_idx] = nlws_node(1 + span(next))
    else
        # Insert a new newline node after the semicolon
        insert!(grandkids′, next_idx, nlws_node(1))
    end
    return make_node(node, grandkids′)
end

function indent_listlike(ctx::Context, node::Node, open_idx::Int, close_idx::Int)
    kids = verified_kids(node)
    # Bail early if there is just a single item
    open_idx == close_idx && return nothing
    # Check whether we expect leading/trailing newlines
    multiline = is_multiline_between_idxs(ctx, node, open_idx, close_idx)
    if !multiline
        # TODO: This should be fine? If there are no newlines it should be safe to just
        # don't indent anything in this node?
        return nothing
    end

    b = NodeBuilder(ctx, node)

    # Leave all initial kids the same
    for i in 1:(open_idx - 1)
        accept!(b, kids[i])
    end

    # Opening token indents
    kid = kids[open_idx]
    @assert is_leaf(kid)
    @assert kind(kid) !== K"NewlineWs"
    if has_tag(kid, TAG_INDENT)
        accept!(b, kid)
    else
        emit!(b, add_tag(kid, TAG_INDENT))
    end

    # Next we expect the leading newline
    kid = kids[open_idx + 1]
    idx_after_leading_nl = open_idx + 2
    if kind(kid) === K"NewlineWs"
        # Newline already in place
        accept!(b, kid)
    elseif kmatch(kids, KSet"Comment NewlineWs", open_idx + 1) ||
            kmatch(kids, KSet"Whitespace Comment NewlineWs", open_idx + 1)
        # Step over (whitespace +) comment up to and including the newline
        i = open_idx + 1
        while kind(kids[i]) !== K"NewlineWs"
            accept!(b, kids[i])
            i += 1
        end
        accept!(b, kids[i])
        idx_after_leading_nl = i + 1
    else
        # Need to insert a newline
        if kind(kid) === K"Whitespace"
            # Merge with the whitespace. It shouldn't matter if the newline is put before
            # or after the space. If put before the space will be handled by the indent
            # pass and if put after it will be handled by the trailing spaces pass.
            emit!(b, nlws_node(span(kid) + 1), "\n", 0)
        elseif kind(kid) === K"parameters"
            # For parameters we want the newline after the semicolon
            kid′ = insert_newline_after_parameters_semi(ctx, kid)
            if kid′ === nothing
                accept!(b, kid)
            else
                emit!(b, kid′)
            end
        else
            @assert kind(first_leaf(kid)) !== K"Whitespace"
            emit!(b, nlws_node(1), "\n", 0)
            accept!(b, kid)
        end
    end
    # Bring all kids between the opening and closing token to the new list
    for i in idx_after_leading_nl:(close_idx - 2)
        accept!(b, kids[i])
    end
    # Kid just before the closing token should be a newline and it should be tagged with
    # pre-dedent.
    if idx_after_leading_nl == close_idx
        # Just a single kid which should then have both leading and trailing newline.
        # Take it back from the builder to modify it again.
        kid = unemit!(b)
    else
        kid = kids[close_idx - 1]
    end
    if kind(kid) === K"NewlineWs" && has_tag(kid, TAG_PRE_DEDENT)
        # Newline with tag already in place
        accept!(b, kid)
    elseif kind(kid) === K"NewlineWs"
        # Newline without tag
        emit!(b, add_tag(kid, TAG_PRE_DEDENT))
    else
        @assert kind(last_leaf(kid)) !== K"NewlineWs"
        # Need to insert a newline. Note that we tag the new newline directly since it
        # is the responsibility of this function (otherwise there would just be an extra
        # repetitive call to add it anyway).
        if kind(kid) === K"Whitespace"
            # Merge with the whitespace
            emit!(b, add_tag(nlws_node(span(kid) + 1), TAG_PRE_DEDENT), "\n", 0)
        else
            @assert kind(last_leaf(kid)) !== K"Whitespace"
            # Note that this is a trailing newline and should be put after this item
            accept!(b, kid)
            emit!(b, add_tag(nlws_node(1), TAG_PRE_DEDENT), "\n", 0)
        end
    end
    # Closing token dedents
    kid = kids[close_idx]
    @assert is_leaf(kid)
    if has_tag(kid, TAG_DEDENT)
        accept!(b, kid)
    else
        emit!(b, add_tag(kid, TAG_DEDENT))
    end
    # Keep remaining kids. In JuliaSyntax v1, do-block calls are represented as K"call"
    # nodes with a trailing K"do" child after the closing paren, so close_idx may not be
    # the last index.
    if close_idx < lastindex(kids)
        @assert kind(node) in KSet"call dotcall" && kind(kids[end]) === K"do"
        for i in (close_idx + 1):lastindex(kids)
            accept!(b, kids[i])
        end
    end
    return finish!(b, node)
end

# Mark opening and closing parentheses, in a call or a tuple, with indent and dedent tags.
function indent_paren(ctx::Context, node::Node)
    @assert kind(node) in KSet"call dotcall tuple parens macrocall"
    return indent_listlike_between(ctx, node, K"(", K")")
end

function indent_braces(ctx::Context, node::Node)
    @assert kind(node) in KSet"curly braces bracescat"
    return indent_listlike_between(ctx, node, K"{", K"}")
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
    kw_idx = findfirst(x -> kind(x) in KSet"for while", kids)::Int
    # findlast because the condition can also be a block
    block_idx = findlast(x -> kind(x) === K"block", kids)::Int
    end_idx = findlast(x -> kind(x) === K"end", kids)::Int
    return indent_keyword_block_end!(ctx, node, kw_idx, block_idx, end_idx)
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
function indent_struct(ctx::Context, node::Node)
    @assert kind(node) === K"struct"
    kids = verified_kids(node)
    struct_idx = findfirst(!JuliaSyntax.is_whitespace, kids)::Int
    @assert kind(kids[struct_idx]) in KSet"mutable struct"
    block_idx = findnext(x -> kind(x) === K"block", kids, struct_idx + 1)::Int
    end_idx = findlast(x -> kind(x) === K"end", kids)::Int
    return indent_keyword_block_end!(ctx, node, struct_idx, block_idx, end_idx)
end

function indent_short_circuit(ctx::Context, node::Node)
    return indent_op_call(ctx, node)
end

function indent_triple_thing(ctx::Context, node::Node)
    @assert is_triple_thing(node)
    if is_triple_string(node)
        return has_tag(node, TAG_LINE_CONT) ? nothing : add_tag(node, TAG_LINE_CONT)
    end
    # Recurse into the string of a triple string macro (kid 2) or into the macro of a
    # juxtaposed triple string (kid 1)
    kids = verified_kids(node)
    if is_triple_string_macro(node)
        idx = 2
        @assert is_triple_string(kids[idx])
    else
        @assert kind(node) === K"juxtapose" && is_triple_string_macro(kids[1])
        idx = 1
    end
    kid′ = indent_triple_thing(ctx, kids[idx])
    kid′ === nothing && return nothing
    kids′ = copy(kids)
    kids′[idx] = kid′
    return make_node(node, kids′)
end

# TODO: This function can be used for more things than just indent_using I think. Perhaps
# with a max_depth parameter.
function continue_all_newlines(
        ctx::Context, node::Node; is_last::Bool = is_leaf(node), is_first::Bool = true
    )
    # Leading and trailing newlines are skipped; they are the responsibility of the
    # parent node.
    if is_leaf(node)
        if kind(node) === K"NewlineWs" && !has_tag(node, TAG_LINE_CONT) &&
                !(is_last || is_first)
            return add_tag(node, TAG_LINE_CONT)
        else
            return nothing
        end
    elseif is_triple_thing(node)
        # Check is_first inside to break the recursion and considier triple strings leafs
        if !is_first
            return indent_triple_thing(ctx, node)
        else
            return nothing
        end
    else
        any_kid_changed = false
        kids = verified_kids(node)
        for (i, kid) in pairs(kids)
            kid′ = continue_all_newlines(
                ctx, kid; is_last = i == lastindex(kids),
                is_first = is_first && i == firstindex(kids)
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
    @assert kind(node) in KSet"iteration generator"
    if kind(node) === K"iteration" && ctx.lineage_kinds[end] === K"for" &&
            count(x -> kind(x) === K"in", verified_kids(node)) == 1
        # Single-iterator for loop: the K"in" child handles its own continuation
        # indentation via indent_op_call, so nothing to do here.
        return nothing
    end
    return continue_all_newlines(ctx, node)
end

function indent_assignment(ctx::Context, node::Node)
    @assert !is_leaf(node)
    @assert is_variable_assignment(ctx, node) || is_short_form_function_definition(node)
    kids = verified_kids(node)
    lhsidx = findfirst(!JuliaSyntax.is_whitespace, kids)::Int
    eqidx = findnext(!JuliaSyntax.is_whitespace, kids, lhsidx + 1)::Int
    # The operator spans multiple leaves for op= (`+` `=`), .= (`.` `=`), and
    # .op= (`.` `+` `=`) nodes; advance eqidx to the final `=` leaf
    if kind(node) in KSet"op= .op= .="
        eqidx += kind(node) === K".op=" ? 2 : 1
    end
    @assert length(kids) > eqidx
    rhsidx = findnext(!JuliaSyntax.is_whitespace, kids, eqidx + 1)::Int
    @assert rhsidx == lastindex(kids)
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
    changed = false
    for i in r
        if kind(kids[i]) === K"NewlineWs"
            changed |= tag_kid!(kids, i, TAG_LINE_CONT)
        end
    end
    # Mark the rhs for line continuation
    changed |= tag_kid!(kids, rhsidx, TAG_LINE_CONT)
    return changed ? make_node(node, kids) : nothing
end

function indent_paren_block(ctx::Context, node::Node)
    @assert kind(node) === K"block"
    @assert JuliaSyntax.has_flags(node, JuliaSyntax.PARENS_FLAG)
    return indent_listlike_between(ctx, node, K"(", K")")
end

function indent_do(ctx::Context, node::Node)
    @assert kind(node) === K"do"
    kids = verified_kids(node)
    # Skip over the call and go directly to the do-keyword
    do_idx = findfirst(x -> kind(x) === K"do", kids)::Int
    block_idx = findnext(x -> kind(x) === K"block", kids, do_idx + 1)::Int
    end_idx = findnext(x -> kind(x) === K"end", kids, block_idx + 1)::Int
    return indent_keyword_block_end!(ctx, node, do_idx, block_idx, end_idx)
end

function indent_quote(ctx::Context, node::Node)
    @assert kind(node) === K"quote"
    # The short (`:(...)`) form can be ignored since the inside (K"block", K"tuple", or
    # K"Identifier") of the quote will be handled by other passes.
    if JuliaSyntax.has_flags(node, JuliaSyntax.COLON_QUOTE)
        return nothing
    end
    # Long (`quote ... end`) form
    kids = verified_kids(node)
    block_idx = findfirst(x -> kind(x) === K"block", kids)
    if block_idx === nothing
        # `bar` in `foo.bar` is a quote block...
        return nothing
    end
    @assert block_idx == 1 # Otherwise need to seek the stream
    kid′ = indent_begin(ctx, kids[block_idx], K"quote")
    kid′ === nothing && return nothing
    kids[block_idx] = kid′
    return make_node(node, kids)
end

# Literal array nodes and also ref-nodes (which can be either a typed-array or a getindex)
function indent_array(ctx::Context, node::Node)
    @assert kind(node) in KSet"vect vcat typed_vcat ncat ref comprehension typed_comprehension"
    return indent_listlike_between(ctx, node, K"[", K"]")
end

# TODO: can a row be multiline?
function indent_comparison(ctx::Context, node::Node)
    @assert kind(node) === K"comparison"
    return continue_all_newlines(ctx, node)
end

# Indent a nested documented module
function indent_doc_module(ctx::Context, node::Node; do_indent::Bool)
    @assert kind(node) === K"doc"
    kids = verified_kids(node)
    mod_idx = findfirst(x -> kind(x) === K"module", kids)::Int
    changed = apply_at_kid!(
        (ctx, kid) -> indent_module(ctx, kid; do_indent = do_indent), ctx, kids, mod_idx
    )
    return changed ? make_node(node, kids) : nothing
end

# Indent a nested module
function indent_module(ctx::Context, node::Node; do_indent::Bool = true)
    @assert kind(node) in KSet"module doc"
    if kind(node) === K"doc"
        return indent_doc_module(ctx, node; do_indent = do_indent)
    end
    kids = verified_kids(node)
    # First node is the module keyword
    mod_idx = 1
    @assert is_leaf(kids[mod_idx]) && kind(kids[mod_idx]) in KSet"module baremodule"
    # Next we expect whitespace + module identifier and then the module body block
    modname_idx = findnext(x -> !JuliaSyntax.is_whitespace(x), kids, mod_idx + 1)::Int
    block_idx = findnext(x -> kind(x) === K"block", kids, modname_idx + 1)::Int
    @assert block_idx == modname_idx + 1
    end_idx = findnext(x -> kind(x) === K"end", kids, block_idx + 1)::Int
    return indent_keyword_block_end!(ctx, node, mod_idx, block_idx, end_idx; do_indent = do_indent)
end

# The only thing at top level that we need to indent are modules which don't occupy the full
# top level expression, for example a file with an inner module followed by some code.
function is_module_or_doc_module(x::Node)
    return kind(x) === K"module" ||
        (kind(x) === K"doc" && findfirst(y -> kind(y) === K"module", verified_kids(x)) !== nothing)
end

function indent_toplevel(ctx::Context, node::Node)
    @assert kind(node) === K"toplevel"
    kids = verified_kids(node)
    mod_idx = findfirst(is_module_or_doc_module, kids)
    if mod_idx === nothing
        # No module here
        return nothing
    end
    # If the only top level expression is a module we don't indent it
    do_indent = count(!JuliaSyntax.is_whitespace, kids) > 1
    any_kid_changed = false
    while mod_idx !== nothing
        any_kid_changed |= apply_at_kid!(
            (ctx, kid) -> indent_module(ctx, kid; do_indent = do_indent), ctx, kids, mod_idx
        )
        mod_idx = findnext(is_module_or_doc_module, kids, mod_idx + 1)
    end
    return any_kid_changed ? make_node(node, kids) : nothing
end

function indent_local_global(ctx::Context, node::Node)
    @assert kind(node) in KSet"local global"
    @assert !is_global_local_list(node)
    # Something like `local x = 1` or `global function foo(...)`. Continue all newlines
    # between the keyword and the next non-whitespace node.
    kids = verified_kids(node)
    kw = findfirst(x -> is_leaf(x) && kind(x) in KSet"local global", kids)::Int
    nonws = findnext(!JuliaSyntax.is_whitespace, kids, kw + 1)::Int
    @assert kind(first_leaf(kids[nonws])) !== K"NewlineWs"
    changed = false
    for i in (kw + 1):(nonws - 1)
        if kind(kids[i]) === K"NewlineWs"
            changed |= tag_kid!(kids, i, TAG_LINE_CONT)
        end
    end
    return changed ? make_node(node, kids) : nothing
end

function insert_delete_mark_newlines(ctx::Context, node::Node)
    if is_leaf(node)
        return nothing
    elseif is_short_form_function_definition(node)
        return indent_assignment(ctx, node)
    elseif kind(node) in KSet"function macro"
        return indent_function_or_macro(ctx, node)
    elseif kind(node) === K"if"
        return indent_if(ctx, node)
    elseif kind(node) === K"let"
        return indent_let(ctx, node)
    elseif is_begin_block(node)
        return indent_begin(ctx, node)
    elseif kind(node) in KSet"call dotcall" && !is_any_op_call(node)
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
    elseif kind(node) in KSet"|| && .|| .&&"
        return indent_short_circuit(ctx, node)
    elseif kind(node) in KSet"using import export public" || is_global_local_list(node)
        return indent_using_import_export_public(ctx, node)
    elseif kind(node) in KSet"local global"
        return indent_local_global(ctx, node)
    elseif is_variable_assignment(ctx, node)
        return indent_assignment(ctx, node)
    elseif kind(node) === K"?"
        return indent_ternary(ctx, node)
    elseif kind(node) in KSet"generator iteration"
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

# Check whether the kid at `idx` is a whitespace node that is followed by an empty line
# (an item node of span 1 whose single byte is a newline). Such whitespace should be
# deleted. The stream must be positioned at the start of the kid and is restored before
# returning.
function ws_precedes_empty_line(
        ctx::Context, kids::Vector{Node}, idx::Int, close_idx::Int, itemkind::JuliaSyntax.Kind
    )
    kid = kids[idx]
    if !(
            kind(kid) === K"Whitespace" && idx + 1 < close_idx &&
                kind(kids[idx + 1]) === itemkind && span(kids[idx + 1]) == 1
        )
        return false
    end
    pos = position(ctx.fmt_io)
    accept_node!(ctx, kid)
    byte = peek(ctx.fmt_io)
    seek(ctx.fmt_io, pos)
    return byte == UInt8('\n')
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

    kids = verified_kids(node)

    # Fastpath for the common case of top level multiline strings like e.g. docstrings
    if !indented && findfirst(x -> kind(x) === K"Whitespace", kids) === nothing
        return nothing
    end

    open_idx = findfirst(x -> kind(x) === triplekind, kids)::Int
    close_idx = findlast(x -> kind(x) === triplekind, kids)::Int
    @assert close_idx == length(kids) # ?

    b = NodeBuilder(ctx, node)

    # Opening triple quote
    for i in 1:open_idx
        @assert i < open_idx || kind(kids[i]) === triplekind
        accept!(b, kids[i])
    end

    # Loop over the lines/expressions. After every item that ends with a newline we
    # expect the indenting whitespace for the next line (`expect_indent`).
    idx = open_idx + 1
    expect_indent = false
    while idx < close_idx
        kid = kids[idx]
        if !expect_indent
            if kind(kid) === itemkind
                if indented && span(kid) > 0 && read_bytes(ctx, kid)[end] == UInt8('\n')
                    expect_indent = true
                end
                accept!(b, kid)
            elseif kind(kid) === K"Whitespace"
                bytes = read_bytes(ctx, kid)
                # Multiline strings with trailing \ will have non-space characters in the
                # Whitespace node. These should be preserved.
                # TODO: Maybe this should be continue-indent to highlight the continuation?
                if length(bytes) == 2 + indent_span && bytes[1] === UInt8('\\') && bytes[2] === UInt8('\n')
                    @assert all(x -> x in (UInt8(' '), UInt8('\t')), @view(bytes[3:end]))
                    # This node is correct
                    accept!(b, kid)
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
                    emit!(b, ws_node(length(bytes)), bytes, span(kid))
                else
                    # Delete this node completely
                    @assert all(x -> x in (UInt8(' '), UInt8('\t')), bytes)
                    skip_kid!(b, kid)
                end
            else
                accept!(b, kid)
            end
        else
            expect_indent = false
            if kind(kid) === itemkind && span(kid) == 1 && peek(ctx.fmt_io) == UInt8('\n')
                # If this line is empty there shouldn't be a whitespace node. Loop around
                # with the same idx.
                continue # Skip the index increment
            elseif ws_precedes_empty_line(ctx, kids, idx, close_idx, itemkind)
                # Whitespace followed by an empty line should be deleted. Loop around with
                # the same idx; the whitespace branch above takes care of the deletion.
                continue # Skip the index increment
            elseif kind(kid) === K"Whitespace" && span(kid) == indent_span
                @assert all(x -> x === UInt8(' '), read_bytes(ctx, kid))
                accept!(b, kid)
            elseif kind(kid) === K"Whitespace"
                emit!(b, Node(head(kid), indent_span, tags(kid)), repeat(" ", indent_span), span(kid))
            else
                emit!(b, ws_node(indent_span), repeat(" ", indent_span), 0)
                continue # Skip the index increment
            end
        end
        idx += 1
    end
    # Make sure to add indent before the closing triple quote
    if expect_indent
        emit!(b, ws_node(indent_span), repeat(" ", indent_span), 0)
    end
    @assert idx == close_idx
    # Closing triple quote
    close_kid = kids[close_idx]
    @assert kind(close_kid) === triplekind
    accept!(b, close_kid)
    return finish!(b, node)
end

const re_fence_open = r"^(\h*)(`{3,})\h*(\{[A-Za-z0-9_-]*\}|[A-Za-z0-9_-]*)"

is_julia_lang(lang::AbstractString) = lang in ("julia", "julia-repl", "jldoctest")

# Markdown and Quarto markdown file extensions (case-insensitive, e.g. `README.MD`)
function is_markdown_file(path::AbstractString)
    ext = lowercase(path)
    return endswith(ext, ".md") || endswith(ext, ".qmd")
end

function format_julia_block(block_lines::Vector{String})
    isempty(block_lines) && return block_lines
    code = join(block_lines)
    # When formatting blocks we adhere to "if it parses we format it" since sometimes blocks
    # are tagged as Julia code but may be pseudo-code for example.
    formatted = try
        format_string(code)
    catch e
        e isa JuliaSyntax.ParseError || rethrow()
        # @error "Could not parse julia block" code e
        return block_lines
    end
    return collect_lines(IOBuffer(formatted); keep = true)
end

const JULIA_REPL_PROMPT = "julia> "

# Strip the prompt, leading whitespace and output. Format the input lines and re-insert the
# prompt and leading whitespace.
function format_repl_block(block_lines::Vector{String})
    nprompt = length(JULIA_REPL_PROMPT)
    continuation = repeat(" ", nprompt)
    result = String[]
    i = 1
    while i <= length(block_lines)
        line = block_lines[i]
        if startswith(line, JULIA_REPL_PROMPT)
            # Collect the full input chunk: strip the prompt prefix and continuations
            input_lines = String[chop(line; head = nprompt, tail = 0)]
            j = i + 1
            while j <= length(block_lines)
                next_line = block_lines[j]
                if isempty(strip(next_line))
                    push!(input_lines, "\n")
                    j += 1
                elseif startswith(next_line, continuation)
                    push!(input_lines, chop(next_line; head = nprompt, tail = 0))
                    j += 1
                else
                    break
                end
            end
            code = join(input_lines)
            # Same as `format_julia_block`: only format if the block parses; fall back
            # to passing the original prompt/continuation lines through unchanged.
            formatted = try
                format_string(code)
            catch e
                e isa JuliaSyntax.ParseError || rethrow()
                # @error "Could not parse julia block" code e
                for k in i:(j - 1)
                    push!(result, block_lines[k])
                end
                i = j
                continue
            end
            fmt_lines = collect_lines(IOBuffer(formatted); keep = true)
            first = true
            for fl in fmt_lines
                if isempty(strip(fl))
                    push!(result, "\n")
                elseif first
                    push!(result, JULIA_REPL_PROMPT * fl)
                    first = false
                else
                    push!(result, continuation * fl)
                end
            end
            i = j
        else
            push!(result, line)
            i += 1
        end
    end
    return result
end

# Dispatch to REPL formatter or regular formatter based on the code blocks language
function format_code_block(block_lines::Vector{String}, lang::String)
    # `format_string` emits LF. Normalize CRLF input while parsing, then restore CRLF
    # so formatting a Markdown code block does not introduce mixed line endings. Blocks
    # with mixed endings count as CRLF so that stray LF lines converge to CRLF instead
    # of the block flip-flopping to LF.
    crlf = any(l -> endswith(l, "\r\n"), block_lines)
    normalized_lines =
        crlf ? [replace(l, "\r\n" => "\n") for l in block_lines] : block_lines
    # jldoctest blocks can be either REPL style or plain code; dispatch on the content
    repl = lang == "julia-repl" ||
        (lang == "jldoctest" && any(l -> startswith(l, JULIA_REPL_PROMPT), normalized_lines))
    formatted = repl ? format_repl_block(normalized_lines) : format_julia_block(normalized_lines)
    return crlf ?
        [endswith(l, "\n") ? chop(l; tail = 1) * "\r\n" : l for l in formatted] :
        formatted
end

# Identify julia source code blocks (``` blocks four-space-indent blocks),
# collect the lines, format the text and re-insert
function format_markdown(s::String; line_ranges::Vector{UnitRange{Int}} = UnitRange{Int}[])
    lines = collect_lines(IOBuffer(s); keep = true)
    validate_line_ranges(lines, line_ranges)
    isempty(lines) && return s
    # A block at lines `a:b` is formatted iff `line_ranges` is empty (no filter) or at
    # least one range overlaps the block. Block-granular: partial blocks are formatted
    # in full when any line within overlaps.
    in_range(a, b) = isempty(line_ranges) ||
        any(r -> !isdisjoint(r, a:b), line_ranges)
    # Indented code blocks (CommonMark "indented" style) are handled like implicit
    # fences: opener is blank-line-or-start-of-docstring followed by a non-blank line
    # with >= 4 leading spaces; content = consecutive non-blank 4-space-indented lines;
    # closer = blank line or end-of-docstring. Same strip / format / re-indent pipeline
    # as ```julia fences, reused via format_julia_block.
    base_indent = "    "
    nbase_indent = ncodeunits(base_indent)
    result = String[]
    i = 1
    # True at start-of-content and just after any blank line. Indented code blocks can
    # only begin at block boundaries (CommonMark rule: must be preceded by a blank line).
    at_boundary = true
    while i <= length(lines)
        line = lines[i]
        m = match(re_fence_open, line)
        if m !== nothing
            indent = String(m.captures[1]::AbstractString)
            ticks = String(m.captures[2]::AbstractString)
            lang = String(m.captures[3]::AbstractString)
            # Quarto wrap the language of executable code blocks in braces, e.g. `{julia}`.
            # Strip them so that the language checks below see the plain language name.
            if startswith(lang, "{") && endswith(lang, "}")
                lang = String(chop(lang; head = 1, tail = 1))
            end
            nticks = length(ticks)
            re_close = Regex("^$(escape_string(indent))`{$(nticks),}\\h*\\r?\$")
            close_i = findnext(l -> occursin(re_close, l), lines, i + 1)
            if close_i === nothing
                # Unclosed fence: everything from here to end of string is inside this
                # (unclosed) block, so there are no further formattable fences.
                append!(result, @view lines[i:end])
                break
            end
            block_lines = lines[(i + 1):(close_i - 1)]
            # Skip if `--lines` doesn't overlap this block's full extent.
            if !in_range(i, close_i)
                append!(result, @view lines[i:close_i])
                i = close_i + 1
                at_boundary = false
                continue
            end
            # Non-Julia fence: copy through unchanged
            if !is_julia_lang(lang)
                append!(result, @view lines[i:close_i])
                i = close_i + 1
                at_boundary = false
                continue
            end
            nindent = ncodeunits(indent)
            # Require non-empty block lines to have >= nindent leading spaces (empty lines
            # are exempt). Strip nindent spaces before formatting so format_string sees
            # toplevel code, then restore using the original count after formatting
            stripped_block = String[]
            valid_indent = true
            for l in block_lines
                if isempty(strip(l))
                    push!(stripped_block, l)
                elseif nindent == 0 || startswith(l, indent)
                    push!(stripped_block, nindent == 0 ? l : l[(nindent + 1):end])
                else
                    valid_indent = false
                    break
                end
            end
            if !valid_indent
                append!(result, @view lines[i:close_i])
                i = close_i + 1
                at_boundary = false
                continue
            end
            formatted_stripped = format_code_block(stripped_block, lang)
            formatted_block = nindent == 0 ? formatted_stripped :
                [isempty(strip(l)) ? l : indent * l for l in formatted_stripped]
            push!(result, line)
            append!(result, formatted_block)
            push!(result, lines[close_i])
            i = close_i + 1
            at_boundary = false
        elseif at_boundary && startswith(line, base_indent) && !all(isspace, line)
            # Indented code block. Blank lines inside the block are allowed (same as
            # inside ```-fences): skip them during the scan and only confirm `end_idx`
            # when another indented non-blank line is found. Terminator = an unindented
            # non-blank line or EOF. Any trailing blank lines after the last indented
            # line are *not* included in the block.
            end_idx = i
            k = i
            while k + 1 <= length(lines)
                next_line = lines[k + 1]
                if all(isspace, next_line)
                    k += 1
                elseif startswith(next_line, base_indent)
                    k += 1
                    end_idx = k
                else
                    break
                end
            end
            if !in_range(i, end_idx)
                append!(result, @view lines[i:end_idx])
                i = end_idx + 1
                at_boundary = false
                continue
            end
            # Strip the indent; normalize whitespace-only lines while preserving their
            # newline style (chop would otherwise swallow the newline).
            stripped = String[
                isempty(strip(l)) ? (endswith(l, "\r\n") ? "\r\n" : "\n") :
                    chop(l; head = nbase_indent, tail = 0)
                    for l in lines[i:end_idx]
            ]
            formatted = format_code_block(stripped, "julia")
            if formatted == stripped
                # parse failed or already idempotent — pass through unchanged
                append!(result, @view lines[i:end_idx])
            else
                for l in formatted
                    push!(result, isempty(strip(l)) ? l : base_indent * l)
                end
            end
            i = end_idx + 1
            at_boundary = false
        else
            push!(result, line)
            at_boundary = all(isspace, line)
            i += 1
        end
    end
    return join(result)
end

# Extract the string content from a triple string node, pass to format_markdown,
# re-interpret lines as a triple string node.
function format_docstring_string(ctx::Context, node::Node)
    @assert is_triple_string(node)
    triplekind = K"\"\"\""

    pos = position(ctx.fmt_io)
    str_kids = verified_kids(node)
    open_idx = findfirst(x -> kind(x) === triplekind, str_kids)::Int
    close_idx = findnext(x -> kind(x) === triplekind, str_kids, open_idx + 1)::Int

    # Bail out if the docstring contains anything other than plain string/whitespace
    # between the triple-quote delimiters (e.g. $-interpolation introduces K"$" and
    # K"Identifier" / K"parens" kids).
    # TODO: Support interpolation by collecting bytes from all non-indent kids (skip
    #       K"Whitespace" + leading trivia K"String" only), formatting as text, then
    #       re-parsing the formatted output as `"""..."""` and grafting the resulting kids.
    for i in (open_idx + 1):(close_idx - 1)
        k = kind(str_kids[i])
        k === K"String" || k === K"Whitespace" || return nothing
    end

    # Collect indent whitespace (first K"Whitespace" kid) and content bytes
    indent_ws_bytes = UInt8[]
    content_bytes = UInt8[]
    for kid in str_kids
        # The `\n` immediately after the opening `"""` is parsed as a trivia-flagged
        # `K"String"` kid (rather than as content) when content starts on the next line.
        # Skip it here — that leading newline is part of the delimiter convention, not
        # the docstring content.
        if kind(kid) === K"String" && !JuliaSyntax.is_trivia(kid)
            append!(content_bytes, read_bytes(ctx, kid))
        elseif kind(kid) === K"Whitespace" && isempty(indent_ws_bytes)
            # All K"Whitespace" kids of a triple-quoted string contain the same bytes —
            # the common leading indent stripped by the parser (same size on every
            # non-empty line). Record the first one we see and skip the rest. A zero-
            # length `K"Whitespace"` kid is impossible (the parser never emits zero-span
            # tokens), so `isempty(indent_ws_bytes)` is a reliable "have we recorded
            # the indent yet?" flag.
            append!(indent_ws_bytes, read_bytes(ctx, kid))
        end
        accept_node!(ctx, kid)
    end
    @assert position(ctx.fmt_io) == pos + span(node)

    # Pass the extracted string to the markdown formatter
    content = String(content_bytes)
    formatted = format_markdown(content)
    seek(ctx.fmt_io, pos)
    content == formatted && return nothing

    # The opening """ may be followed by a trivia K"String" "\n" (when content starts on
    # a new line) or directly by non-trivia content (when the first line already has
    # content, e.g. `"""Summary.\n...\n"""`).
    has_leading_nl = open_idx + 1 < close_idx &&
        kind(str_kids[open_idx + 1]) === K"String" &&
        JuliaSyntax.has_flags(str_kids[open_idx + 1], JuliaSyntax.TRIVIA_FLAG)
    prefix_count = has_leading_nl ? 2 : 1
    open_idx + prefix_count <= close_idx || return nothing

    # Accept the fixed prefix: opening """ and optional trivia \n
    for k in 0:(prefix_count - 1)
        accept_node!(ctx, str_kids[open_idx + k])
    end

    # Span of the middle section: everything between the accepted prefix and close_idx
    middle_span = sum(span(str_kids[i]) for i in (open_idx + prefix_count):(close_idx - 1); init = 0)

    # Split up the formatted strings and insert the trivia newlines and leading whitespace
    new_middle_bytes = UInt8[]
    new_middle_kids = Node[]
    ws_head = JuliaSyntax.SyntaxHead(K"Whitespace", JuliaSyntax.TRIVIA_FLAG)
    str_head = JuliaSyntax.SyntaxHead(K"String", 0)
    for line in collect_lines(IOBuffer(formatted); keep = true)
        line_bytes = codeunits(line)
        if length(line_bytes) == 1 && line_bytes[1] == UInt8('\n')
            # Empty line should note have leading whitespace trivia
            push!(new_middle_bytes, UInt8('\n'))
            push!(new_middle_kids, Node(str_head, 1))
        else
            # Include the indent whitespace from the original source
            if !isempty(indent_ws_bytes)
                append!(new_middle_bytes, indent_ws_bytes)
                push!(new_middle_kids, Node(ws_head, length(indent_ws_bytes)))
            end
            append!(new_middle_bytes, line_bytes)
            push!(new_middle_kids, Node(str_head, length(line_bytes)))
        end
    end
    # For an indented triple string, the parser also emits a K"Whitespace" between the
    # final content line and the closing `"""` — representing the indent on the closing
    # delimiter's own line. Append it so the reconstructed structure matches.
    if !isempty(indent_ws_bytes)
        append!(new_middle_bytes, indent_ws_bytes)
        push!(new_middle_kids, Node(ws_head, length(indent_ws_bytes)))
    end

    # Insert the formatted bytes into the stream
    replace_bytes!(ctx, new_middle_bytes, middle_span)
    seek(ctx.fmt_io, pos)

    # Well-formed triple-quoted strings always have the closing """ as the last kid —
    # `"""` is balanced, and no trailing kids can follow it in a valid parse.
    @assert close_idx == length(str_kids)
    new_str_kids = Node[str_kids[i] for i in open_idx:(open_idx + prefix_count - 1)]
    append!(new_str_kids, new_middle_kids)
    push!(new_str_kids, str_kids[close_idx])
    return make_node(node, new_str_kids)
end

# Find string literals that are docstrings (K"doc" or @doc). We only consider triple-strings
# even though regular "..." strings can also be docstrings (but they rarely, if ever,
# contain julia code blocks).
function format_docstring(ctx::Context, node::Node)
    ctx.docstrings || return nothing
    # Only triple-quoted K"string" nodes (not cmdstring) can be docstrings
    kind(node) === K"string" || return nothing
    JuliaSyntax.has_flags(node, JuliaSyntax.TRIPLE_STRING_FLAG) || return nothing
    # Must be a direct child of a doc-string context
    isempty(ctx.lineage_kinds) && return nothing
    parent_kind = ctx.lineage_kinds[end]
    if parent_kind === K"doc" ||
            (
            parent_kind === K"macrocall" && !isempty(ctx.lineage_macros) &&
                ctx.lineage_macros[end] == "@doc"
        )
        return format_docstring_string(ctx, node)
    end
    return nothing
end

function is_docstring_literal(node::Node)
    kind(node) === K"string" && return true
    kind(node) === K"parens" || return false
    significant_kids = filter(verified_kids(node)) do kid
        return !JuliaSyntax.is_whitespace(kid) && kind(kid) !== K"Comment" &&
            !(is_leaf(kid) && kind(kid) in KSet"( )")
    end
    return length(significant_kids) == 1 && is_docstring_literal(only(significant_kids))
end

# Pattern matching for "bad" semicolons:
#  - `\s*;\n` -> `\n`
#  - `\s*;\s*#\n` -> `\s* \s*#\n`
function remove_trailing_semicolon_block(ctx::Context, node::Node, struct_body::Bool = false)
    kind(node) === K"block" || return nothing
    @assert !is_leaf(node)
    pos = position(ctx.fmt_io)
    kids = verified_kids(node)
    kids′ = kids
    # Whether strings in this block can become docstrings of the following expression.
    # Struct bodies can not be detected from the block node itself so the caller passes
    # `struct_body` instead.
    docstring_context = struct_body || is_begin_block(node) || is_begin_block(node, K"quote")
    semi_idx = findfirst(x -> kind(x) === K";", kids′)
    while semi_idx !== nothing
        if docstring_context
            prev_expr_idx = findprev(
                x -> !(JuliaSyntax.is_whitespace(x) || kind(x) === K"Comment"),
                kids′, semi_idx - 1
            )
            next_expr_idx = findnext(
                x -> !(JuliaSyntax.is_whitespace(x) || kind(x) in KSet"Comment ; end"),
                kids′, semi_idx + 1
            )
            if prev_expr_idx !== nothing && is_docstring_literal(kids′[prev_expr_idx]) &&
                    next_expr_idx !== nothing
                # In begin/quote blocks and struct bodies, a string followed by another
                # expression becomes its docstring unless a semicolon separates them.
                # Preserve that semicolon to avoid changing the parsed program. If no
                # expression follows the string no docstring can form and the semicolon
                # can be removed as usual.
                semi_idx = findnext(x -> kind(x) === K";", kids′, semi_idx + 1)
                continue
            end
        end
        search_index = semi_idx + 1
        if kmatch(kids′, KSet"; NewlineWs", semi_idx)
            # `\s*;\n` -> `\n`
            kids′ = kids′ === kids ? copy(kids) : kids′
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
            kids′ = kids′ === kids ? copy(kids) : kids′
            space_after = kmatch(kids′, KSet"; Whitespace", semi_idx)
            if semi_idx > firstindex(kids′) &&
                    kind(kids′[semi_idx - 1]) === K"NewlineWs"
                # A semicolon at the start of a comment-only line should be removed
                # together with the whitespace between it and the comment. Keeping a
                # replacement space here gives the comment one extra indentation level
                # until the formatter is run a second time.
                span_overwrite = span(kids′[semi_idx])
                space_after && (span_overwrite += span(kids′[semi_idx + 1]))
                let p = position(ctx.fmt_io)
                    for i in 1:(semi_idx - 1)
                        accept_node!(ctx, kids′[i])
                    end
                    replace_bytes!(ctx, "", span_overwrite)
                    seek(ctx.fmt_io, p)
                end
                space_after && deleteat!(kids′, semi_idx + 1)
                deleteat!(kids′, semi_idx)
                search_index = semi_idx
                semi_idx = findnext(x -> kind(x) === K";", kids′, search_index)
                continue
            end
            ws_span = span(kids′[semi_idx])
            @assert ws_span == 1
            space_before = kmatch(kids′, KSet"Whitespace ;", semi_idx - 1)
            if space_before
                ws_span += span(kids′[semi_idx - 1])
            end
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
            ws = ws_node(ws_span)
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
        return remove_trailing_semicolon_block(ctx, node)
    end
    if !(!is_leaf(node) && kind(node) in KSet"if elseif quote function for let while macro try catch finally else do struct")
        return nothing
    end
    if kind(node) === K"quote" && JuliaSyntax.has_flags(node, JuliaSyntax.COLON_QUOTE)
        # This node is `:(...)` and not `quote...end`
        return nothing
    end
    kids = verified_kids(node)
    block_predicate = function (x)
        return kind(x) === K"block" && !JuliaSyntax.has_flags(x, JuliaSyntax.PARENS_FLAG)
    end
    struct_body = kind(node) === K"struct"
    block_idx = findfirst(block_predicate, kids)
    if kind(node) === K"let"
        # The first block of let is the variables
        block_idx = findnext(block_predicate, kids, block_idx + 1)
    end
    any_changed = false
    while block_idx !== nothing
        any_changed |= apply_at_kid!(
            (ctx, kid) -> remove_trailing_semicolon_block(ctx, kid, struct_body),
            ctx, kids, block_idx
        )
        block_idx = findnext(block_predicate, kids, block_idx + 1)
    end
    return any_changed ? make_node(node, kids) : nothing
end

function spaces_around_comments(ctx::Context, node::Node)
    is_leaf(node) && return nothing
    kids = verified_kids(node)
    # We assume that the previous node ends with ws, which should be true since the same
    # pass here adds it if the first kid is a comment.
    prev_kid_ends_with_ws = true
    ws = ws_node(1)
    b = NodeBuilder(ctx, node)
    for kid in kids
        kid′ = kid
        if !prev_kid_ends_with_ws && (
                kind(kid) === K"Comment" ||
                    (fl = first_leaf(kid); fl !== nothing && kind(fl) === K"Comment")
            )
            if kind(kid) === K"Comment"
                # Insert a space before the comment
                emit!(b, ws, " ", 0)
                accept!(b, kid)
            else
                # When the comment is found within the kid the whitespace is added right
                # before the comment inside of the kid instead of in this outer context.
                # This does not necessarily match how JuliaSyntax would have parsed it, but
                # seems to work better than the alternative.
                kid′ = add_before_first_leaf(kid, ws)
                @assert span(kid′) == span(kid) + 1
                emit!(b, kid′, " ", 0)
            end
        else
            accept!(b, kid)
        end
        # Note: This allows (but doesn't require) no space after opening brackets, see
        # https://github.com/fredrikekre/Runic.jl/issues/81
        prev_kid_ends_with_ws = kind(kid′) in KSet"Whitespace NewlineWs ( { [" ||
            (ll = last_leaf(kid′); ll !== nothing && kind(ll) in KSet"Whitespace NewlineWs")
    end
    return finish!(b, node)
end

function return_node(ctx::Context, ret::Node)
    if !is_leaf(ret)
        @assert !(kind(first_leaf(ret)) in KSet"NewlineWs Whitespace")
    end
    replace_bytes!(ctx, "return ", 0)
    kids = [Node(JuliaSyntax.SyntaxHead(K"return", 0), 6), ws_node(1), ret]
    return Node(JuliaSyntax.SyntaxHead(K"return", 0), kids)
end

function has_return(node::Node)
    kids = verified_kids(node)
    if kind(node) in KSet"let catch else finally"
        idx = findfirst(x -> kind(x) === K"block", kids)::Int
        if kind(node) === K"let"
            idx = findnext(x -> kind(x) === K"block", kids, idx + 1)::Int
        end
        return has_return(kids[idx])
    elseif kind(node) in KSet"try if elseif"
        # Look for the initial try/if block and then for
        # catch/else/finally (for try) or elseif/else (for if).
        pred = function (x)
            return !is_leaf(x) && kind(x) in KSet"catch else finally elseif block"
        end
        idx = findfirst(pred, kids)
        while idx !== nothing
            has_return(kids[idx]) && return true
            idx = findnext(pred, kids, idx + 1)
        end
        return false
    elseif kind(node) === K"macrocall"
        # Check direct kids but also recurse into blocks to catch e.g. `@foo begin ... end`.
        idx = findfirst(x -> kind(x) === K"return", kids)
        idx === nothing || return true
        # juliac: written as a loop instead of `any(f, kids)` since the `any` keyword
        # argument body fails `--trim` verification on Julia 1.12.
        for x in kids
            if !is_leaf(x) && kind(x) in KSet"let try if block macrocall" && has_return(x)
                return true
            end
        end
        return false
    elseif kind(node) === K"block"
        # Don't care whether this is the last expression,
        # that is the job of a linter or something I guess.
        return findfirst(x -> kind(x) === K"return", kids) !== nothing
    else
        unreachable()
    end
end

function explicit_return_block(ctx, node)
    @assert kind(node) === K"block"
    if has_return(node)
        # If the block already has a return node (anywhere) we accept it and move on.
        return nothing
    end
    kids = verified_kids(node)
    kids′ = kids
    rexpr_idx = findlast(!JuliaSyntax.is_whitespace, kids′)
    if rexpr_idx === nothing
        # Empty block. TODO: Perhaps add `return nothing`?
        return nothing
    end
    rexpr = kids′[rexpr_idx]
    @assert kind(rexpr) !== K"return" # Should have been caught by has_return
    if is_leaf(rexpr) ||
            kind(rexpr) in KSet"call dotcall tuple vect ref hcat typed_hcat vcat typed_vcat \
                ? && || .&& .|| :: juxtapose <: >: comparison string . -> comprehension do macro \
                typed_comprehension where parens curly function quote global local cmdstring" ||
            is_string_macro(rexpr) || is_assignment(rexpr) ||
            (kind(rexpr) in KSet"let if try" && !has_return(rexpr)) ||
            (kind(rexpr) === K"macrocall" && !has_return(rexpr)) ||
            (is_begin_block(rexpr) && !has_return(rexpr))
        # The cases caught in this branch are simple, just wrap the last expression in a
        # return node. Also make sure the previous node is a K"NewlineWs".
        for i in 1:(rexpr_idx - 1)
            accept_node!(ctx, kids′[i])
        end
        # If this is a call node, and the call the function name contains `throw` or `error` we
        # bail because `return throw(...)` looks kinda stupid.
        if kind(rexpr) === K"call"
            call_kids = verified_kids(rexpr)
            fname_idx = findfirst(!JuliaSyntax.is_whitespace, call_kids)::Int
            @assert fname_idx == firstindex(call_kids)
            local fname
            let p = position(ctx.fmt_io)
                fname = String(read_bytes(ctx, call_kids[fname_idx]))
                seek(ctx.fmt_io, p)
            end
            if contains(fname, "throw") || contains(fname, "error")
                return nothing
            end
        end
        # We will make changes so copy
        kids′ = kids′ === kids ? copy(kids) : kids′
        # Make sure the previous node is a K"NewlineWs"
        if !kmatch(kids′, KSet"NewlineWs", rexpr_idx - 1)
            spn = 0
            @assert kind(first_leaf(rexpr)) !== K"NewlineWs"
            # Can it happen that there are whitespace hidden in the previous node?
            # Let's see if this assert ever fire.
            if rexpr_idx > 1
                prev = kids′[rexpr_idx - 1]
                if !is_leaf(prev)
                    @assert !(kind(last_leaf(prev)) in KSet"Whitespace NewlineWs")
                end
            end
            # Check whether there are whitespace we need to overwrite
            if kmatch(kids′, KSet"Whitespace", rexpr_idx - 1)
                # The previous node is whitespace
                spn = span(popat!(kids′, rexpr_idx - 1))
                seek(ctx.fmt_io, position(ctx.fmt_io) - spn)
                rexpr_idx -= 1
                @assert kind(first_leaf(rexpr)) !== K"Whitespace"
            end
            @assert kind(first_leaf(rexpr)) !== K"Whitespace"
            nl = "\n" * repeat(" ", 4 * ctx.indent_level)
            nlnode = nlws_node(sizeof(nl))
            insert!(kids′, rexpr_idx, nlnode)
            rexpr_idx += 1
            replace_bytes!(ctx, nl, spn)
            accept_node!(ctx, nlnode)
        end
        ret = return_node(ctx, rexpr)
        kids′[rexpr_idx] = ret
        return make_node(node, kids′)
    elseif kind(rexpr) in KSet"for while"
        # For `for` and `while` loops we add `return` after the block.
        @assert kind(kids′[end]) === K"NewlineWs"
        @assert kind(last_leaf(rexpr)) === K"end"
        insert_idx = lastindex(kids)
        kids′ = kids′ === kids ? copy(kids) : kids′
        for i in 1:(insert_idx - 1)
            accept_node!(ctx, kids′[i])
        end
        # Insert newline
        nl = "\n" * repeat(" ", 4 * ctx.indent_level)
        nlnode = nlws_node(sizeof(nl))
        insert!(kids′, insert_idx, nlnode)
        replace_bytes!(ctx, nl, 0)
        accept_node!(ctx, nlnode)
        # Insert `return`
        replace_bytes!(ctx, "return", 0)
        retnode = Node(
            JuliaSyntax.SyntaxHead(K"return", 0), [
                Node(JuliaSyntax.SyntaxHead(K"return", 0), 6),
            ]
        )
        insert!(kids′, insert_idx + 1, retnode)
        return make_node(node, kids′)
    else
        # error("Unhandled node in explicit_return_block: $(kind(rexpr))")
    end
    return nothing
end

function explicit_return(ctx::Context, node::Node)
    if !(!is_leaf(node) && kind(node) in KSet"function macro" && !is_short_form_function_definition(node))
        return nothing
    end
    if !safe_to_insert_return(ctx, node)
        return nothing
    end
    kids = verified_kids(node)
    pos = position(ctx.fmt_io)
    block_idx = findlast(x -> kind(x) === K"block", verified_kids(node))
    block_idx === nothing && return nothing
    for i in 1:(block_idx - 1)
        accept_node!(ctx, kids[i])
    end
    block′ = explicit_return_block(ctx, kids[block_idx])
    seek(ctx.fmt_io, pos)
    block′ === nothing && return nothing
    kids′ = copy(kids)
    kids′[block_idx] = block′
    return make_node(node, kids′)
end

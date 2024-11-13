# SPDX-License-Identifier: MIT

########################################################
# Node utilities extensions and JuliaSyntax extensions #
########################################################

# JuliaSyntax.jl overloads == for this but seems easier to just define a new function
function nodes_equal(n1::Node, n2::Node)
    head(n1) == head(n2) && span(n1) == span(n2) || return false
    # juliac: this is `all(((x, y),) -> nodes_equal(x, y), zip(n1.kids, n2.kids))` but
    # written out as an explicit loop to help inference.
    if is_leaf(n1)
        return is_leaf(n2)
    end
    kids1 = verified_kids(n1)
    kids2 = verified_kids(n2)
    length(kids1) == length(kids2) || return false
    for i in eachindex(kids1)
        nodes_equal(n1.kids[i], n2.kids[i]) || return false
    end
    return true
end

# See JuliaSyntax/src/parse_stream.jl
function stringify_flags(node::Node)
    io = IOBuffer()
    if JuliaSyntax.has_flags(node, JuliaSyntax.TRIVIA_FLAG)
        write(io, "trivia,")
    end
    if JuliaSyntax.is_operator(kind(node))
        if JuliaSyntax.has_flags(node, JuliaSyntax.DOTOP_FLAG)
            write(io, "dotted,")
        end
        if JuliaSyntax.has_flags(node, JuliaSyntax.SUFFIXED_FLAG)
            write(io, "suffixed,")
        end
    end
    if kind(node) in KSet"call dotcall"
        if JuliaSyntax.has_flags(node, JuliaSyntax.PREFIX_CALL_FLAG)
            write(io, "prefix-call,")
        end
        if JuliaSyntax.has_flags(node, JuliaSyntax.INFIX_FLAG)
            write(io, "infix-op,")
        end
        if JuliaSyntax.has_flags(node, JuliaSyntax.PREFIX_OP_FLAG)
            write(io, "prefix-op,")
        end
        if JuliaSyntax.has_flags(node, JuliaSyntax.POSTFIX_OP_FLAG)
            write(io, "postfix-op,")
        end
    end
    if kind(node) in KSet"string cmdstring" &&
            JuliaSyntax.has_flags(node, JuliaSyntax.TRIPLE_STRING_FLAG)
        write(io, "triple,")
    end
    if kind(node) in KSet"string cmdstring Identifier" &&
            JuliaSyntax.has_flags(node, JuliaSyntax.RAW_STRING_FLAG)
        write(io, "raw,")
    end
    if kind(node) in KSet"tuple block macrocall" &&
            JuliaSyntax.has_flags(node, JuliaSyntax.PARENS_FLAG)
        write(io, "parens,")
    end
    if kind(node) === K"quote" && JuliaSyntax.has_flags(node, JuliaSyntax.COLON_QUOTE)
        write(io, "colon,")
    end
    if kind(node) === K"toplevel" && JuliaSyntax.has_flags(node, JuliaSyntax.TOPLEVEL_SEMICOLONS_FLAG)
        write(io, "semicolons,")
    end
    if kind(node) === K"struct" && JuliaSyntax.has_flags(node, JuliaSyntax.MUTABLE_FLAG)
        write(io, "mutable,")
    end
    if kind(node) === K"module" && JuliaSyntax.has_flags(node, JuliaSyntax.BARE_MODULE_FLAG)
        write(io, "baremodule,")
    end
    truncate(io, max(0, position(io) - 1)) # Remove trailing comma
    return String(take!(io))
end

function pop_if_whitespace!(popf!::F, node) where {F}
    if !is_leaf(node) && begin
            kids = verified_kids(node)
            length(kids) > 0 &&
                JuliaSyntax.is_whitespace(popf! === popfirst! ? kids[1] : kids[end])
        end
        ws = popf!(kids)
        @assert JuliaSyntax.is_whitespace(ws)
        node′ = make_node(node, kids)
        @assert span(node) == span(node′) + span(ws)
        return node′, ws
    end
    return nothing, nothing
end

# The parser is somewhat inconsistent(?) with where e.g. whitespace nodes end up so in order
# to simplify the formatting code we normalize some things.
function normalize_tree!(node)
    is_leaf(node) && return node
    kids = verified_kids(node)

    # For K"let" the token (K"NewlineWs" or K";") separating the vars block from the
    # body ends up in between the two K"block" nodes. Move it into the body block.
    if kind(node) === K"let"
        varsidx = findfirst(x -> kind(x) === K"block", kids)::Int
        blockidx = findnext(x -> kind(x) === K"block", kids, varsidx + 1)::Int
        while blockidx != varsidx + 1
            grandkid = popat!(kids, blockidx - 1)
            blockidx -= 1
            block = kids[blockidx]
            pushfirst!(verified_kids(block), grandkid)
            kids[blockidx] = make_node(block, verified_kids(block))
        end
        @assert span(node) == mapreduce(span, +, kids; init = 0)
    end

    # Normalize K"Whitespace" nodes in blocks. For example in `if x y end` the space
    # will be outside the block just before the K"end" node, but in `if x\ny\nend` the
    # K"NewlineWs" will end up inside the block.
    if kind(node) in KSet"function if elseif for while try do macro module baremodule let struct module"
        blockidx = 0
        while let
                blockidx = findnext(x -> kind(x) === K"block", kids, blockidx + 1)
                blockidx !== nothing && blockidx < length(kids)
            end
            if kind(kids[blockidx + 1]) !== K"Whitespace"
                continue
            end
            block = kids[blockidx]
            blockkids = verified_kids(block)
            @assert !(kind(blockkids[end]) in KSet"Whitespace NewlineWs")
            push!(blockkids, popat!(kids, blockidx + 1))
            kids[blockidx] = make_node(block, blockkids)
        end
        @assert span(node) == mapreduce(span, +, kids; init = 0)
    end

    # Normalize K"Whitespace" nodes in if-elseif-else chains where the node needs to
    # move many steps into the last else block.
    if kind(node) === K"if"
        elseifidx = findfirst(x -> kind(x) === K"elseif", kids)
        if elseifidx !== nothing
            endidx = findnext(x -> kind(x) === K"end", kids, elseifidx + 1)::Int
            if elseifidx + 2 == endidx && kind(kids[elseifidx + 1]) === K"Whitespace"
                ws = popat!(kids, elseifidx + 1)
                elseifnode = insert_into_last_else_block(kids[elseifidx], ws)
                @assert elseifnode !== nothing
                kids[elseifidx] = elseifnode
            end
        end
        @assert span(node) == mapreduce(span, +, kids; init = 0)
    end

    # Normalize K"Whitespace" nodes in try-catch-finally-else
    if kind(node) === K"try"
        catchidx = 0
        while let
                catchidx = findnext(
                    x -> kind(x) in KSet"catch finally else", kids, catchidx + 1
                )
                catchidx !== nothing
            end
            @assert catchidx + 1 <= length(kids)
            if kind(kids[catchidx + 1]) === K"Whitespace"
                ws = popat!(kids, catchidx + 1)
                catchnode = insert_into_last_catchlike_block(kids[catchidx], ws)
                @assert catchnode !== nothing
                kids[catchidx] = catchnode
            end
        end
        @assert span(node) == mapreduce(span, +, kids; init = 0)
    end

    # Normalize K"NewlineWs" nodes in empty do-blocks
    if kind(node) === K"do"
        tupleidx = findfirst(x -> kind(x) === K"tuple", kids)::Int
        blockidx = findnext(x -> kind(x) === K"block", kids, tupleidx + 1)::Int
        @assert tupleidx + 1 == blockidx
        tuple = kids[tupleidx]
        tuplekids = verified_kids(tuple)
        if length(tuplekids) > 0 && kind(tuplekids[end]) === K"NewlineWs"
            # If the tuple ends with a K"NewlineWs" node we move it into the block
            block = kids[blockidx]
            blockkids = verified_kids(block)
            if length(blockkids) > 0
                @assert kind(blockkids[1]) !== K"Whitespace"
            end
            pushfirst!(blockkids, pop!(tuplekids))
            # Remake the nodes to recompute the spans
            kids[tupleidx] = make_node(tuple, tuplekids)
            kids[blockidx] = make_node(block, blockkids)
        end
        @assert span(node) == mapreduce(span, +, kids; init = 0)
    end

    @assert span(node) == mapreduce(span, +, kids; init = 0)
    @assert kids === verified_kids(node)

    # Loop over the kids to bubble up whitespace
    i = 0
    while i < lastindex(kids)
        i += 1
        kid = kids[i]

        # Recursively normalize the kid to bubble up whitespace to the front. This should
        # never change the node kind or the span, just internally reorganize the tokens.
        kid′ = normalize_tree!(kid)
        @assert kid === kid′
        is_leaf(kid) ||
            @assert span(kid) == mapreduce(span, +, verified_kids(kid); init = 0)

        # If the kid is a K"block" we don't bubble up whitespace since it is more convenient
        # to keep it inside the block (see e.g. `indent_block` which relies on this for
        # making sure blocks starts/ends with a K"NewlineWs").
        if kind(kid) === K"block"
            # If this is a begin-block (or quote-block) we can still bubble up the
            # whitespace since the begin and end tokens are inside of the block. I don't
            # think this currently happens though so for now we just assert.
            if is_begin_block(kid)
                @assert kind(verified_kids(kid)[1]) === K"begin"
                @assert kind(verified_kids(kid)[end]) === K"end"
            elseif is_begin_block(kid, K"quote")
                @assert kind(verified_kids(kid)[1]) === K"quote"
                @assert kind(verified_kids(kid)[end]) === K"end"
            elseif is_paren_block(kid)
                @assert kind(verified_kids(kid)[1]) === K"("
                @assert kind(verified_kids(kid)[end]) === K")"
            end
            continue
        end

        # Extract any leading whitespace and insert it before the node.
        while ((kid′, ws) = pop_if_whitespace!(popfirst!, kid); ws !== nothing)
            @assert kid′ !== nothing
            @assert kind(kid′) === kind(kid)
            kids[i] = kid = kid′
            insert!(kids, i, ws)
            i += 1
        end

        # Extract any trailing whitespace and insert it after the node.
        while ((kid′, ws) = pop_if_whitespace!(pop!, kid); ws !== nothing)
            # Trailing whitespace have only been seen in the wild in these three cases
            @assert kind(kid) in KSet"row nrow parameters"
            @assert kid′ !== nothing
            @assert kind(kid′) === kind(kid)
            kids[i] = kid = kid′
            insert!(kids, i + 1, ws)
        end
    end
    # We only move around things inside this node so the span should be unchanged
    @assert span(node) == mapreduce(span, +, kids; init = 0)
    return node
end

function insert_into_last_else_block(node, ws)
    @assert kind(node) === K"elseif"
    kids = verified_kids(node)
    elseifidx = findfirst(x -> !is_leaf(x) && kind(x) === K"elseif", kids)
    if elseifidx !== nothing
        @assert elseifidx == lastindex(kids)
        elseifnode′ = insert_into_last_else_block(kids[elseifidx], ws)
        @assert elseifnode′ !== nothing
        kids[elseifidx] = elseifnode′
        return make_node(node, kids)
    end
    # Find the else block
    elseifblockidx = findfirst(x -> kind(x) === K"block", kids)::Int
    elseleafidx = findnext(x -> kind(x) === K"else", kids, elseifblockidx + 1)::Int
    elseblockidx = findnext(x -> kind(x) === K"block", kids, elseleafidx + 1)::Int
    @assert elseblockidx == lastindex(kids)
    elseblock = kids[elseblockidx]
    # Insert the node
    elseblockkids = verified_kids(elseblock)
    @assert !(kind(elseblockkids[end]) in KSet"NewlineWs Whitespace")
    push!(elseblockkids, ws)
    # Remake the else block
    kids[elseblockidx] = make_node(elseblock, elseblockkids)
    # Remake and return the elseif node
    return make_node(node, kids)
end

function insert_into_last_catchlike_block(node, ws)
    @assert kind(node) in KSet"catch finally else"
    kids = verified_kids(node)
    catchblockidx = findfirst(x -> kind(x) === K"block", kids)::Int
    @assert catchblockidx == lastindex(kids)
    catchblock = kids[catchblockidx]
    catchblockkids = verified_kids(catchblock)
    @assert !(kind(catchblockkids[end]) in KSet"NewlineWs Whitespace")
    push!(catchblockkids, ws)
    # Remake the catch block
    kids[catchblockidx] = make_node(catchblock, catchblockkids)
    # Remake and return the catch node
    return make_node(node, kids)
end


# Node tags #

# This node is responsible for incrementing the indentation level
const TAG_INDENT = TagType(1) << 0
# This node is responsible for decrementing the indentation level
const TAG_DEDENT = TagType(1) << 1
# This (NewlineWs) node is the last one before a TAG_DEDENT
const TAG_PRE_DEDENT = TagType(1) << 2
# This (NewlineWs) node is a line continuation
const TAG_LINE_CONT = UInt32(1) << 31
# Parameters that should have a trailing comma after last item
const TAG_TRAILING_COMMA = TagType(1) << 4
# Parameters that should optinally have a trailing comma after last item
const TAG_TRAILING_COMMA_OPT = TagType(1) << 5

function add_tag(node::Node, tag::TagType)
    return Node(head(node), span(node), node.kids, node.tags | tag)
end

# Tags all leading NewlineWs nodes as continuation nodes. Note that comments are skipped
# over so that cases like `\n#comment\ncode` works as expected.
function continue_newlines(node::Node; leading::Bool = true, trailing::Bool = true)
    if is_leaf(node)
        if kind(node) === K"NewlineWs" && !has_tag(node, TAG_LINE_CONT)
            return add_tag(node, TAG_LINE_CONT)
        else
            return nothing
        end
    end
    kids = verified_kids(node)
    if length(kids) == 1
        return nothing
    end
    any_kid_changed = false
    if leading
        idx = firstindex(kids) - 1
        while true
            # Skip over whitespace + comments which can mask the newlines
            idx = findnext(x -> !(kind(x) in KSet"Whitespace Comment"), kids, idx + 1)
            if idx === nothing
                # No matching kid found
                break
            elseif kind(kids[idx]) === K"NewlineWs"
                # Kid is a NewlineWs node, tag and keep looking
                kid′ = continue_newlines(kids[idx]; leading = leading, trailing = trailing)
                if kid′ !== nothing
                    kids[idx] = kid′
                    any_kid_changed = false
                end
            else
                # This kid is not Whitespace, Comment or NewlineWs.
                # Recurse but break out of the loop
                kid′ = continue_newlines(kids[idx]; leading = leading, trailing = trailing)
                if kid′ !== nothing
                    kids[idx] = kid′
                    any_kid_changed = false
                end
                break
            end
        end
    end
    if trailing
        idx = lastindex(kids) + 1
        while true
            # Skip over whitespace + comments which can mask the newlines
            idx = findprev(x -> !(kind(x) in KSet"Whitespace Comment"), kids, idx - 1)
            if idx === nothing
                # No matching kid found
                break
            elseif kind(kids[idx]) === K"NewlineWs"
                # Kid is a NewlineWs node, tag and keep looking
                kid′ = continue_newlines(kids[idx]; leading = leading, trailing = trailing)
                if kid′ !== nothing
                    kids[idx] = kid′
                    any_kid_changed = false
                end
            else
                # This kid is not Whitespace, Comment or NewlineWs.
                # Recurse but break out of the loop
                kid′ = continue_newlines(kids[idx]; leading = leading, trailing = trailing)
                if kid′ !== nothing
                    kids[idx] = kid′
                    any_kid_changed = false
                end
                break
            end
        end
    end
    return any_kid_changed ? node : nothing
end

function has_tag(node::Node, tag::TagType)
    return node.tags & tag != 0
end

function stringify_tags(node::Node)
    io = IOBuffer()
    if has_tag(node, TAG_INDENT)
        write(io, "indent,")
    end
    if has_tag(node, TAG_DEDENT)
        write(io, "dedent,")
    end
    if has_tag(node, TAG_PRE_DEDENT)
        write(io, "pre-dedent,")
    end
    if has_tag(node, TAG_LINE_CONT)
        write(io, "line-cont.,")
    end
    if has_tag(node, TAG_TRAILING_COMMA)
        write(io, "trail-comma.,")
    end
    truncate(io, max(0, position(io) - 1)) # Remove trailing comma
    return String(take!(io))
end

# Create a new node with the same head but new kids
function make_node(node::Node, kids′::Vector{Node}, tags = node.tags)
    span′ = mapreduce(span, +, kids′; init = 0)
    return Node(head(node), span′, kids′, tags)
end

function make_node(node::Node, span′::Integer, tags = node.tags)
    @assert is_leaf(node)
    return Node(head(node), span′, (), tags)
end

# TODO: Remove?
first_leaf(node::Node) = nth_leaf(node, 1)

function nth_leaf(node::Node, nth::Int)
    leaf, n_seen = nth_leaf(node, nth, 0)
    return n_seen == nth ? leaf : nothing
end
function nth_leaf(node::Node, nth::Int, n_seen::Int)
    if is_leaf(node)
        return node, n_seen + 1
    else
        kids = verified_kids(node)
        for kid in kids
            leaf, n_seen = nth_leaf(kid, nth, n_seen)
            if n_seen == nth
                return leaf, n_seen
            end
        end
        return nothing, n_seen
    end
end

# Return number of non-whitespace kids, basically the length the equivalent
# (expr::Expr).args
function meta_nargs(node::Node)
    return is_leaf(node) ? 0 : count(!JuliaSyntax.is_whitespace, verified_kids(node))
end

# Replace the first leaf
# TODO: Append the replacement bytes inside this utility function?
function replace_first_leaf(node::Node, kid′::Union{Node, NullNode})
    if is_leaf(node)
        return kid′
    else
        kids′ = copy(verified_kids(node))
        kid′′ = replace_first_leaf(kids′[1], kid′)
        if kid′′ === nullnode
            popfirst!(kids′)
        else
            kids′[1] = kid′′
        end
        # kids′[1] = replace_first_leaf(kids′[1], kid′)
        @assert length(kids′) > 0
        return make_node(node, kids′)
    end
end

function replace_last_leaf(node::Node, kid′::Union{Node, NullNode})
    if is_leaf(node)
        return kid′
    else
        kids′ = copy(verified_kids(node))
        kid′′ = replace_last_leaf(kids′[end], kid′)
        if kid′′ === nullnode
            pop!(kids′)
        else
            kids′[end] = kid′′
        end
        @assert length(kids′) > 0
        return make_node(node, kids′)
    end
end

# Insert a node before the first leaf (at the same level)
# TODO: Currently only works for inserting a space before a comment
function add_before_first_leaf(node::Node, kid′::Union{Node, NullNode})
    @assert !is_leaf(node)
    kids = verified_kids(node)
    @assert length(kids) > 0
    kids′ = copy(kids)
    if kind(kids′[1]) === K"Comment"
        pushfirst!(kids′, kid′)
    else
        kids′[1] = add_before_first_leaf(kids′[1], kid′)
    end
    return make_node(node, kids′)
end

function last_leaf(node::Node)
    if is_leaf(node)
        return node
    else
        kids = verified_kids(node)
        if length(kids) == 0
            return nothing
        else
            return last_leaf(last(kids))
        end
    end
end

function second_last_leaf(node::Node)
    node, n = second_last_leaf(node, 0)
    return n == 2 ? node : nothing
end

function second_last_leaf(node::Node, n_seen::Int)
    if is_leaf(node)
        return node, n_seen + 1
    else
        kids = verified_kids(node)
        for i in reverse(1:length(kids))
            kid, n_seen = second_last_leaf(kids[i], n_seen)
            if n_seen == 2
                return kid, n_seen
            end
        end
    end
    return nothing, n_seen
end

function is_assignment(node::Node)
    return JuliaSyntax.is_prec_assignment(node)
end

# Assignment node but exclude loop variable assignment
function is_variable_assignment(ctx, node::Node)
    return !is_leaf(node) && is_assignment(node) &&
        !(ctx.lineage_kinds[end] in KSet"for generator cartesian_iterator filter")
end

# K"global" and K"local" nodes can be either `global a, b, c` or `global a = 1`. This method
# checks whether the node is of the former kind.
function is_global_local_list(node)
    if !(kind(node) in KSet"global local" && !is_leaf(node))
        return false
    end
    kids = verified_kids(node)
    # If it contains assignments it is not a list
    if any(x -> is_assignment(x), kids)
        return false
    end
    # If it contains K"," it is a list
    if any(x -> kind(x) === K",", kids)
        return true
    end
    # If we reach here we have a single item list (`local a`) or something like
    # ```
    # global function f()
    #     # ...
    # end
    # ```
    # For now we only say it is a list if the item is in the subset below
    idx = findfirst(x -> kind(x) in KSet"global local", kids)::Int
    idx = findnext(x -> !JuliaSyntax.is_whitespace(x), kids, idx + 1)::Int
    return kind(kids[idx]) in KSet"Identifier var"
end

function unwrap_to_call_or_tuple(x)
    is_leaf(x) && return nothing
    @assert !is_leaf(x)
    if kind(x) in KSet"call tuple parens"
        return x
    end
    xkids = verified_kids(x)
    xi = findfirst(x -> !JuliaSyntax.is_whitespace(x), xkids)::Int
    return unwrap_to_call_or_tuple(xkids[xi])
end

# TODO: This should be reworked to be more specific, in particular K"parens" is maybe not
# correct (found in e.g. `function(x * b)\n\nend`).
function is_longform_anon_function(node::Node)
    is_leaf(node) && return false
    kind(node) === K"function" || return false
    kids = verified_kids(node)
    kw = findfirst(x -> kind(x) === K"function", kids)
    @assert kw !== nothing
    sig = findnext(x -> !JuliaSyntax.is_whitespace(x), kids, kw + 1)::Int
    sigkid = kids[sig]
    maybe_tuple = unwrap_to_call_or_tuple(sigkid)
    if maybe_tuple === nothing
        return false
    else
        return kind(maybe_tuple) in KSet"tuple parens"
    end
end

function is_longform_functor(node::Node)
    is_leaf(node) && return false
    kind(node) === K"function" || return false
    kids = verified_kids(node)
    kw = findfirst(x -> kind(x) === K"function", kids)
    @assert kw !== nothing
    calli = findnext(x -> !JuliaSyntax.is_whitespace(x), kids, kw + 1)::Int
    call = kids[calli]
    if !is_leaf(call) && kind(call) == K"call" &&
            kind(first(verified_kids(call))) === K"parens"
        return true
    end
    return false
end

# Just like `JuliaSyntax.is_infix_op_call`, but also check that the node is K"call" or
# K"dotcall"
function is_infix_op_call(node::Node)
    return kind(node) in KSet"call dotcall" && JuliaSyntax.is_infix_op_call(node)
end

# Extract the operator of an infix op call node
function infix_op_call_op(node::Node)
    @assert is_infix_op_call(node) || kind(node) === K"||"
    kids = verified_kids(node)
    first_operand_index = findfirst(!JuliaSyntax.is_whitespace, kids)::Int
    op_index = findnext(JuliaSyntax.is_operator, kids, first_operand_index + 1)::Int
    return kids[op_index]
end

# Comparison leaf or a dotted comparison leaf (.<)
function is_comparison_leaf(node::Node)
    if is_leaf(node) && JuliaSyntax.is_prec_comparison(node)
        return true
    elseif !is_leaf(node) && kind(node) === K"." &&
            meta_nargs(node) == 2 && is_comparison_leaf(verified_kids(node)[2])
        return true
    else
        return false
    end
end

function is_operator_leaf(node::Node)
    return is_leaf(node) && JuliaSyntax.is_operator(node)
end

function first_non_whitespace_kid(node::Node)
    @assert !is_leaf(node)
    kids = verified_kids(node)
    idx = findfirst(!JuliaSyntax.is_whitespace, kids)::Int
    return kids[idx]
end

function is_begin_block(node::Node, token = K"begin")
    @assert token in KSet"begin quote"
    return kind(node) === K"block" && length(verified_kids(node)) > 0 &&
        kind(verified_kids(node)[1]) === token
end

function is_paren_block(node::Node)
    return kind(node) === K"block" && JuliaSyntax.has_flags(node, JuliaSyntax.PARENS_FLAG)
end

function first_leaf_predicate(node::Node, pred::F) where {F}
    if is_leaf(node)
        return pred(node) ? node : nothing
    else
        kids = verified_kids(node)
        for k in kids
            r = first_leaf_predicate(k, pred)
            if r !== nothing
                return r
            end
        end
        return nothing
    end
end

function last_leaf_predicate(node::Node, pred::F) where {F}
    if is_leaf(node)
        return pred(node) ? node : nothing
    else
        kids = verified_kids(node)
        for k in Iterators.reverse(kids)
            r = first_leaf_predicate(k, pred)
            if r !== nothing
                return r
            end
        end
        return nothing
    end
end

function predicate_contains(pred::F, node::Node) where {F}
    if pred(node)::Bool
        return true
    elseif is_leaf(node)
        return false
    else
        for k in verified_kids(node)
            r = predicate_contains(pred, k)
            r && return r
        end
        return false
    end
end

function contains_outer_newline(kids::Vector{Node}, oidx::Int, cidx::Int; recurse = true)
    pred = x -> kind(x) === K"NewlineWs" || !JuliaSyntax.is_whitespace(x)
    for i in (oidx + 1):(cidx - 1)
        kid = kids[i]
        r = first_leaf_predicate(kid, pred)
        if r !== nothing && kind(r) === K"NewlineWs"
            return true
        end
        r = last_leaf_predicate(kid, pred)
        if r !== nothing && kind(r) === K"NewlineWs"
            return true
        end
        if kind(kid) === K"parameters"
            grandkids = verified_kids(kid)
            semiidx = findfirst(x -> kind(x) === K";", grandkids)::Int
            r = contains_outer_newline(verified_kids(kid), semiidx, length(grandkids) + 1)
            if r === true # r can be nothing so `=== true` is intentional
                return true
            end
        end
    end
    return false
end

function any_leaf(pred::F, node::Node) where {F}
    if is_leaf(node)
        return pred(node)::Bool
    else
        kids = verified_kids(node)
        for k in kids
            any_leaf(pred, k) && return true
        end
        return false
    end
end

# TODO: Alternative non-recursive definition that only looks at the current layer
# ```
# contains_outer_newline(kids, opening_leaf_idx, closing_leaf_idx)
# ```
function is_multiline_between_idxs(ctx, node::Node, opening_idx::Int, closing_idx::Int)
    @assert !is_leaf(node)
    kids = verified_kids(node)
    # Check for newline nodes
    if any(y -> any_leaf(x -> kind(x) === K"NewlineWs", kids[y]), (opening_idx + 1):(closing_idx - 1))
        return true
    end
    # Recurse into multiline triple-strings
    pos = position(ctx.fmt_io)
    for i in 1:opening_idx
        accept_node!(ctx, kids[i])
    end
    for i in (opening_idx + 1):(closing_idx - 1)
        kid = kids[i]
        ipos = position(ctx.fmt_io)
        if contains_multiline_triple_string(ctx, kid)
            seek(ctx.fmt_io, pos)
            return true
        end
        seek(ctx.fmt_io, ipos)
        accept_node!(ctx, kid)
    end
    seek(ctx.fmt_io, pos)
    return false
end

function contains_multiline_triple_string(ctx, node::Node)
    # If this is a leaf just advance the stream
    if is_leaf(node)
        accept_node!(ctx, node)
        return false
    end
    kids = verified_kids(node)
    pos = position(ctx.fmt_io)
    # If this is a triple string we inspect it
    if kind(node) in KSet"string cmdstring" && JuliaSyntax.has_flags(node, JuliaSyntax.TRIPLE_STRING_FLAG)
        triplekind, triplestring, itemkind = kind(node) === K"string" ?
            (K"\"\"\"", "\"\"\"", K"String") : (K"```", "```", K"CmdString")
        # Look for K"String"s ending in `\n`
        for (i, kid) in pairs(kids)
            if i === firstindex(kids) || i === lastindex(kids)
                @assert kind(kid) === triplekind
                @assert String(read_bytes(ctx, kid)) == triplestring
            end
            if kind(kid) === itemkind
                if endswith(String(read_bytes(ctx, kid)), "\n")
                    return true
                end
            end
            accept_node!(ctx, kid)
        end
        @assert position(ctx.fmt_io) == pos + span(node)
    else
        for kid in kids
            kpos = position(ctx.fmt_io)
            if contains_multiline_triple_string(ctx, kid)
                return true
            end
            seek(ctx.fmt_io, kpos)
            accept_node!(ctx, kid)
        end
    end
    return false
end

function is_string_macro(node)
    kind(node) === K"macrocall" || return false
    @assert !is_leaf(node)
    kids = verified_kids(node)
    return length(kids) >= 2 &&
        kind(kids[1]) in KSet"StringMacroName CmdMacroName core_@cmd" &&
        kind(kids[2]) in KSet"string cmdstring"
end

function is_triple_string(node)
    return kind(node) in KSet"string cmdstring" &&
        JuliaSyntax.has_flags(node, JuliaSyntax.TRIPLE_STRING_FLAG)
end

function is_triple_string_macro(node)
    if kind(node) === K"macrocall"
        kids = verified_kids(node)
        if length(kids) >= 2 &&
                kind(kids[1]) in KSet"StringMacroName CmdMacroName core_@cmd" &&
                is_triple_string(kids[2])
            return true
        end
    end
    return false
end

function is_triple_thing(node)
    return is_triple_string(node) || is_triple_string_macro(node) ||
        (kind(node) === K"juxtapose" && is_triple_string_macro(verified_kids(node)[1]))
end

# Check whether the sequence of kinds in `kinds` exist in `kids` starting at index `i`.
function kmatch(kids, kinds, i = firstindex(kids))
    if i < 1 || i + length(kinds) - 1 > length(kids)
        return false
    end
    for (j, k) in pairs(kinds)
        if kind(kids[i + j - 1]) !== k
            return false
        end
    end
    return true
end

# Extract the macro name as written in the source.
function macrocall_name(ctx, node)
    @assert kind(node) === K"macrocall"
    kids = verified_kids(node)
    pred = x -> kind(x) in KSet"MacroName StringMacroName CmdMacroName core_@cmd"
    mkind = kind(first_leaf_predicate(node, pred)::Node)
    if kmatch(kids, KSet"@ MacroName")
        p = position(ctx.fmt_io)
        bytes = read(ctx.fmt_io, span(kids[1]) + span(kids[2]))
        seek(ctx.fmt_io, p)
        return String(bytes)
    elseif kmatch(kids, KSet".") || kmatch(kids, KSet"CmdMacroName") ||
            kmatch(kids, KSet"StringMacroName")
        bytes = read_bytes(ctx, kids[1])
        if mkind === K"CmdMacroName"
            append!(bytes, "_cmd")
        elseif mkind === K"StringMacroName"
            append!(bytes, "_str")
        end
        return String(bytes)
    elseif kmatch(kids, KSet"core_@cmd")
        bytes = read_bytes(ctx, kids[1])
        @assert length(bytes) == 0
        return "core_@cmd"
    else
        # Don't bother looking in more complex expressions, just return unknown
        return "<unknown macro>"
    end
end

# Inserting `return` modifies the AST in a way that is visible to macros.. In general it is
# never safe to change the AST inside a macro, but we make an exception for some common
# "known" macros in order to be able to format functions that e.g. have an `@inline`
# annotation in front.
const MACROS_SAFE_TO_INSERT_RETURN = let set = Set{String}()
    for m in (
            "inline", "noinline", "propagate_inbounds", "generated", "eval",
            "assume_effects", "doc",
        )
        push!(set, "@$m", "Base.@$m")
    end
    push!(set, "Core.@doc")
    set
end
function safe_to_insert_return(ctx, node)
    for m in ctx.lineage_macros
        m in MACROS_SAFE_TO_INSERT_RETURN || return false
    end
    return true
end

##########################
# Utilities for IOBuffer #
##########################

# Replace bytes for a node at the current position in the IOBuffer. `size` is the current
# window for the node, i.e. the number of bytes until the next node starts. If `size` is
# smaller or larger than the length of `bytes` this method will shift the bytes for
# remaining nodes to the left or right. Return number of written bytes.
function replace_bytes!(io::IOBuffer, bytes::Union{String, AbstractVector{UInt8}}, size::Int)
    pos = position(io)
    nb = (bytes isa AbstractVector{UInt8} ? length(bytes) : sizeof(bytes))
    if nb == size
        nw = write(io, bytes)
        @assert nb == nw
    else
        backup = IOBuffer() # TODO: global const (with lock)?
        seek(io, pos + size)
        @assert position(io) == pos + size
        nb_written_to_backup = write(backup, io)
        seek(io, pos)
        @assert position(io) == pos
        nw = write(io, bytes)
        @assert nb == nw
        nb_read_from_backup = write(io, seekstart(backup))
        @assert nb_written_to_backup == nb_read_from_backup
        truncate(io, position(io))
    end
    seek(io, pos)
    @assert position(io) == pos
    return nb
end

replace_bytes!(io::IOBuffer, bytes::Union{String, AbstractVector{UInt8}}, size::Integer) =
    replace_bytes!(io, bytes, Int(size))

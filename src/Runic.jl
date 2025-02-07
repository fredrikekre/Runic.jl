# SPDX-License-Identifier: MIT

module Runic

if VERSION >= v"1.11.0-DEV.469"
    let str = "public format_file, format_string"
        eval(Meta.parse(str))
    end
end

using JuliaSyntax:
    JuliaSyntax, @K_str, @KSet_str

const RUNIC_VERSION = pkgversion(@__MODULE__)
# XXX: This should probably be handled by Base.
Base.include_dependency(joinpath(pkgdir(@__MODULE__), "Project.toml"))

# Debug and assert utilities
include("debug.jl")
include("JuliaSyntax.jl")

########
# Node #
########

const TagType = UInt32

struct NullNode end
const nullnode = NullNode()

# This is essentially just a re-packed `JuliaSyntax.GreenNode`.
struct Node
    # The next three fields directly match JuliaSyntax.GreenNode. We can not store a
    # GreenNode directly because the type of the children vector should be `Vector{Node}`
    # and not `Vector{GreenNode}`.
    head::JuliaSyntax.SyntaxHead
    span::UInt32
    kids::Union{Tuple{}, Vector{Node}}
    # Metadata for the formatter
    tags::TagType
end

function Node(head::JuliaSyntax.SyntaxHead, span::Integer, tags::Integer = 0)
    return Node(head, span % UInt32, (), tags % TagType)
end

function Node(head::JuliaSyntax.SyntaxHead, kids::Vector{Node})
    spn = mapreduce(span, +, kids; init = 0)
    return Node(head, spn % UInt32, kids, 0 % TagType)
end

# Re-package a GreenNode as a Node
function Node(node::JuliaSyntax.GreenNode)
    tags = 0 % TagType
    # juliac: this is `kids = map(Node, JuliaSyntax.children(node))` but written out like
    # this in order to help inference.
    children = JuliaSyntax.children(node)
    if children isa Tuple{}
        kids = ()
    else
        kids = Vector{Node}(undef, length(children))
        for (i, child) in pairs(children)
            kids[i] = Node(child)
        end
    end
    return Node(JuliaSyntax.head(node), JuliaSyntax.span(node), kids, tags)
end

function Base.show(io::IO, ::MIME"text/plain", node::Node)
    show(io, node)
    println(io)
    _show_green_node(io, node, "", 1, nothing, true)
    return
end

function Base.show(io::IO, node::Node)
    print(io, "Node({head: {kind: ")
    show(io, kind(node))
    print(io, ", flags: \"$(stringify_flags(node))\"}, span: $(span(node)), tags: \"$(stringify_tags(node))\"})")
    return nothing
end

# Defining these allow using many duck-typed methods in JuliaSyntax directly without having
# to re-package a Node as a GreenNode.
JuliaSyntax.head(node::Node) = head(node)
JuliaSyntax.span(node::Node) = span(node)

# Matching JuliaSyntax.(head|span|flags|kind)
head(node::Node) = node.head
span(node::Node) = node.span
tags(node::Node) = node.tags
flags(node::Node) = JuliaSyntax.flags(node)
kind(node::Node) = JuliaSyntax.kind(node)

# Inverse of JuliaSyntax.haschildren
function is_leaf(node::Node)
    return node.kids === ()
end

# This function must only be be called after verifying that the node is not a leaf. We can
# then type-assert the return value to narrow it down from `Union{Tuple{}, Vector{Node}}` to
# `Vector{Node}`.
function verified_kids(node::Node)
    @assert !is_leaf(node)
    return node.kids::Vector{Node}
end

# Node utilities and JuliaSyntax extensions
include("chisels.jl")

# Return the result of expr if it doesn't evaluate to `nothing`
macro return_something(expr)
    return :(
        let node = $(esc(expr))
            node === nothing || return node
        end
    )
end

#######################################################
# Main drivers for traversing and formatting the tree #
#######################################################

mutable struct Context
    # Input
    const src_str::String
    const src_tree::Node
    const src_io::IOBuffer
    # Output
    const fmt_io::IOBuffer
    fmt_tree::Union{Node, Nothing}
    # User settings
    quiet::Bool
    verbose::Bool
    assert::Bool
    debug::Bool
    check::Bool
    diff::Bool
    filemode::Bool
    filename::String
    line_ranges::Vector{UnitRange{Int}}
    # Global state
    indent_level::Int # track (hard) indentation level
    call_depth::Int # track call-depth level for debug printing
    format_on::Bool
    # Current state
    # node::Union{Node, Nothing}
    prev_sibling::Union{Node, Nothing}
    next_sibling::Union{Node, Nothing}
    # parent::Union{Node, Nothing}
    lineage_kinds::Vector{JuliaSyntax.Kind}
    lineage_macros::Vector{String}
end

const RANGE_FORMATTING_BEGIN = "#= RUNIC RANGE FORMATTING " * "BEGIN =#"
const RANGE_FORMATTING_END = "#= RUNIC RANGE FORMATTING " * "END =#"

function add_line_range_markers(str, line_ranges)
    lines = collect(eachline(IOBuffer(str); keep = true))
    sort!(line_ranges, rev = true)
    for r in line_ranges
        a, b = extrema(r)
        if a < 1 || b > length(lines)
            throw(MainError("`--lines` range out of bounds"))
        end
        if b == length(lines) && !endswith(lines[end], "\n")
            lines[end] *= "\n"
        end
        insert!(lines, b + 1, RANGE_FORMATTING_END * "\n")
        insert!(lines, a, RANGE_FORMATTING_BEGIN * "\n")
    end
    io = IOBuffer(; maxsize = sum(sizeof, lines; init = 0))
    join(io, lines)
    src_str = String(take!(io))
    return src_str
end

function remove_line_range_markers(src_io, fmt_io)
    src_lines = eachline(seekstart(src_io); keep = true)
    fmt_lines = eachline(seekstart(fmt_io); keep = true)
    io = IOBuffer()
    # These can't fail because we will at the minimum have the begin/end comments
    src_itr = iterate(src_lines)
    @assert src_itr !== nothing
    src_ln, src_token = src_itr
    itr_fmt = iterate(fmt_lines)
    @assert itr_fmt !== nothing
    fmt_ln, fmt_token = itr_fmt
    eof = false
    while true
        # Take source lines until range start or eof
        while !occursin(RANGE_FORMATTING_BEGIN, src_ln)
            if !occursin(RANGE_FORMATTING_END, src_ln)
                write(io, src_ln)
            end
            src_itr = iterate(src_lines, src_token)
            if src_itr === nothing
                eof = true
                break
            end
            src_ln, src_token = src_itr
        end
        eof && break
        @assert occursin(RANGE_FORMATTING_BEGIN, src_ln) &&
            strip(src_ln) == RANGE_FORMATTING_BEGIN
        # Skip ahead in the source lines until the range end
        while !occursin(RANGE_FORMATTING_END, src_ln)
            src_itr = iterate(src_lines, src_token)
            @assert src_itr !== nothing
            src_ln, src_token = src_itr
        end
        @assert occursin(RANGE_FORMATTING_END, src_ln) &&
            strip(src_ln) == RANGE_FORMATTING_END
        # Skip ahead in the formatted lines until range start
        while !occursin(RANGE_FORMATTING_BEGIN, fmt_ln)
            fmt_itr = iterate(fmt_lines, fmt_token)
            @assert fmt_itr !== nothing
            fmt_ln, fmt_token = fmt_itr
        end
        @assert occursin(RANGE_FORMATTING_BEGIN, fmt_ln) &&
            strip(fmt_ln) == RANGE_FORMATTING_BEGIN
        # Take formatted lines until range end
        while !occursin(RANGE_FORMATTING_END, fmt_ln)
            if !occursin(RANGE_FORMATTING_BEGIN, fmt_ln)
                write(io, fmt_ln)
            end
            fmt_itr = iterate(fmt_lines, fmt_token)
            @assert fmt_itr !== nothing
            fmt_ln, fmt_token = fmt_itr
        end
        @assert occursin(RANGE_FORMATTING_END, fmt_ln) &&
            strip(fmt_ln) == RANGE_FORMATTING_END
        eof && break
    end
    write(seekstart(fmt_io), take!(io))
    truncate(fmt_io, position(fmt_io))
    return
end

function Context(
        src_str::String; assert::Bool = true, debug::Bool = false, verbose::Bool = debug,
        diff::Bool = false, check::Bool = false, quiet::Bool = false, filemode::Bool = true,
        line_ranges::Vector{UnitRange{Int}} = UnitRange{Int}[], filename::String = "-",
    )
    if !isempty(line_ranges)
        # If formatting is limited to certain line ranges we modify the source string to
        # include begin and end marker comments.
        src_str = add_line_range_markers(src_str, line_ranges)
    end
    src_io = IOBuffer(src_str)
    # TODO: If parsing here fails, and we have line ranges, perhaps try to parse without the
    # markers to check whether the markers are the cause of the failure.
    src_tree = Node(
        JuliaSyntax.parseall(
            JuliaSyntax.GreenNode, src_str;
            filename = filename, ignore_warnings = true, version = v"2-"
        )
    )
    normalize_tree!(src_tree)
    fmt_io = IOBuffer()
    fmt_tree = nothing
    # Set up buffers
    src_pos = position(src_io)
    @assert src_pos == 0
    fmt_pos = position(fmt_io)
    @assert fmt_pos == 0
    nb = write(fmt_io, read(src_io, span(src_tree)))
    @assert nb == span(src_tree)
    # Reset IO positions to the beginning
    seek(src_io, src_pos)
    seek(fmt_io, fmt_pos)
    # Debug mode enforces verbose and assert
    verbose = debug ? true : verbose
    assert = debug ? true : assert
    indent_level = 0
    call_depth = 0
    prev_sibling = next_sibling = nothing
    lineage_kinds = JuliaSyntax.Kind[]
    lineage_macros = String[]
    format_on = true
    return Context(
        src_str, src_tree, src_io, fmt_io, fmt_tree, quiet, verbose, assert, debug, check,
        diff, filemode, filename, line_ranges, indent_level, call_depth, format_on,
        prev_sibling, next_sibling, lineage_kinds, lineage_macros
    )
end

function next_sibling_kind(ctx::Context)::Union{JuliaSyntax.Kind, Nothing}
    next = ctx.next_sibling
    return next === nothing ? nothing : JuliaSyntax.kind(next)
end

# Read the bytes of the current node from the output io
function read_bytes(ctx, node)
    pos = position(ctx.fmt_io)
    bytes = read(ctx.fmt_io, span(node))
    @assert length(bytes) == span(node)
    seek(ctx.fmt_io, pos)
    @assert position(ctx.fmt_io) == pos
    return bytes
end

function accept_node!(ctx::Context, node::Node)
    # Accept the string representation of the current node by advancing the
    # output IO to the start of the next node
    pos = position(ctx.fmt_io) + span(node)
    seek(ctx.fmt_io, pos)
    return
end

function replace_bytes!(ctx::Context, bytes::Union{String, AbstractVector{UInt8}}, sz::Integer)
    return replace_bytes!(ctx.fmt_io, bytes, Int(sz))
end

# Validate the toggle comments
function validate_toggle(ctx, kids, i)
    toplevel = length(ctx.lineage_kinds) == 1 && ctx.lineage_kinds[1] === K"toplevel"
    valid = true
    prev = get(kids, i - 1, nothing)
    if prev === nothing
        valid &= toplevel && i == 1
    else
        valid &= kind(prev) === K"NewlineWs" || (toplevel && i == 1 && kind(prev) === K"Whitespace")
    end
    next = get(kids, i + 1, nothing)
    if next === nothing
        valid &= toplevel && i == lastindex(kids)
    else
        valid &= kind(next) === K"NewlineWs"
    end
    return valid
end

function check_format_toggle(ctx::Context, node::Node, kid::Node, i::Int)::Union{Int, Nothing}
    @assert ctx.format_on
    @assert !is_leaf(node)
    kids = verified_kids(node)
    @assert kid === kids[i]
    # Check if the kid is a comment
    kind(kid) === K"Comment" || return nothing
    # Check the comment content
    reg = r"#(!)? (runic|format): (on|off)"
    str = String(read_bytes(ctx, kid))
    offmatch = match(reg, str)
    offmatch === nothing && return nothing
    toggle = offmatch.captures[3]::AbstractString
    if toggle == "on"
        # @debug "Ignoring `$(offmatch.match)` toggle since formatting is already on."
        return nothing
    end
    if !validate_toggle(ctx, kids, i)
        # @debug "Ignoring `$(offmatch.match)` toggle since it is not on a separate line."
        return nothing
    end
    # Find a matching closing toggle
    pos = position(ctx.fmt_io)
    accept_node!(ctx, kid)
    for j in (i + 1):length(kids)
        lkid = kids[j]
        if kind(lkid) !== K"Comment"
            accept_node!(ctx, lkid)
            continue
        end
        str = String(read_bytes(ctx, lkid))
        onmatch = match(reg, str)
        if onmatch === nothing
            accept_node!(ctx, lkid)
            continue
        end
        # Check that the comments match in style
        if offmatch.captures[1] != onmatch.captures[1] ||
                offmatch.captures[2] != onmatch.captures[2]
            # @debug "Ignoring `$(onmatch.match)` toggle since it doesn't match the " *
            #     "style of the `$(offmatch.match)` toggle."
            accept_node!(ctx, lkid)
            continue
        end
        toggle = onmatch.captures[3]::AbstractString
        if toggle == "off"
            # @debug "Ignoring `$(onmatch.match)` toggle since formatting is already off."
            accept_node!(ctx, lkid)
            continue
        end
        @assert toggle == "on"
        if !validate_toggle(ctx, kids, j)
            # @debug "Ignoring `$(onmatch.match)` toggle since it is not on a separate line."
            accept_node!(ctx, lkid)
            continue
        end
        seek(ctx.fmt_io, pos)
        return j
    end
    # Reset the stream
    seek(ctx.fmt_io, pos)
    # No closing toggle found. This is allowed as a top level statement so that complete
    # files can be ignored by just a comment at the top.
    if length(ctx.lineage_kinds) == 1 && ctx.lineage_kinds[1] === K"toplevel"
        return typemax(Int)
    end
    # @debug "Ignoring `$(offmatch.match)` toggle since no matching `on` toggle " *
    #     "was found at the same tree level."
    return nothing
end

function format_node_with_kids!(ctx::Context, node::Node)
    # If the node doesn't have kids there is nothing to do here
    if is_leaf(node)
        return nothing
    end

    ctx.call_depth += 1

    # Keep track of the siblings on this stack
    prev_sibling = ctx.prev_sibling
    next_sibling = ctx.next_sibling
    ctx.prev_sibling = nothing
    ctx.next_sibling = nothing
    push!(ctx.lineage_kinds, kind(node))
    if kind(node) === K"macrocall"
        push!(ctx.lineage_macros, macrocall_name(ctx, node))
    end

    # The new node parts. `kids′` aliases `kids` and only copied below if any of the
    # nodes change ("copy-on-write").
    kids = verified_kids(node)
    kids′ = kids
    any_kid_changed = false

    # This method should never be called if formatting is off for this node
    @assert ctx.format_on
    format_on_idx = typemin(Int)

    # Loop over all the kids
    for (i, kid) in pairs(kids)
        # Set the siblings: previous from kids′, next from kids
        ctx.prev_sibling = get(kids′, i - 1, nothing)
        ctx.next_sibling = get(kids, i + 1, nothing)
        kid′ = kid
        this_kid_changed = false
        itr = 0
        # Check if this kid toggles formatting off
        if ctx.format_on && i > format_on_idx
            format_on_idx′ = check_format_toggle(ctx, node, kid, i)
            if format_on_idx′ !== nothing
                ctx.format_on = false
                format_on_idx = format_on_idx′
            end
        elseif !ctx.format_on && i > format_on_idx - 2
            # The formatter is turned on 2 steps before so that we can format
            # the indent of the `#! format: on` comment.
            ctx.format_on = true
        end
        # Loop until this node reaches a steady state and is accepted
        while true
            # Keep track of the stream position and reset it below if the node is changed
            fmt_pos = position(ctx.fmt_io)
            # Format the kid
            kid′′ = format_node!(ctx, kid′)
            if kid′′ === nullnode
                # This node should be deleted from the tree
                # TODO: When this is fixed the sibling setting above needs to be modified to
                # handle this too
                this_kid_changed = true
                error("TODO: handle removed kids")
            elseif kid′′ === nothing
                # The node was accepted, continue to next sibling
                @assert position(ctx.fmt_io) == fmt_pos + span(kid′)
                break
            else
                # The node should be replaced with the new one. Reset the stream and try
                # again until it is accepted.
                @assert kid′′ isa Node
                if !is_leaf(kid′′)
                    @assert span(kid′′) == mapreduce(span, +, verified_kids(kid′′); init = 0)
                end
                this_kid_changed = true
                seek(ctx.fmt_io, fmt_pos)
                kid′ = kid′′
            end
            if (itr += 1) == 1000
                error("infinite loop?")
            end
        end
        any_kid_changed |= this_kid_changed
        if any_kid_changed
            # De-alias the kids if not already done
            if kids′ === kids
                kids′ = eltype(kids)[kids[j] for j in 1:(i - 1)]
            end
            push!(kids′, kid′)
        end
    end
    # Reset the siblings
    ctx.prev_sibling = prev_sibling
    ctx.next_sibling = next_sibling
    pop!(ctx.lineage_kinds)
    if kind(node) === K"macrocall"
        pop!(ctx.lineage_macros)
    end
    ctx.call_depth -= 1
    # Return a new node if any of the kids changed
    if any_kid_changed
        return make_node(node, kids′)
    else
        return nothing
    end
end

"""
    format_node!(ctx::Context, node::JuliaSyntax.GreenNode)

Format a node. Return values:
 - `nothing::Nothing`: The node is accepted as is
 - `nullnode::NullNode`: The node should be deleted from the tree
 - `node::JuliaSyntax.GreenNode`: The node should be replaced with the new node
"""
function format_node!(ctx::Context, node::Node)::Union{Node, Nothing, NullNode}
    # If formatting is off just return
    if !ctx.format_on
        accept_node!(ctx, node)
        return nothing
    end
    node_kind = kind(node)

    # Not that two separate `if`s are used here because a node like `else` can be both
    # dedent and indent
    if has_tag(node, TAG_INDENT)
        ctx.indent_level += 1
    end
    if has_tag(node, TAG_DEDENT)
        ctx.indent_level -= 1
    end

    # Go through the runestone and apply transformations.
    ctx.call_depth += 1
    @return_something replace_tabs_with_four_spaces(ctx, node)
    @return_something no_leading_and_single_trailing_newline(ctx, node)
    @return_something max_three_consecutive_newlines(ctx, node)
    @return_something insert_delete_mark_newlines(ctx, node)
    @return_something remove_trailing_semicolon(ctx, node)
    @return_something trim_trailing_whitespace(ctx, node)
    @return_something format_hex_literals(ctx, node)
    @return_something format_float_literals(ctx, node)
    @return_something spaces_around_operators(ctx, node)
    @return_something spaces_around_assignments(ctx, node)
    @return_something spaces_around_anonymous_function(ctx, node)
    @return_something spaces_around_ternary(ctx, node)
    @return_something spaces_around_keywords(ctx, node)
    @return_something spaces_in_import_using(ctx, node)
    @return_something spaces_in_export_public(ctx, node)
    @return_something spaces_in_let(ctx, node)
    @return_something spaces_around_comments(ctx, node)
    @return_something no_spaces_around_colon_etc(ctx, node)
    @return_something parens_around_op_calls_in_colon(ctx, node)
    @return_something for_loop_use_in(ctx, node)
    @return_something explicit_return(ctx, node)
    @return_something braces_around_where_rhs(ctx, node)
    @return_something indent_multiline_strings(ctx, node)
    @return_something four_space_indent(ctx, node)
    @return_something spaces_in_listlike(ctx, node)
    ctx.call_depth -= 1

    # If none of the transformations above changed the node (and thus returned back up one
    # level before recursing down here again) we i) accept it if it is a leaf or ii) recurse
    # one level depeer.
    if is_leaf(node)
        accept_node!(ctx, node)
        return nothing
    else
        return format_node_with_kids!(ctx, node)
    end
end

# Entrypoint
function format_tree!(ctx::Context)
    root = ctx.src_tree
    # Verify buffers
    @assert position(ctx.src_io) == 0
    fmt_pos = position(ctx.fmt_io)
    @assert fmt_pos == 0
    # Set the root to the current node
    root′ = root
    itr = 0
    while true
        # Format the node.
        root′′ = format_node!(ctx, root′)
        if root′′ === nullnode
            # This signals that the node should be deleted, but that doesn't make sense for
            # the root node so error instead
            error("root node deleted")
        elseif root′′ === nothing
            # root′ = root′′
            @assert position(ctx.fmt_io) == fmt_pos + span(root′)
            break
        else
            @assert root′′ isa Node
            # The node was changed, reset the output stream and try again
            seek(ctx.fmt_io, fmt_pos)
            root′ = root′′
        end
        # The root node must only change once.
        if (itr += 1) > 100
            error("root node modified more than 100 times?")
        end
    end
    # Truncate the output at the root span
    truncate(ctx.fmt_io, span(root′))
    # Remove line range markers if any
    if !isempty(ctx.line_ranges)
        remove_line_range_markers(ctx.src_io, ctx.fmt_io)
    end
    # Check that the output is parseable
    try
        fmt_str = String(read(seekstart(ctx.fmt_io)))
        # TODO: parsing may fail here because of the removal of the range comments
        JuliaSyntax.parseall(JuliaSyntax.GreenNode, fmt_str; ignore_warnings = true, version = v"2-")
    catch
        throw(AssertionError("re-parsing the formatted output failed"))
    end
    # Set the final tree
    # TODO: When range formatting this doesn't match the content of ctx.fmt_io
    ctx.fmt_tree = root′
    return nothing
end

"""
    Runic.format_string(str::AbstractString) -> String

Format string `str` and return the formatted string.
"""
function format_string(str::AbstractString; filemode::Bool = false)
    ctx = Context(str; filemode = filemode, filename = "string")
    format_tree!(ctx)
    return String(take!(ctx.fmt_io))
end

# TODO: Implement the check and diff options here too.
"""
    Runic.format_file(
        inputfile::AbstractString, outputfile::AbstractString = inputfile;
        inplace::Bool=false,
    )

Format file `inputfile` and write the formatted text to `outputfile`.

Setting the keyword argument `inplace = true` is required if `inputfile` and `outputfile`
are the same file.
"""
function format_file(inputfile::AbstractString, outputfile::AbstractString = inputfile; inplace::Bool = false)
    # Argument handling
    inputfile = normpath(abspath(String(inputfile)))
    outputfile = normpath(abspath(String(outputfile)))
    str = read(inputfile, String)
    if !inplace && (outputfile == inputfile || (isfile(outputfile) && samefile(inputfile, outputfile)))
        error("input and output must not be the same when `inplace = false`")
    end
    # Format it
    ctx = Context(str; filename = inputfile)
    format_tree!(ctx)
    # Write the output but skip if it text didn't change
    changed = ctx.fmt_tree !== nothing
    if changed || !inplace
        write(outputfile, take!(ctx.fmt_io))
    end
    return
end

include("runestone.jl")
include("main.jl")

# Precompile the entry points
@assert precompile(main, (Vector{String},))
@assert precompile(format_file, (String, String))
@assert precompile(format_string, (String,))

end # module

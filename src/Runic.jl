module Runic

using JuliaSyntax:
    JuliaSyntax, @K_str, @KSet_str

# compat for const fields
@eval macro $(Symbol("const"))(field)
    if VERSION >= v"1.8.0-DEV.1148"
        Expr(:const, esc(field))
    else
        return esc(field)
    end
end

mutable struct Context
    # Input
    @const src_str::String
    @const src_tree::JuliaSyntax.GreenNode
    @const src_io::IOBuffer
    # Output
    @const fmt_io::IOBuffer
    fmt_tree::Union{JuliaSyntax.GreenNode, Nothing}
    # User settings
    verbose::Bool
    debug::Bool
end

function Context(src_str; debug::Bool = false, verbose::Bool = debug)
    src_io = IOBuffer(src_str)
    src_tree = JuliaSyntax.parseall(JuliaSyntax.GreenNode, src_str; ignore_warnings=true)
    fmt_io = IOBuffer()
    fmt_tree = nothing
    return Context(src_str, src_tree, src_io, fmt_io, fmt_tree, verbose, debug)
end

# Read the bytes of the current node from the output io
function node_bytes(ctx, node)
    pos = mark(ctx.fmt_io)
    bytes = read(ctx.fmt_io, JuliaSyntax.span(node))
    reset(ctx.fmt_io)
    @assert position(ctx.fmt_io) == pos
    return bytes
end

function accept_node!(ctx::Context, node::JuliaSyntax.GreenNode)
    # Accept the string representation of the current node by advancing the
    # output IO to the start of the next node
    pos = position(ctx.fmt_io) + JuliaSyntax.span(node)
    seek(ctx.fmt_io, pos)
    return
end

# Write formatted thing and reset the output stream
function write_and_reset(ctx::Context, bytes::Union{String, SubString{String}, Vector{UInt8}})
    fmt_pos = position(ctx.fmt_io)
    nb = write(ctx.fmt_io, bytes)
    seek(ctx.fmt_io, fmt_pos)
    @assert nb == (bytes isa Vector{UInt8} ? length(bytes) : sizeof(bytes))
    return nb
end

struct NullNode end
const nullnode = NullNode()

function format_node_with_children!(ctx::Context, node::JuliaSyntax.GreenNode)
    if !JuliaSyntax.haschildren(node)
        return node
    end
    # @assert JuliaSyntax.haschildren(node)
    span_sum = 0
    original_bytes = node_bytes(ctx, node) # TODO: Read into reusable buffer
    children = JuliaSyntax.children(node)
    # The new node parts
    head′ = JuliaSyntax.head(node)
    children′ = ()
    # Keep track of changes; if no child changes the original node can be returned
    any_child_changed = false
    for (i, child) in pairs(children)
        child′ = child
        span_sum += JuliaSyntax.span(child)
        this_child_changed = false
        itr = 0
        while true
            # Format this node
            fmt_pos = position(ctx.fmt_io)
            child′′ = format_node!(ctx, child′)
            if child′′ === nullnode
                this_child_changed = true
                error("TODO: handle removed children")
            elseif child′′ === child′
                child′ = child′′
                @assert position(ctx.fmt_io) == fmt_pos + JuliaSyntax.span(child′)
                break
            else
                this_child_changed = true
                # any_changed = true
                # Reset the output stream and go again
                seek(ctx.fmt_io, fmt_pos)
                child′ = child′′
            end
            if (itr += 1) == 1000
                error("infinite loop?")
            end
        end
        if this_child_changed
            # If the node change we have to re-write the original bytes for the next
            # children
            remaining_bytes = @view original_bytes[(span_sum+1):end]
            fmt_pos = position(ctx.fmt_io)
            nb = write(ctx.fmt_io, remaining_bytes)
            seek(ctx.fmt_io, fmt_pos)
            @assert nb == length(remaining_bytes)
        end
        any_child_changed |= this_child_changed
        if any_child_changed
            # Promote children from tuple to array and copy older siblings into it
            if children′ === ()
                children′ = eltype(children)[children[j] for j in 1:(i-1)]
            end
            push!(children′, child′)
        end
    end
    if any_child_changed
        span′ = mapreduce(JuliaSyntax.span, +, children′; init=0)
        return JuliaSyntax.GreenNode(head′, span′, children′)
    else
        return node
    end
end

function format_node!(ctx::Context, node::JuliaSyntax.GreenNode)
    node_kind = JuliaSyntax.kind(node)

    # TODO: Split these into matchers and a handlers and move to another file
    # Normalize line endings and remove trailing whitespace
    if node_kind === K"NewlineWs"
        @assert !JuliaSyntax.haschildren(node)
        str = String(node_bytes(ctx, node))
        str′ = replace(str, r"\h*(\r\n|\r|\n)" => '\n')
        if str != str′
            # Write new bytes and reset the stream
            nb = write_and_reset(ctx, str′)
            @assert nb != JuliaSyntax.span(node)
            # Create new node and return it
            node′ = JuliaSyntax.GreenNode(JuliaSyntax.head(node), nb, ())
            return node′
        end
    end

    # Hex literals
    if node_kind === K"HexInt"
        @assert JuliaSyntax.flags(node) == 0
        @assert !JuliaSyntax.haschildren(node)
        span = JuliaSyntax.span(node)
        @assert span > 2 # 0x prefix
        target_spans = 2 .+ (2, 4, 8, 16, 32) # 0x + expected chars
        if span < 34 && !(span in target_spans)
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
        else
            # Do nothing: correctly formatted or a BigInt hex literal
        end
    end

    # Oct literals
    if node_kind === K"OctInt"
        @assert JuliaSyntax.flags(node) == 0
        @assert !JuliaSyntax.haschildren(node)
        span = JuliaSyntax.span(node)
        # Padding depends on the value of the literal
        str = String(node_bytes(ctx, node))
        n = tryparse(UInt128, str)
        if n !== nothing
            target_span_from_value =
                n <= typemax(UInt8) ? 5 : n <= typemax(UInt16) ? 8 :
                n <= typemax(UInt32) ? 13 : n <= typemax(UInt64) ? 24 :
                n <= typemax(UInt128) ? 45 : error("unreachable")
            target_spans = (5, 8, 13, 24, 45)
            i = findfirst(x -> x >= span, target_spans)::Int
            target_span_from_source = target_spans[i]
            target_span = max(target_span_from_value, target_span_from_source)
            if span != target_span
                bytes = node_bytes(ctx, node)
                while length(bytes) < target_span
                    insert!(bytes, 3, '0')
                end
                nb = write_and_reset(ctx, bytes)
                @assert nb == length(bytes) == target_span
                # Create new node and return it
                node′ = JuliaSyntax.GreenNode(JuliaSyntax.head(node), nb, ())
                return node′
            else
                # Do nothing: correctly formatted oct literal
            end
        else
            # Do nothing: BigInt oct literal
        end
    end

    # If the node is unchanged, just keep going.

    # Nodes that always recurse!
    if (
        node_kind === K"block" ||
        node_kind === K"braces" ||
        node_kind === K"bracescat" || # {a; b}
        node_kind === K"call" ||
        node_kind === K"cartesian_iterator" ||
        node_kind === K"char" ||
        node_kind === K"cmdstring" ||
        node_kind === K"comparison" ||
        node_kind === K"comprehension" ||
        node_kind === K"core_@cmd" ||
        node_kind === K"curly" ||
        node_kind === K"dotcall" ||
        node_kind === K"filter" ||
        node_kind === K"generator" ||
        node_kind === K"hcat" ||
        node_kind === K"importpath" ||
        node_kind === K"inert" ||
        node_kind === K"juxtapose" ||
        node_kind === K"macrocall" ||
        node_kind === K"ncat" ||
        node_kind === K"nrow" ||
        node_kind === K"parens" ||
        node_kind === K"ref" ||
        node_kind === K"row" ||
        node_kind === K"string" ||
        node_kind === K"toplevel" ||
        node_kind === K"typed_comprehension" ||
        node_kind === K"typed_hcat" ||
        node_kind === K"typed_ncat" ||
        node_kind === K"typed_vcat" ||
        node_kind === K"vcat" ||
        node_kind === K"vect"
    )
        @assert !JuliaSyntax.is_trivia(node)
        node′ = format_node_with_children!(ctx, node)
        return node′

        # Nodes that recurse! if not trivia
        elseif !JuliaSyntax.is_trivia(node) && (
           node_kind === K"abstract" ||
           node_kind === K"as" ||
           node_kind === K"break" ||
           node_kind === K"catch" ||
           node_kind === K"const" ||
           node_kind === K"continue" ||
           node_kind === K"do" ||
           node_kind === K"doc" ||
           node_kind === K"elseif" ||
           node_kind === K"export" ||
           node_kind === K"finally" ||
           node_kind === K"for" ||
           node_kind === K"function" ||
           node_kind === K"global" ||
           node_kind === K"if" ||
           node_kind === K"import" ||
           node_kind === K"let" ||
           node_kind === K"local" ||
           node_kind === K"macro" ||
           node_kind === K"module" ||
           node_kind === K"outer" ||
           node_kind === K"parameters" ||
           node_kind === K"primitive" ||
           node_kind === K"quote" ||
           node_kind === K"return" ||
           node_kind === K"struct" ||
           node_kind === K"try" ||
           node_kind === K"tuple" ||
           node_kind === K"using" ||
           node_kind === K"var" ||
           node_kind === K"where" ||
           node_kind === K"while"
        )
        node′ = format_node_with_children!(ctx, node)
        return node′

    # Nodes that should recurse if they have children (all??)
    elseif JuliaSyntax.haschildren(node) && (
        JuliaSyntax.is_operator(node) ||
        node_kind === K"else" # try-(catch|finally)-else
    )
        node′ = format_node_with_children!(ctx, node)
        return node′

    # Whitespace and comments emitted verbatim for now
    elseif node_kind === K"Whitespace" ||
           node_kind === K"NewlineWs" ||
           node_kind === K"Comment"
        accept_node!(ctx, node)
        return node

    # Nodes that always emit like the source code
    elseif (
        node_kind === K"(" ||
        node_kind === K")" ||
        node_kind === K"," ||
        node_kind === K"::" ||
        node_kind === K";" ||
        node_kind === K"<:" ||
        node_kind === K"@" ||
        node_kind === K"BinInt" ||
        node_kind === K"Char" ||
        node_kind === K"CmdMacroName" ||
        node_kind === K"CmdString" ||
        node_kind === K"Float" ||
        node_kind === K"Float32" ||
        node_kind === K"HexInt" ||
        node_kind === K"Identifier" ||
        node_kind === K"Integer" ||
        node_kind === K"MacroName" ||
        node_kind === K"OctInt" ||
        node_kind === K"String" ||
        node_kind === K"StringMacroName" ||
        node_kind === K"false" ||
        node_kind === K"true" ||
        node_kind === K"type" ||
        JuliaSyntax.is_operator(node) ||
        JuliaSyntax.is_trivia(node) && (
            node_kind === K"$" ||
            node_kind === K"=" ||
            node_kind === K"[" ||
            node_kind === K"\"" ||
            node_kind === K"\"\"\"" ||
            node_kind === K"]" ||
            node_kind === K"`" ||
            node_kind === K"```" ||
            node_kind === K"abstract" ||
            node_kind === K"as" ||
            node_kind === K"baremodule" ||
            node_kind === K"begin" ||
            node_kind === K"break" ||
            node_kind === K"catch" ||
            node_kind === K"const" ||
            node_kind === K"continue" ||
            node_kind === K"do" ||
            node_kind === K"else" ||
            node_kind === K"elseif" ||
            node_kind === K"end" ||
            node_kind === K"export" ||
            node_kind === K"finally" ||
            node_kind === K"for" ||
            node_kind === K"function" ||
            node_kind === K"global" ||
            node_kind === K"if" ||
            node_kind === K"import" ||
            node_kind === K"in" ||
            node_kind === K"let" ||
            node_kind === K"local" ||
            node_kind === K"macro" ||
            node_kind === K"module" ||
            node_kind === K"mutable" ||
            node_kind === K"outer" ||
            node_kind === K"primitive" ||
            node_kind === K"quote" ||
            node_kind === K"return" ||
            node_kind === K"struct" ||
            node_kind === K"try" ||
            node_kind === K"using" ||
            node_kind === K"var" ||
            node_kind === K"while" ||
            node_kind === K"{" ||
            node_kind === K"}"
        )
    )
        accept_node!(ctx, node)
        return node
    else
        msg = "unhandled node of type $(node_kind), current text:\n" * String(take!(ctx.fmt_io))
        throw(ErrorException(msg))
    end
end

# Entrypoint
function format_tree!(ctx::Context)
    root = ctx.src_tree
    # Write the root node to the output IO so that the formatter can read it if needed
    src_pos = position(ctx.src_io)
    @assert src_pos == 0
    fmt_pos = position(ctx.fmt_io)
    @assert fmt_pos == 0
    nb = write(ctx.fmt_io, read(ctx.src_io, JuliaSyntax.span(root)))
    @assert nb == JuliaSyntax.span(root)
    # Reset IOs so that the offsets are correct
    seek(ctx.src_io, src_pos)
    seek(ctx.fmt_io, fmt_pos)
    # Keep track of the depth to break out of infinite loops
    root′ = root
    itr = 0
    while true
        # Format the node.
        root′′ = format_node!(ctx, root′)
        if root′′ === nullnode
            # This signals that the node should be deleted, but that doesn't make sense for
            # the root node so error instead
            error("root node deleted")
        elseif root′′ === root′
            root′ = root′′
            @assert position(ctx.fmt_io) == fmt_pos + JuliaSyntax.span(root′)
            break
        else
            # The node was changed, reset the output stream and try again
            seek(ctx.fmt_io, fmt_pos)
            root′ = root′′
        end
        # The root node must only change once.
        if (itr += 1) == 2
            error("root node modified more than once")
        end
    end
    # Truncate the output at the root span
    truncate(ctx.fmt_io, JuliaSyntax.span(root′))
    # Set the final tree
    ctx.fmt_tree = root′
    return nothing
end

# function format_context(ctx)
#     # Build the context
#     ctx = Context(sourcetext)
#     # Run the formatter
#     fmt_tree = format_tree!(ctx)
#     ctx.fmt_tree = fmt_tree
#     return ctx
# end

"""
    format_string(str::AbstractString) -> String

Format a string.
"""
function format_string(str::AbstractString)
    ctx = Context(str)
    format_tree!(ctx)
    return String(take!(ctx.fmt_io))
end

"""
    format_file(inputfile::AbstractString, outputfile::AbstractString; inplace::Bool=false)

Format a file.
"""
function format_file(inputfile::AbstractString, outputfile::AbstractString = inputfile; inplace::Bool=false)
    # Argument handling
    inputfile = normpath(abspath(inputfile))
    outputfile = normpath(abspath(outputfile))
    str = read(inputfile, String)
    if !inplace && (outputfile == inputfile || (isfile(outputfile) && samefile(inputfile, outputfile)))
        error("input and output must not be the same when `inplace = false`")
    end
    # Format it
    ctx = Context(str)
    format_tree!(ctx)
    # Write the output but skip if it text didn't change
    changed = ctx.fmt_tree !== nothing
    if changed || !inplace
        write(outputfile, take!(ctx.fmt_io))
    end
    return
end

if isdefined(Base, Symbol("@main"))
    include("main.jl")
end

end # module

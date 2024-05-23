module Runic

using JuliaSyntax:
    JuliaSyntax, @K_str, @KSet_str

mutable struct Context
    const io::IO
    const src::String
    indent_level::Int
    offset::Int
    node::JuliaSyntax.GreenNode
    parent::Union{JuliaSyntax.GreenNode, Nothing}
end

function Context(src)::Union{Tuple{Nothing, Exception}, Tuple{Context, Nothing}}
    root = try
        JuliaSyntax.parseall(JuliaSyntax.GreenNode, src; ignore_warnings=true)
    catch e
        return nothing, e
    end
    return Context(IOBuffer(), src, 0, 0, root, nothing), nothing
end

# Emit the node like in the original source code.
function emit!(ctx::Context)::Union{Nothing, Exception}
    node = ctx.node
    # Should never emit nodes with children
    @assert !JuliaSyntax.haschildren(node)
    # First index of the current node, assumed to be valid
    i = ctx.offset + 1
    @assert isvalid(ctx.src, i)
    # Last index of the current node computed as the previous valid index from the first index of the next node
    j = prevind(ctx.src, ctx.offset + JuliaSyntax.span(node) + 1)
    @assert isvalid(ctx.src, j)
    # String representation of this node
    str = @view ctx.src[i:j]
    @debug "Emitting ..." JuliaSyntax.kind(node) str
    return emit!(ctx, str)
end

# Emit the node with a replacement string.
function emit!(ctx::Context, str::AbstractString)
    write(ctx.io, str)
    ctx.offset += JuliaSyntax.span(ctx.node)
    return
end

function recurse!(ctx)::Union{Nothing, Exception}
    # Stash the family
    grandparent = ctx.parent
    ctx.parent = ctx.node
    for child in JuliaSyntax.children(ctx.node)
        ctx.node = child
        if (err = format_context!(ctx); err !== nothing)
            return err
        end
    end
    # Restore the family
    ctx.node = ctx.parent
    ctx.parent = grandparent
    return nothing
end

function format_context!(ctx::Context)::Union{Nothing, Exception}
    node = ctx.node
    node_kind = JuliaSyntax.kind(node)

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
        @debug "Recursing always" node_kind
        @assert !JuliaSyntax.is_trivia(node)
        if (err = recurse!(ctx); err !== nothing)
            return err
        end

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
        @debug "Recursing if not trivia" node_kind
        if (err = recurse!(ctx); err !== nothing)
            return err
        end

    # Nodes that should recurse if they have children (all??)
    elseif JuliaSyntax.haschildren(node) && (
        JuliaSyntax.is_operator(node) ||
        node_kind === K"else" # try-(catch|finally)-else
    )
        @debug "Recursing because children" node_kind
        if (err = recurse!(ctx); err !== nothing)
            return err
        end

    # Whitespace and comments emitted verbatim for now
    elseif node_kind === K"Whitespace" ||
           node_kind === K"NewlineWs" ||
           node_kind === K"Comment"
        @debug "emit ws" node_kind
        if (err = emit!(ctx); err !== nothing)
            return err
        end

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
        @debug "Emitting raw" node_kind
        if (err = emit!(ctx); err !== nothing)
            return err
        end
    else
        return ErrorException("unhandled node of type $(JuliaSyntax.kind(ctx.node)), current text:\n" * String(take!(ctx.io)))
    end
    return nothing
end

function format_context(sourcetext)::Tuple{Any,Union{Nothing, Exception}}
    # Build the context
    ctx, err = Context(sourcetext)
    if err !== nothing
        return ctx, err
    end
    # Run the formatter
    err = format_context!(ctx)
    return ctx, err
end

"""
    format_string(sourcetext::AbstractString) -> String

Format a string.
"""
function format_string(sourcetext::AbstractString)
    # Format it!
    ctx, err = format_context(sourcetext)
    if err !== nothing
        throw(err)
    end
    # Return the string
    return String(take!(ctx.io))
end

"""
    format_file(inputfile::AbstractString, outputfile::AbstractString; inplace::Bool=false)

Format a file.
"""
function format_file(inputfile::AbstractString, outputfile::Union{AbstractString, Nothing} = nothing; inplace::Bool=false)
    # Argument handling
    sourcetext = read(inputfile, String)
    if outputfile === nothing && !inplace
        error("output file required when `inplace = false`")
    end
    if isfile(outputfile) && samefile(inputfile, outputfile) && !inplace
        error("must not use same input and output file when `inplace = false`")
    end
    # Format it
    ctx, err = format_context(sourcetext)
    if err !== nothing
        throw(err)
    end
    # Write the output on success
    write(outputfile, take!(ctx.io))
    return
end

if isdefined(Base, Symbol("@main"))
    include("main.jl")
end

end # module

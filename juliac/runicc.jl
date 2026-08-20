# Entry point for the compiled `runicc` executable. JuliaC.jl generates the C `main`
# function from this `@main` entry point.

using Runic: Runic

if Base.generating_output()
    # Workaround for dynamic dispatch in `JuliaSyntax.parse_block`: the `down` argument is
    # not specialized (it is only passed through, never called, so Julia's heuristics
    # don't specialize it) which makes the call to `parse_block_inner` dynamic and the
    # binary fail `--trim` verification (and crash at runtime with `--trim=unsafe`).
    # Overwrite the method with one that forces specialization. Only do this when
    # compiling (`Base.generating_output()`) to avoid method overwrite warnings when this
    # file is included in a regular session (e.g. in Runic's test suite).
    let JS = Runic.JuliaSyntax
        @eval function $(JS).parse_block(
                ps::$(JS).ParseState, down::F = $(JS).parse_eq,
                mark = $(JS).position(ps)
            ) where {F <: Function}
            $(JS).parse_block_inner(ps, down)
            return $(JS).emit(ps, mark, $(JS).Kind("block"))
        end
    end
end

function (@main)(argv::Vector{String})::Cint
    return Runic.main(argv)
end

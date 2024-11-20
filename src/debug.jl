# SPDX-License-Identifier: MIT

##############
# Debug info #
##############

abstract type RunicException <: Exception end

struct AssertionError <: RunicException
    msg::String
end

function Base.showerror(io::IO, err::AssertionError)
    print(
        io,
        "Runic.AssertionError: ", err.msg, ". This is unexpected, " *
            "please file an issue with a reproducible example at " *
            "https://github.com/fredrikekre/Runic.jl/issues/new."
    )
    return
end

macro assert(expr)
    msg = string(expr)
    return :($(esc(expr)) || throw(AssertionError($msg)))
end

@noinline unreachable() = throw(AssertionError("unreachable code reached"))

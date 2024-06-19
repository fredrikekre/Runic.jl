# SPDX-License-Identifier: MIT

using Runic:
    Runic, format_string
using Test:
    @test, @testset, @test_broken, @inferred
using JuliaSyntax:
    JuliaSyntax

@testset "Node" begin
    node = Runic.Node(JuliaSyntax.parseall(JuliaSyntax.GreenNode, "a = 1 + b\n"))

    # Pretty-printing
    @test sprint(show, node) == "Node({head: {kind: K\"toplevel\", flags: \"\"}, span: 10, tags: \"\"})"

    # JuliaSyntax duck-typing
    for n in (node, Runic.verified_kids(node)...)
        @test Runic.head(n) === JuliaSyntax.head(n) === n.head
        @test Runic.kind(n) === JuliaSyntax.kind(n) === n.head.kind
        @test Runic.flags(n) === JuliaSyntax.flags(n) === n.head.flags
        @test Runic.span(n) === JuliaSyntax.span(n) === n.span
    end
end

@testset "Chisels" begin
    # Type stability of verified_kids
    node = Runic.Node(JuliaSyntax.parseall(JuliaSyntax.GreenNode, "a = 1 + b\n"))
    @test typeof(@inferred Runic.verified_kids(node)) === Vector{Runic.Node}

    # replace_bytes!: insert larger
    io = IOBuffer(); write(io, "abc"); seek(io, 1)
    p = position(io)
    Runic.replace_bytes!(io, "xx", 1)
    @test p == position(io)
    @test read(io, String) == "xxc"
    seekstart(io)
    @test read(io, String) == "axxc"
    # replace_bytes!: insert smaller
    io = IOBuffer(); write(io, "abbc"); seek(io, 1)
    p = position(io)
    Runic.replace_bytes!(io, "x", 2)
    @test p == position(io)
    @test read(io, String) == "xc"
    seekstart(io)
    @test read(io, String) == "axc"
    # replace_bytes!: insert same
    io = IOBuffer(); write(io, "abc"); seek(io, 1)
    p = position(io)
    Runic.replace_bytes!(io, "x", 1)
    @test p == position(io)
    @test read(io, String) == "xc"
    seekstart(io)
    @test read(io, String) == "axc"
end

@testset "Trailing whitespace" begin
    io = IOBuffer()
    println(io, "a = 1  ") # Trailing space
    println(io, "b = 2\t") # Trailing tab
    println(io, "  ") # Trailing space on consecutive lines
    println(io, "  ")
    str = String(take!(io))
    @test format_string(str) == "a = 1\nb = 2\n\n\n"
end

@testset "Hex/oct/bin literal integers" begin
    z(n) = "0"^n
    test_cases = [
        # Hex UInt8
        ("0x" * z(n) * "1" => "0x01" for n in 0:1)...,
        # Hex  UInt16
        ("0x" * z(n) * "1" => "0x0001" for n in 2:3)...,
        # Hex  UInt32
        ("0x" * z(n) * "1" => "0x00000001" for n in 4:7)...,
        # Hex  UInt64
        ("0x" * z(n) * "1" => "0x0000000000000001" for n in 8:15)...,
        # Hex UInt128
        ("0x" * z(n) * "1" => "0x" * z(31) * "1" for n in 16:31)...,
        # Hex BigInt
        ("0x" * z(n) * "1" => "0x" * z(n) * "1" for n in 32:35)...,
        # Octal UInt8
        ("0o" * z(n) * "1" => "0o001" for n in 0:2)...,
        "0o377" => "0o377", # typemax(UInt8)
        # Octal UInt16
        "0o400" => "0o000400", # typemax(UInt8) + 1
        ("0o" * z(n) * "1" => "0o000001" for n in 3:5)...,
        "0o177777" => "0o177777", # typemax(UInt16)
        # Octal UInt32
        "0o200000" => "0o00000200000", # typemax(UInt16) + 1
        ("0o" * z(n) * "1" => "0o00000000001" for n in 6:10)...,
        "0o37777777777" => "0o37777777777", # typemax(UInt32)
        # Octal UInt64
        "0o40000000000" => "0o0000000000040000000000", # typemax(UInt32) + 1
        ("0o" * z(n) * "1" => "0o" * z(21) * "1" for n in 11:21)...,
        "0o1777777777777777777777" => "0o1777777777777777777777", # typemax(UInt64)
        # Octal UInt128
        "0o2" * z(21) => "0o" * z(21) * "2" * z(21), # typemax(UInt64) + 1
        ("0o" * z(n) * "1" => "0o" * z(42) * "1" for n in 22:42)...,
        "0o3" * "7"^42 => "0o3" * "7"^42, # typemax(UInt128)
        # Octal BigInt
        "0o4" * z(42) => "0o4" * z(42), # typemax(UInt128) + 1
        "0o7" * z(42) => "0o7" * z(42),
    ]
    mod = Module()
    for (a, b) in test_cases
        c = Core.eval(mod, Meta.parse(a))
        d = Core.eval(mod, Meta.parse(b))
        @test c == d
        @test typeof(c) == typeof(d)
        @test format_string(a) == b
    end
end

@testset "Floating point literals" begin
    test_cases = [
        ["1.0", "1.", "01.", "001.", "001.00", "1.00"] => "1.0",
        ["0.1", ".1", ".10", ".100", "00.100", "0.10"] => "0.1",
        ["1.1", "01.1", "1.10", "1.100", "001.100", "01.10"] => "1.1",
        ["1e3", "01e3", "01.e3", "1.e3", "1.000e3", "01.00e3"] => "1.0e3",
        ["1e+3", "01e+3", "01.e+3", "1.e+3", "1.000e+3", "01.00e+3"] => "1.0e+3",
        ["1e-3", "01e-3", "01.e-3", "1.e-3", "1.000e-3", "01.00e-3"] => "1.0e-3",
        ["1E3", "01E3", "01.E3", "1.E3", "1.000E3", "01.00E3"] => "1.0e3",
        ["1E+3", "01E+3", "01.E+3", "1.E+3", "1.000E+3", "01.00E+3"] => "1.0e+3",
        ["1E-3", "01E-3", "01.E-3", "1.E-3", "1.000E-3", "01.00E-3"] => "1.0e-3",
        ["1f3", "01f3", "01.f3", "1.f3", "1.000f3", "01.00f3"] => "1.0f3",
        ["1f+3", "01f+3", "01.f+3", "1.f+3", "1.000f+3", "01.00f+3"] => "1.0f+3",
        ["1f-3", "01f-3", "01.f-3", "1.f-3", "1.000f-3", "01.00f-3"] => "1.0f-3",
    ]
    mod = Module()
    for prefix in ("", "-", "+")
        for (as, b) in test_cases
            b = prefix * b
            for a in as
                a = prefix * a
                c = Core.eval(mod, Meta.parse(a))
                d = Core.eval(mod, Meta.parse(b))
                @test c == d
                @test typeof(c) == typeof(d)
                @test format_string(a) == b
            end
        end
    end
end

@testset "whitespace between operators" begin
    for sp in ("", " ", "  ")
        for op in ("+", "-", "==", "!=", "===", "!==", "<", "<=", ".+", ".==")
            # a op b
            @test format_string("$(sp)a$(sp)$(op)$(sp)b$(sp)") ==
                "$(sp)a $(op) b$(sp)"
            # x = a op b
            @test format_string("$(sp)x$(sp)=$(sp)a$(sp)$(op)$(sp)b$(sp)") ==
                "$(sp)x = a $(op) b$(sp)"
            # a op b op c
            @test format_string("$(sp)a$(sp)$(op)$(sp)b$(sp)$(op)$(sp)c$(sp)") ==
                "$(sp)a $(op) b $(op) c$(sp)"
            # a op b other_op c
            @test format_string("$(sp)a$(sp)$(op)$(sp)b$(sp)*$(sp)c$(sp)") ==
                "$(sp)a $(op) b * c$(sp)"
            # a op (b other_op c) (TODO: leading and trailing spaces should be removed in ()
            @test format_string("$(sp)a$(sp)$(op)$(sp)($(sp)b$(sp)*$(sp)c$(sp))$(sp)") ==
                "$(sp)a $(op) ($(sp)b * c$(sp))$(sp)"
            # call() op call()
            @test format_string("$(sp)sin(α)$(sp)$(op)$(sp)cos(β)$(sp)") ==
                "$(sp)sin(α) $(op) cos(β)$(sp)"
            # call() op call() op call()
            @test format_string("$(sp)sin(α)$(sp)$(op)$(sp)cos(β)$(sp)$(op)$(sp)tan(γ)$(sp)") ==
                "$(sp)sin(α) $(op) cos(β) $(op) tan(γ)$(sp)"
            # a op \n b
            @test format_string("$(sp)a$(sp)$(op)$(sp)\nb$(sp)") ==
                "$(sp)a $(op)\n    b$(sp)"
            # a op # comment \n b
            @test format_string("$(sp)a$(sp)$(op)$(sp)# comment\nb$(sp)") ==
                "$(sp)a $(op) # comment\n    b$(sp)"
        end
        # Exceptions to the rule: `:` and `^`
        # a:b
        @test format_string("$(sp)a$(sp):$(sp)b$(sp)") == "$(sp)a:b$(sp)"
        @test format_string("$(sp)(1 + 2)$(sp):$(sp)(1 + 3)$(sp)") ==
            "$(sp)(1 + 2):(1 + 3)$(sp)"
        # a:b:c
        @test format_string("$(sp)a$(sp):$(sp)b$(sp):$(sp)c$(sp)") == "$(sp)a:b:c$(sp)"
        @test format_string("$(sp)(1 + 2)$(sp):$(sp)(1 + 3)$(sp):$(sp)(1 + 4)$(sp)") ==
            "$(sp)(1 + 2):(1 + 3):(1 + 4)$(sp)"
        # a^b
        @test format_string("$(sp)a$(sp)^$(sp)b$(sp)") == "$(sp)a^b$(sp)"
        # Edgecase when formatting whitespace in the next leaf, when the next leaf is a
        # grand child or even younger. Note that this test depends a bit on where
        # JuliaSyntax.jl decides to place the K"Whitespace" node.
        @test format_string("$(sp)a$(sp)+$(sp)b$(sp)*$(sp)c$(sp)/$(sp)d$(sp)") ==
            "$(sp)a + b * c / d$(sp)"
        # Edgecase when using whitespace from the next leaf but the call chain continues
        # after with more children.
        @test format_string("$(sp)z$(sp)+$(sp)2x$(sp)+$(sp)z$(sp)") == "$(sp)z + 2x + z$(sp)"
        # Edgecase where the NewlineWs ends up inside the second call in a chain
        @test format_string("$(sp)a$(sp)\\$(sp)b$(sp)≈ $(sp)\n$(sp)c$(sp)\\$(sp)d$(sp)") ==
            "$(sp)a \\ b ≈\n    c \\ d$(sp)"
    end
end

@testset "spaces in lists" begin
    for sp in ("", " ", "  "), a in ("a", "a + a", "a(x)"), b in ("b", "b + b", "b(y)")
        # tuple, call, dotcall
        for f in ("", "f", "f.")
            # single line
            @test format_string("$(f)($(sp))") == "$(f)()"
            @test format_string("$(f)($(sp)$(a)$(sp),$(sp)$(b)$(sp))") ==
                format_string("$(f)($(sp)$(a)$(sp),$(sp)$(b)$(sp),$(sp))") ==
                "$(f)($(a), $(b))"
            # line break in between items
            @test format_string("$(f)($(sp)$(a)$(sp),\n$(sp)$(b)$(sp))") ==
                format_string("$(f)($(sp)$(a)$(sp),\n$(sp)$(b)$(sp),$(sp))") ==
                "$(f)($(a),\n    $(b))"
            # line break after opening token
            @test format_string("$(f)(\n$(sp)$(a)$(sp),$(sp)$(b)$(sp))") ==
                format_string("$(f)(\n$(sp)$(a)$(sp),$(sp)$(b)$(sp),)") ==
                "$(f)(\n    $(a), $(b))"
            # line break before closing token
            @test format_string("$(f)($(sp)$(a)$(sp),$(sp)$(b)\n)") ==
                format_string("$(f)($(sp)$(a)$(sp),$(sp)$(b),\n)") ==
                "$(f)($(a), $(b),\n)"
            # line break after opening and before closing token
            @test format_string("$(f)(\n$(sp)$(a)$(sp),$(sp)$(b)\n)") ==
                format_string("$(f)(\n$(sp)$(a)$(sp),$(sp)$(b),\n)") ==
                "$(f)(\n    $(a), $(b),\n)"
            # line break after opening and before closing token and between items
            @test format_string("$(f)(\n$(sp)$(a)$(sp),\n$(sp)$(b)\n)") ==
                format_string("$(f)(\n$(sp)$(a)$(sp),\n$(sp)$(b),\n)") ==
                "$(f)(\n    $(a),\n    $(b),\n)"
            # trailing comments
            @test format_string("$(f)($(sp)# x\n$(sp)$(a)$(sp),$(sp)# a\n$(sp)$(b)$(sp)# b\n)") ==
                format_string("$(f)($(sp)# x\n$(sp)$(a)$(sp),$(sp)# a\n$(sp)$(b),$(sp)# b\n)") ==
                "$(f)($(sp)# x\n    $(a),$(sp)# a\n    $(b),$(sp)# b\n)"
            # comments on separate lines between items
            @test format_string("$(f)(\n# a\n$(a)$(sp),\n# b\n$(b)\n)") ==
                format_string("$(f)(\n# a\n$(a)$(sp),\n# b\n$(b)$(sp),\n)") ==
                "$(f)(\n    # a\n    $(a),\n    # b\n    $(b),\n)"
        end
        # Single item
        @test format_string("($(sp)$(a)$(sp),$(sp))") == "($(a),)"
        @test format_string("f($(sp)$(a)$(sp),$(sp))") == "f($(a))"
        # Keyword arguments
        @test format_string("f($(sp)$(a)$(sp);$(sp)$(b)$(sp))") ==
            format_string("f($(sp)$(a)$(sp);$(sp)$(b)$(sp),$(sp))") ==
            "f($(a); $(b))"
        @test format_string("f(\n$(sp)$(a)$(sp);\n$(sp)$(b)$(sp)\n)") ==
            format_string("f(\n$(sp)$(a)$(sp);\n$(sp)$(b)$(sp),$(sp)\n)") ==
            "f(\n    $(a);\n    $(b),\n)"
    end
    # Splatting
    for sp in ("", " ", "  ")
        @test format_string("($(sp)a$(sp)...,$(sp))") == "(a$(sp)...,)"
        @test format_string("f($(sp)a$(sp)...,$(sp))") == "f(a$(sp)...)"
        @test format_string("f($(sp)a$(sp)...;$(sp)b$(sp)...$(sp))") == "f(a$(sp)...; b$(sp)...)"
    end
    # Curly (not as extensive testing as tuple/call/dotcall above but the code path is the
    # same)
    for x in ("", "X"), sp in ("", " ", "  "), a in ("A", "<:B", "C <: D"), b in ("E", "<:F", "G <: H")
        tr = x == "" ? "" : ","
        @test format_string("$(x){$(sp)$(a)$(sp),$(sp)$(b)$(sp)}") == "$(x){$(a), $(b)}"
        @test format_string("$(x){$(sp)$(a)$(sp);$(sp)$(b)$(sp)}") == "$(x){$(a); $(b)}"
        @test format_string("$(x){$(sp)$(a)$(sp);$(sp)$(b)$(sp)}") == "$(x){$(a); $(b)}"
        @test format_string("$(x){\n$(sp)$(a)$(sp);$(sp)$(b)$(sp)\n}") == "$(x){\n$(a); $(b)$(tr)\n}"
    end
end

@testset "whitespace around ->" begin
    for sp in ("", " ", "  ")
        @test format_string("a$(sp)->$(sp)b") == "a -> b"
    end
end

@testset "whitespace in comparison chains" begin
    for sp in ("", " ", "  ")
        @test format_string("a$(sp)==$(sp)b") == "a == b"
        @test format_string("a$(sp)==$(sp)b$(sp)==$(sp)c") == "a == b == c"
        @test format_string("a$(sp)<=$(sp)b$(sp)==$(sp)c") == "a <= b == c"
        @test format_string("a$(sp)<=$(sp)b$(sp)>=$(sp)c") == "a <= b >= c"
        @test format_string("a$(sp)<$(sp)b$(sp)>=$(sp)c") == "a < b >= c"
        @test format_string("a$(sp)<$(sp)b$(sp)<$(sp)c") == "a < b < c"
        # Dotted chains
        @test format_string("a$(sp).<=$(sp)b$(sp).>=$(sp)c") == "a .<= b .>= c"
        @test format_string("a$(sp).<$(sp)b$(sp).<$(sp)c") == "a .< b .< c"
    end
end

@testset "whitespace around assignments" begin
    # Regular assignments and dot-assignments
    for a in ("=", "+=", "-=", ".=", ".+=", ".-=")
        @test format_string("a$(a)b") == "a $(a) b"
        @test format_string("a $(a)b") == "a $(a) b"
        @test format_string("a$(a) b") == "a $(a) b"
        @test format_string("  a$(a) b") == "  a $(a) b"
        @test format_string("  a$(a) b  ") == "  a $(a) b  "
        @test format_string("a$(a)   b") == "a $(a) b"
        @test format_string("a$(a)   b  *  x") == "a $(a) b * x"
        @test format_string("a$(a)( b *  x)") == "a $(a) ( b * x)"
    end
    # Chained assignments
    @test format_string("x=a= b  ") == "x = a = b  "
    @test format_string("a=   b = x") == "a = b = x"
    # Check the common footgun of permuting the operator and =
    @test format_string("a =+ c") == "a = + c"
    # Short form function definitions
    @test format_string("sin(π)=cos(pi)") == "sin(π) = cos(pi)"
    # For loop nodes are assignment, even when using `in` and `∈`
    for op in ("in", "=", "∈"), sp in ("", " ", "  ")
        op == "in" && sp == "" && continue
        @test format_string("for i$(sp)$(op)$(sp)1:10\nend\n") == "for i in 1:10\nend\n"
    end
    # Quoted assignment operators
    @test format_string(":(=)") == ":(=)"
    @test format_string(":(+=)") == ":(+=)"
end

@testset "whitespace around <: and >:, no whitespace around ::" begin
    # K"::" with both LHS and RHS
    @test format_string("a::T") == "a::T"
    @test format_string("a::T::S") == "a::T::S"
    @test format_string("a  ::  T") == "a::T"
    # K"::" with just RHS
    @test format_string("f(::T)::T = 1") == "f(::T)::T = 1"
    @test format_string("f(:: T) :: T = 1") == "f(::T)::T = 1"
    # K"<:" and K">:" with both LHS and RHS
    @test format_string("a<:T") == "a <: T"
    @test format_string("a>:T") == "a >: T"
    @test format_string("a  <:   T") == "a <: T"
    @test format_string("a  >:   T") == "a >: T"
    # K"<:" and K">:" with just RHS
    @test format_string("V{<:T}") == "V{<:T}"
    @test format_string("V{<: T}") == "V{<:T}"
    @test format_string("V{>:T}") == "V{>:T}"
    @test format_string("V{>: T}") == "V{>:T}"
    # K"comparison" for chains
    @test format_string("a<:T<:S") == "a <: T <: S"
    @test format_string("a>:T>:S") == "a >: T >: S"
    @test format_string("a <:  T   <:    S") == "a <: T <: S"
    @test format_string("a >:  T   >:    S") == "a >: T >: S"
end

@testset "replace ∈ and = with in in for loops and generators" begin
    for sp in ("", " ", "  "), op in ("∈", "=", "in")
        op == "in" && sp == "" && continue
        # for loops
        @test format_string("for i$(sp)$(op)$(sp)I\nend") == "for i in I\nend"
        @test format_string("for i$(sp)$(op)$(sp)I, j$(sp)$(op)$(sp)J\nend") ==
            "for i in I, j in J\nend"
        @test format_string("for i$(sp)$(op)$(sp)I, j$(sp)$(op)$(sp)J, k$(sp)$(op)$(sp)K\nend") ==
            "for i in I, j in J, k in K\nend"
        # for generators
        for (l, r) in (("[", "]"), ("(", ")"))
            @test format_string("$(l)i for i$(sp)$(op)$(sp)I$(r)") == "$(l)i for i in I$(r)"
            @test format_string("$(l)(i, j) for i$(sp)$(op)$(sp)I, j$(sp)$(op)$(sp)J$(r)") ==
                "$(l)(i, j) for i in I, j in J$(r)"
            @test format_string("$(l)(i, j, k) for i$(sp)$(op)$(sp)I, j$(sp)$(op)$(sp)J, k$(sp)$(op)$(sp)K$(r)") ==
                "$(l)(i, j, k) for i in I, j in J, k in K$(r)"
        end
    end
end

@testset "block/hard indentation" begin
    for sp in ("", "  ", "    ", "      ")
        # function-end
        @test format_string("function f()\n$(sp)x\n$(sp)end") ==
            "function f()\n    x\nend"
        @test format_string("function f end") == "function f end"
        # macro-end
        @test format_string("macro f()\n$(sp)x\n$(sp)end") ==
            "macro f()\n    x\nend"
        # let-end
        @test format_string("let a = 1\n$(sp)x\n$(sp)end") ==
            "let a = 1\n    x\nend"
        # begin-end
        @test format_string("begin\n$(sp)x\n$(sp)end") ==
            "begin\n    x\nend"
        # quote-end
        @test format_string("quote\n$(sp)x\n$(sp)end") ==
            "quote\n    x\nend"
        # if-end
        @test format_string("if a\n$(sp)x\n$(sp)end") ==
            "if a\n    x\nend"
        # if-else-end
        @test format_string("if a\n$(sp)x\n$(sp)else\n$(sp)y\n$(sp)end") ==
            "if a\n    x\nelse\n    y\nend"
        # if-elseif-end
        @test format_string("if a\n$(sp)x\n$(sp)elseif b\n$(sp)y\n$(sp)end") ==
            "if a\n    x\nelseif b\n    y\nend"
        # if-elseif-elseif-end
        @test format_string(
            "if a\n$(sp)x\n$(sp)elseif b\n$(sp)y\n$(sp)elseif c\n$(sp)z\n$(sp)end",
        ) == "if a\n    x\nelseif b\n    y\nelseif c\n    z\nend"
        # if-elseif-else-end
        @test format_string(
            "if a\n$(sp)x\n$(sp)elseif b\n$(sp)y\n$(sp)else\n$(sp)z\n$(sp)end",
        ) == "if a\n    x\nelseif b\n    y\nelse\n    z\nend"
        # if-elseif-elseif-else-end
        @test format_string(
            "if a\n$(sp)x\n$(sp)elseif b\n$(sp)y\n$(sp)elseif " *
            "c\n$(sp)z\n$(sp)else\n$(sp)u\n$(sp)end",
        ) ==
            "if a\n    x\nelseif b\n    y\nelseif c\n    z\nelse\n    u\nend"
        # begin-end
        @test format_string("begin\n$(sp)x\n$(sp)end") == "begin\n    x\nend"
        # (mutable) struct
        for mut in ("", "mutable ")
            @test format_string("$(mut)struct A\n$(sp)x\n$(sp)end") ==
                "$(mut)struct A\n    x\nend"
        end
        # for-end
        @test format_string("for i in I\n$(sp)x\n$(sp)end") == "for i in I\n    x\nend"
        @test format_string("for i in I, j in J\n$(sp)x\n$(sp)end") == "for i in I, j in J\n    x\nend"
        # while-end
        @test format_string("while x\n$(sp)y\n$(sp)end") == "while x\n    y\nend"
        # try-catch-end
        @test format_string("try\n$(sp)x\n$(sp)catch\n$(sp)y\n$(sp)end") ==
            "try\n    x\ncatch\n    y\nend"
        # try-catch(err)-end
        @test format_string("try\n$(sp)x\n$(sp)catch err\n$(sp)y\n$(sp)end") ==
            "try\n    x\ncatch err\n    y\nend"
        # try-catch-finally-end
        @test format_string(
            "try\n$(sp)x\n$(sp)catch\n$(sp)y\n$(sp)finally\n$(sp)z\n$(sp)end",
        ) == "try\n    x\ncatch\n    y\nfinally\n    z\nend"
        # try-catch(err)-finally-end
        @test format_string(
            "try\n$(sp)x\n$(sp)catch err\n$(sp)y\n$(sp)finally\n$(sp)z\n$(sp)end",
        ) == "try\n    x\ncatch err\n    y\nfinally\n    z\nend"
        # try-finally-catch-end (yes, this is allowed...)
        @test format_string(
            "try\n$(sp)x\n$(sp)finally\n$(sp)y\n$(sp)catch\n$(sp)z\n$(sp)end",
        ) == "try\n    x\nfinally\n    y\ncatch\n    z\nend"
        # try-finally-catch(err)-end
        @test format_string(
            "try\n$(sp)x\n$(sp)finally\n$(sp)y\n$(sp)catch err\n$(sp)z\n$(sp)end",
        ) == "try\n    x\nfinally\n    y\ncatch err\n    z\nend"
        if VERSION >= v"1.8"
            # try-catch-else-end
            @test format_string(
                "try\n$(sp)x\n$(sp)catch\n$(sp)y\n$(sp)else\n$(sp)z\n$(sp)end",
            ) == "try\n    x\ncatch\n    y\nelse\n    z\nend"
            # try-catch(err)-else-end
            @test format_string(
                "try\n$(sp)x\n$(sp)catch err\n$(sp)y\n$(sp)else\n$(sp)z\n$(sp)end",
            ) == "try\n    x\ncatch err\n    y\nelse\n    z\nend"
            # try-catch-else-finally-end
            @test format_string(
                "try\n$(sp)x\n$(sp)catch\n$(sp)y\n$(sp)else\n$(sp)z\n$(sp)finally\n$(sp)z\n$(sp)end",
            ) == "try\n    x\ncatch\n    y\nelse\n    z\nfinally\n    z\nend"
            # try-catch(err)-else-finally-end
            @test format_string(
                "try\n$(sp)x\n$(sp)catch err\n$(sp)y\n$(sp)else\n$(sp)z\n$(sp)finally\n$(sp)z\n$(sp)end",
            ) == "try\n    x\ncatch err\n    y\nelse\n    z\nfinally\n    z\nend"
        end
        # do-end
        @test format_string("open() do\n$(sp)a\n$(sp)end") == "open() do\n    a\nend"
        @test format_string("open() do io\n$(sp)a\n$(sp)end") == "open() do io\n    a\nend"
        # module-end, baremodule-end
        for b in ("", "bare")
            # Just a module
            @test format_string("$(b)module A\n$(sp)x\n$(sp)end") == "$(b)module A\nx\nend"
            # Comment before
            @test format_string("# c\n$(b)module A\n$(sp)x\n$(sp)end") ==
                "# c\n$(b)module A\nx\nend"
            # Docstring before
            @test format_string("\"doc\"\n$(b)module A\n$(sp)x\n$(sp)end") ==
                "\"doc\"\n$(b)module A\nx\nend"
            # code before
            @test format_string("f\n$(b)module A\n$(sp)x\n$(sp)end") ==
                "f\n$(b)module A\n    x\nend"
            @test format_string("f\n$(b)module A\n$(sp)x\n$(sp)end\n$(b)module B\n$(sp)x\n$(sp)end") ==
                "f\n$(b)module A\n    x\nend\n$(b)module B\n    x\nend"
            # code after
            @test format_string("$(b)module A\n$(sp)x\n$(sp)end\nf") ==
                "$(b)module A\n    x\nend\nf"
            # nested modules
            @test format_string("$(b)module A\n$(sp)$(b)module B\n$(sp)x\n$(sp)end\n$(sp)end") ==
                "$(b)module A\n$(b)module B\n    x\nend\nend"
            # nested documented modules
            @test format_string("\"doc\"\n$(b)module A\n\"doc\"\n$(b)module B\n$(sp)x\n$(sp)end\n$(sp)end") ==
                "\"doc\"\n$(b)module A\n\"doc\"\n$(b)module B\n    x\nend\nend"
        end
    end
end

@testset "continuation/soft indentation" begin
    for sp in ("", "  ", "    ", "      ")
        # tuple
        @test format_string("(a,\n$(sp)b)") == "(a,\n    b)"
        @test format_string("(a,\n$(sp)b\n$(sp))") ==
            format_string("(a,\n$(sp)b,\n$(sp))") == "(a,\n    b,\n)"
        @test format_string("(\n$(sp)a,\n$(sp)b,\n$(sp))") == "(\n    a,\n    b,\n)"
        # call, dotcall
        for sep in (",", ";"), d in ("", ".")
            @test format_string("f$(d)(a$(sep)\n$(sp)b)") == "f$(d)(a$(sep)\n    b)"
            @test format_string("f$(d)(a$(sep)\n$(sp)b\n$(sp))") ==
                format_string("f$(d)(a$(sep)\n$(sp)b,\n$(sp))") ==
                "f$(d)(a$(sep)\n    b,\n)"
            @test format_string("f$(d)(\n$(sp)a$(sep)\n$(sp)b,\n$(sp))") ==
                "f$(d)(\n    a$(sep)\n    b,\n)"
        end
        # op-call, dot-op-call
        for d in ("", ".")
            @test format_string("a $(d)+\n$(sp)b") == "a $(d)+\n    b"
            @test format_string("a $(d)+ b $(d)*\n$(sp)c") == "a $(d)+ b $(d)*\n    c"
            @test format_string("a $(d)+\n$(sp)b $(d)*\n$(sp)c") == "a $(d)+\n    b $(d)*\n    c"
            if !(VERSION < v"1.7" && d == ".")
                @test format_string("a $(d)||\n$(sp)b") == "a $(d)||\n    b"
            end
        end
        # assignment
        for op in ("=", "+=")
            @test format_string("a $(op)\n$(sp)b") == "a $(op)\n    b"
        end
        # using/import
        for verb in ("using", "import")
            @test format_string("$(verb) A,\n$(sp)B") == "$(verb) A,\n    B"
            @test format_string("$(verb) A: a,\n$(sp)b") == "$(verb) A: a,\n    b"
            @test format_string("$(verb) A:\n$(sp)a,\n$(sp)b") == "$(verb) A:\n    a,\n    b"
        end
        # export
        @test format_string("export a,\n$(sp)b") == "export a,\n    b"
        @test format_string("export\n$(sp)a,\n$(sp)b") == "export\n    a,\n    b"
        # ternary
        @test format_string("a ?\n$(sp)b : c") == "a ?\n    b : c"
        @test format_string("a ? b :\n$(sp)c") == "a ? b :\n    c"
        @test format_string("a ?\n$(sp)b :\n$(sp)c") == "a ?\n    b :\n    c"
        @test format_string("a ?\n$(sp)b :\n$(sp)c ?\n$(sp)d : e") ==
            "a ?\n    b :\n    c ?\n    d : e"
        # paren-quote
        @test format_string(":(a,\n$(sp)b)") == ":(a,\n    b)"
        @test format_string(":(a,\n$(sp)b)") == ":(a,\n    b)"
        @test format_string(":(a;\n$(sp)b)") == ":(a;\n    b)"
        # paren-block
        @test format_string("(a;\n$(sp)b)") == "(a;\n    b)"
        # array literals
        for t in ("", "T")
            @test format_string("$(t)[a,\n$(sp)b]") == "$(t)[a,\n    b]"
            @test format_string("$(t)[\n$(sp)a,\n$(sp)b\n$(sp)]") == "$(t)[\n    a,\n    b\n]"
            @test format_string("$(t)[a b\n$(sp)c d]") == "$(t)[a b\n    c d]"
            @test format_string("$(t)[\n$(sp)a b\n$(sp)c d\n$(sp)]") == "$(t)[\n    a b\n    c d\n]"
        end
        # comparison
        @test format_string("a == b ==\n$(sp)c") == "a == b ==\n    c"
        @test format_string("a <= b >=\n$(sp)c") == "a <= b >=\n    c"
        # curly braces
        @test format_string("{a,\n$(sp)b}") == "{a,\n    b}"
        @test format_string("{a,\n$(sp)b\n$(sp)}") ==
            format_string("{a,\n$(sp)b,\n$(sp)}") == "{a,\n    b,\n}"
        @test format_string("{\n$(sp)a,\n$(sp)b,\n$(sp)}") == "{\n    a,\n    b,\n}"
    end
end

# SPDX-License-Identifier: MIT

using Runic:
    format_string
using Test:
    @test, @testset, @test_broken

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
        ("0x" * z(n) * "1" => "0x01" for n in  0:1)...,
        # Hex  UInt16
        ("0x" * z(n) * "1" => "0x0001" for n in  2:3)...,
        # Hex  UInt32
        ("0x" * z(n) * "1" => "0x00000001" for n in  4:7)...,
        # Hex  UInt64
        ("0x" * z(n) * "1" => "0x0000000000000001" for n in  8:15)...,
        # Hex UInt128
        ("0x" * z(n) * "1" => "0x" * z(31) * "1" for n in 16:31)...,
        # Hex BigInt
        ("0x" * z(n) * "1" => "0x" * z(n) * "1" for n in 32:35)...,
        # Octal UInt8
        ("0o" * z(n) * "1" => "0o001" for n in 0:2)...,
        "0o377" => "0o377", # typemax(UInt8)
        # Octal UInt16
        "0o400"    => "0o000400", # typemax(UInt8) + 1
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
    for (as, b) in test_cases
        for a in as
            c = Core.eval(mod, Meta.parse(a))
            d = Core.eval(mod, Meta.parse(b))
            @test c == d
            @test typeof(c) == typeof(d)
            @test format_string(a) == b
        end
    end
end

@testset "whitespace between operators" begin
    for op in ("+", "-", "==", "!=", "===", "!==", "<", "<=")
        @test format_string("a$(op)b") == "a $(op) b"
        @test format_string("a $(op)b") == "a $(op) b"
        @test format_string("a$(op) b") == "a $(op) b"
        @test format_string("  a$(op) b") == "  a $(op) b"
        @test format_string("  a$(op) b  ") == "  a $(op) b  "
        @test format_string("x=a$(op) b  ") == "x = a $(op) b  "
        @test format_string("a$(op)   b") == "a $(op) b"
        @test format_string("a$(op)   b $(op) x") == "a $(op) b $(op) x"
        @test format_string("a$(op)   b  *  x") == "a $(op) b * x"
        @test format_string("a$(op)( b *  x)") == "a $(op) ( b * x)"
        @test format_string("sin(π)$(op)cos(pi)") == "sin(π) $(op) cos(pi)"
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
end

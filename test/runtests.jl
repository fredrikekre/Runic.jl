using Runic:
    format_string
using Test:
    @test, @testset

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
    # Test the test cases :)
    mod = Module()
    for (a, b) in test_cases
        c = Core.eval(mod, Meta.parse(a))
        d = Core.eval(mod, Meta.parse(b))
        @test c == d
        @test typeof(c) == typeof(d)
    end
    # Join all cases to a single string so that we only need to call the formatter once
    input_str = let io = IOBuffer()
        join(io, (case.first for case in test_cases), '\n')
        String(take!(io))
    end
    output_str = let io = IOBuffer()
        join(io, (case.second for case in test_cases), '\n')
        String(take!(io))
    end
    @test format_string(input_str) == output_str
end
